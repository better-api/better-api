//! Defines semantic representation of values.
//!
//! The main data structure is a [`ValueArena`] that holds
//! the values. Values in the arena are referenced with [`ValueId`].
//!
//! ## Building An Arena
//!
//! [Primitive values](`PrimitiveValue`) can be added
//! to the arena directly. For composite values (arrays and objects) you need
//! to use the builder API - [`ArrayBuilder`] and [`ObjectBuilder`].
//! Builder APIs also support building a nested composite values.
//!
//! Once you add a value to the arena, you get a [`ValueId`] which can
//! later be used to retrieve the value.
//!
//! ## Getting Values
//!
//! To retrieve the value, you pass [`ValueId`] to [`ValueArena::get`] method.
//! This will give you a [`Value`]. Primitive values are represented directly
//! with Rust types.
//!
//! Composite types are represented with [`Array`] and [`Object`]. These two types
//! allow you to iterate through the values inside the composite type.

use crate::string::StringId;

/// Representation of a value.
#[derive(Debug, derive_more::Display, Clone)]
pub enum Value<'a> {
    #[display("`null`")]
    Null,
    #[display("string")]
    String(StringId),
    #[display("bool")]
    Bool(bool),
    #[display("integer")]
    Integer(i128), // i128 is large enough for i64 and u64
    #[display("float")]
    Float(f64),
    #[display("object")]
    Object(Object<'a>),
    #[display("array")]
    Array(Array<'a>),
}

impl<'a> From<PrimitiveValue> for Value<'a> {
    fn from(value: PrimitiveValue) -> Self {
        match value {
            PrimitiveValue::Null => Value::Null,
            PrimitiveValue::String(s) => Value::String(s),
            PrimitiveValue::Bool(b) => Value::Bool(b),
            PrimitiveValue::Integer(i) => Value::Integer(i),
            PrimitiveValue::Float(f) => Value::Float(f),
        }
    }
}

impl<'a> Value<'a> {
    fn from_slot(arena: &'a ValueArena, id: ValueId, slot: &'a Slot) -> Self {
        match slot {
            Slot::Primitive(val) => (*val).into(),
            Slot::Object { end } => Value::Object(Object {
                fields: ObjectFields {
                    arena,
                    current: ValueId(id.0 + 1),
                    end: *end,
                    object_id: id,
                },
                id,
            }),
            Slot::Array { end } => Value::Array(Array {
                items: ArrayItems {
                    arena,
                    current: ValueId(id.0 + 1),
                    end: *end,
                },
                id,
            }),
            Slot::ObjectField(_) => {
                unreachable!("invalid conversion of Slot::ObjectField to Value")
            }
        }
    }
}

/// A field inside of the object.
#[derive(Debug, Clone)]
pub struct ObjectField<'a> {
    pub id: ObjectFieldId,
    pub name: StringId,
    pub value: Value<'a>,
}

/// Object value returned by the [`ValueArena`].
#[derive(Debug, Clone)]
pub struct Object<'a> {
    // Id of the object
    pub id: ValueId,

    /// Iterator over fields.
    fields: ObjectFields<'a>,
}

impl<'a> Object<'a> {
    /// Returns iterator over [object fields](ObjectField).
    pub fn fields(&self) -> ObjectFields<'a> {
        self.fields.clone()
    }
}

/// Iterator over object fields.
#[derive(derive_more::Debug, Clone)]
pub struct ObjectFields<'a> {
    #[debug(skip)]
    arena: &'a ValueArena,
    current: ValueId,
    end: ValueId,

    object_id: ValueId,
}

impl<'a> Iterator for ObjectFields<'a> {
    type Item = ObjectField<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 >= self.end.0 {
            return None;
        }

        let field_id = ObjectFieldId {
            object_id: self.object_id,
            slot_idx: self.current.0,
        };
        let field = self.arena.get_field(field_id);

        self.current = match field.value {
            Value::Object(Object { ref fields, .. }) => fields.end,
            Value::Array(Array { ref items, .. }) => items.end,
            _ => ValueId(self.current.0 + 2),
        };

        Some(field)
    }
}

/// Item returned by an [`Array`] iterator.
#[derive(Debug)]
pub struct ArrayItem<'a> {
    /// Id of the item, used for [`ValueArena::get`]
    pub id: ValueId,

    /// Value of the item
    pub value: Value<'a>,
}

/// Array value returned by the [`ValueArena`].
#[derive(Debug, Clone)]
pub struct Array<'a> {
    // Id of the array
    pub id: ValueId,

    items: ArrayItems<'a>,
}

impl<'a> Array<'a> {
    /// Returns iterator over [array items](ArrayItem)
    pub fn items(&self) -> ArrayItems<'a> {
        self.items.clone()
    }
}

/// Iterator over array items.
#[derive(derive_more::Debug, Clone)]
pub struct ArrayItems<'a> {
    #[debug(skip)]
    arena: &'a ValueArena,
    current: ValueId,
    end: ValueId,
}

impl<'a> Iterator for ArrayItems<'a> {
    type Item = ArrayItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 >= self.end.0 {
            return None;
        }

        let id = self.current;
        let item = &self.arena.data[id.0 as usize];
        let value = Value::from_slot(self.arena, id, item);

        self.current = match value {
            Value::Object(Object { ref fields, .. }) => fields.end,
            Value::Array(Array { ref items, .. }) => items.end,
            _ => ValueId(self.current.0 + 1),
        };

        Some(ArrayItem { id, value })
    }
}

/// Non-composite value
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveValue {
    Null,
    String(StringId),
    Bool(bool),
    Integer(i128),
    Float(f64),
}

/// Helper type for adding array to arena.
///
/// This type is constructed with [`ValueArena::start_array`] method.
/// After you are done, you should call [`finish`](ArrayBuilder::finish), which returns
/// the id of the final array in the arena.
///
/// If builder is dropped before calling finish, added values are removed from the
/// arena.
pub struct ArrayBuilder<'p> {
    data: &'p mut Vec<Slot>,

    /// Optionally clean up the data if array is not finished successfully.
    truncate: Option<u32>,

    /// Index in the arena that contains Slot::Array of this array.
    start: ValueId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'p> ArrayBuilder<'p> {
    fn new(data: &'p mut Vec<Slot>, truncate: Option<u32>) -> Self {
        let idx = data.len();
        data.push(Slot::Array { end: ValueId(0) });

        Self {
            data,
            truncate,
            start: ValueId(idx as u32),
            finished: false,
        }
    }

    /// Add a new primitive value to the array.
    pub fn add_primitive(&mut self, value: PrimitiveValue) -> ValueId {
        let idx = self.data.len();

        self.data.push(value.into());

        ValueId(idx as u32)
    }

    /// Insert an array as the next element of this array.
    pub fn start_array<'a>(&'a mut self) -> ArrayBuilder<'a> {
        ArrayBuilder::new(self.data, None)
    }

    /// Insert an object as the next element of this array.
    pub fn start_object<'a>(&'a mut self) -> ObjectBuilder<'a> {
        ObjectBuilder::new(self.data, None)
    }

    /// Constructs the final array.
    ///
    /// Returns the id of the final array in the arena.
    pub fn finish(mut self) -> ValueId {
        self.finished = true;

        let idx = self.data.len();

        let start = self.start.0 as usize;
        let head = &mut self.data[start];
        match head {
            Slot::Array { end } => *end = ValueId(idx as u32),
            _ => unreachable!("invalid ArrayBuilder start"),
        }

        self.start
    }
}

impl<'p> Drop for ArrayBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        // Truncate the builder to `truncate` parameter, or self start.
        let len = self.truncate.unwrap_or(self.start.0);
        self.data.truncate(len as usize);
    }
}

/// Helper type for adding object to arena.
///
/// This type is constructed with [`ValueArena::start_object`] method.
/// After you are done, you should call [`finish`](ObjectBuilder::finish), which returns
/// the id of the final object in the arena.
///
/// If builder is dropped before calling finish, added fields and values are removed from the
/// arena.
pub struct ObjectBuilder<'p> {
    data: &'p mut Vec<Slot>,

    /// Optionally clean up the data if array is not finished successfully.
    truncate: Option<u32>,

    /// Index in the arena that contains Slot::Object of this object.
    start: ValueId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'p> ObjectBuilder<'p> {
    fn new(data: &'p mut Vec<Slot>, truncate: Option<u32>) -> Self {
        let idx = data.len();
        data.push(Slot::Object { end: ValueId(0) });

        Self {
            data,
            truncate,
            start: ValueId(idx as u32),
            finished: false,
        }
    }

    /// Add a new field to the object with primitive value.
    pub fn add_primitive(&mut self, name: StringId, value: PrimitiveValue) -> ObjectFieldId {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::ObjectField(name));
        self.data.push(value.into());

        ObjectFieldId {
            object_id: self.start,
            slot_idx,
        }
    }

    /// Add a new field to the object with object value.
    pub fn start_object<'a>(&'a mut self, name: StringId) -> (ObjectBuilder<'a>, ObjectFieldId) {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::ObjectField(name));

        let field_id = ObjectFieldId {
            object_id: self.start,
            slot_idx,
        };
        let builder = ObjectBuilder::new(self.data, Some(slot_idx));

        (builder, field_id)
    }

    /// Add a new field to the object with array value.
    pub fn start_array<'a>(&'a mut self, name: StringId) -> (ArrayBuilder<'a>, ObjectFieldId) {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::ObjectField(name));

        let field_id = ObjectFieldId {
            object_id: self.start,
            slot_idx,
        };
        let builder = ArrayBuilder::new(self.data, Some(slot_idx));

        (builder, field_id)
    }

    /// Constructs the final object.
    ///
    /// Returns the id of the final object in the arena.
    pub fn finish(mut self) -> ValueId {
        self.finished = true;

        let idx = self.data.len();

        let start = self.start.0 as usize;
        let head = &mut self.data[start];
        match head {
            Slot::Object { end } => *end = ValueId(idx as u32),
            _ => unreachable!("invalid ObjectBuilder start"),
        }

        self.start
    }
}

impl<'p> Drop for ObjectBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        // Truncate the builder to `truncate` parameter, or self start.
        let len = self.truncate.unwrap_or(self.start.0);
        self.data.truncate(len as usize);
    }
}

/// Id of the value in the [`ValueArena`] .
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ValueId(u32);

/// Id of a object field.
///
/// Used for getting specific [`ObjectField`] with [`ValueArena::get_field`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ObjectFieldId {
    object_id: ValueId,

    /// Index of the slot in the arena
    slot_idx: u32,
}

impl ObjectFieldId {
    /// Get [`ValueId`] of the object that contains this field.
    pub fn object_id(&self) -> ValueId {
        self.object_id
    }

    /// Get [`ValueId`] of the field's value.
    pub fn value_id(&self) -> ValueId {
        ValueId(self.slot_idx + 1)
    }
}

/// Slot in the arena
#[derive(Debug, Clone, PartialEq)]
enum Slot {
    Primitive(PrimitiveValue),
    Array {
        // Id after the last element in the array.
        // Used for skipping the whole array during iteration.
        end: ValueId,
    },
    Object {
        // Id after the last field in the object.
        // Used for skipping the whole object during iteration.
        end: ValueId,
    },
    // Name of the field in the object.
    // In TrackedSlot syntax pointer points to the whole field
    // and not just the name.
    ObjectField(StringId),
}

impl From<PrimitiveValue> for Slot {
    fn from(value: PrimitiveValue) -> Self {
        Self::Primitive(value)
    }
}

/// Arena that holds the values.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ValueArena {
    data: Vec<Slot>,
}

impl ValueArena {
    /// Create a new value arena.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get [`Value`] by id.
    pub fn get<'a>(&'a self, id: ValueId) -> Value<'a> {
        let val = &self.data[id.0 as usize];
        Value::from_slot(self, id, val)
    }

    /// Get [`ObjectField`] by id.
    pub fn get_field<'a>(&'a self, id: ObjectFieldId) -> ObjectField<'a> {
        let name_slot = &self.data[id.slot_idx as usize];
        let value_slot = &self.data[id.slot_idx as usize + 1];

        let name = match &name_slot {
            Slot::ObjectField(name) => name,
            val => unreachable!("invalid object field in arena for id {id:?}: {val:?}"),
        };

        let value = Value::from_slot(self, ValueId(id.slot_idx + 1), value_slot);

        ObjectField {
            id,
            name: *name,
            value,
        }
    }

    /// Add primitive value to arena.
    pub fn add_primitive(&mut self, value: PrimitiveValue) -> ValueId {
        let idx = self.data.len();
        self.data.push(value.into());
        ValueId(idx as u32)
    }

    /// Insert an object into the arena.
    pub fn start_object<'a>(&'a mut self) -> ObjectBuilder<'a> {
        ObjectBuilder::new(&mut self.data, None)
    }

    /// Insert an array into the arena.
    pub fn start_array<'a>(&'a mut self) -> ArrayBuilder<'a> {
        ArrayBuilder::new(&mut self.data, None)
    }
}

#[cfg(test)]
mod test {
    use crate::string::StringInterner;
    use crate::value::{ArrayItem, ObjectFieldId, PrimitiveValue, Value, ValueArena, ValueId};

    use super::Slot;

    #[test]
    fn builds_nested_values() {
        let mut arena = ValueArena::new();

        // Insert some strings
        let mut interner = StringInterner::default();
        let hello_str = interner.insert("hello");
        let flag_str = interner.insert("flag");
        let container_str = interner.insert("container");
        let numbers_str = interner.insert("numbers");
        let deep_str = interner.insert("deep");
        let unused_str = interner.insert("unused");
        let value_str = interner.insert("value");
        let label_str = interner.insert("label");
        let done_str = interner.insert("done");
        let nested_str = interner.insert("nested");
        let status_str = interner.insert("status");
        let ok_str = interner.insert("ok");
        let count_str = interner.insert("count");
        let active_str = interner.insert("active");

        let null_id = arena.add_primitive(PrimitiveValue::Null);
        let bool_id = arena.add_primitive(PrimitiveValue::Bool(true));
        let string_id = arena.add_primitive(PrimitiveValue::String(hello_str));

        // root object will look like this:
        // {
        //   "flag": 7,
        //   "container": {
        //     "numbers": [
        //       1,
        //       ["deep", 99],
        //       {
        //         "value": 2.5,
        //         "label": "done"
        //       }
        //     ],
        //     "nested": {
        //       "status": "ok",
        //       "count": 2
        //     },
        //     "active": false
        //   }
        // }
        let mut root = arena.start_object();
        root.add_primitive(flag_str, PrimitiveValue::Integer(7));

        let (mut container, container_field_id) = root.start_object(container_str);
        let (mut numbers_builder, numbers_field_id) = container.start_array(numbers_str);
        numbers_builder.add_primitive(PrimitiveValue::Integer(1));

        let mut deep_array_builder = numbers_builder.start_array();
        deep_array_builder.add_primitive(PrimitiveValue::String(deep_str));
        deep_array_builder.add_primitive(PrimitiveValue::Integer(99));
        deep_array_builder.finish();

        let mut array_obj_builder = numbers_builder.start_object();
        {
            let (mut dropped_array, _) = array_obj_builder.start_array(unused_str);
            dropped_array.add_primitive(PrimitiveValue::Integer(123));
            // Dropped without finish on purpose to ensure cleanup.
        }

        array_obj_builder.add_primitive(value_str, PrimitiveValue::Float(2.5));
        array_obj_builder.add_primitive(label_str, PrimitiveValue::String(done_str));
        let array_obj_id = array_obj_builder.finish();

        let numbers_id = numbers_builder.finish();

        {
            let (mut dropped_object, _) = container.start_object(unused_str);
            dropped_object.add_primitive(unused_str, PrimitiveValue::Null);
            // Dropped without finish on purpose to ensure cleanup.
        }

        let (mut nested_builder, nested_field_id) = container.start_object(nested_str);
        let status_field_id =
            nested_builder.add_primitive(status_str, PrimitiveValue::String(ok_str));
        let count_field_id = nested_builder.add_primitive(count_str, PrimitiveValue::Integer(2));
        let nested_obj_id = nested_builder.finish();

        container.add_primitive(active_str, PrimitiveValue::Bool(false));
        let container_id = container.finish();

        let root_id = root.finish();

        // Check that the layout of the arena is as expected
        let expected_slots = vec![
            Slot::Primitive(PrimitiveValue::Null),
            Slot::Primitive(PrimitiveValue::Bool(true)),
            Slot::Primitive(PrimitiveValue::String(hello_str)),
            Slot::Object { end: ValueId(27) },
            Slot::ObjectField(flag_str),
            Slot::Primitive(PrimitiveValue::Integer(7)),
            Slot::ObjectField(container_str),
            Slot::Object { end: ValueId(27) },
            Slot::ObjectField(numbers_str),
            Slot::Array { end: ValueId(19) },
            Slot::Primitive(PrimitiveValue::Integer(1)),
            Slot::Array { end: ValueId(14) },
            Slot::Primitive(PrimitiveValue::String(deep_str)),
            Slot::Primitive(PrimitiveValue::Integer(99)),
            Slot::Object { end: ValueId(19) },
            Slot::ObjectField(value_str),
            Slot::Primitive(PrimitiveValue::Float(2.5)),
            Slot::ObjectField(label_str),
            Slot::Primitive(PrimitiveValue::String(done_str)),
            Slot::ObjectField(nested_str),
            Slot::Object { end: ValueId(25) },
            Slot::ObjectField(status_str),
            Slot::Primitive(PrimitiveValue::String(ok_str)),
            Slot::ObjectField(count_str),
            Slot::Primitive(PrimitiveValue::Integer(2)),
            Slot::ObjectField(active_str),
            Slot::Primitive(PrimitiveValue::Bool(false)),
        ];

        assert_eq!(arena.data.len(), expected_slots.len());
        for (idx, expected) in expected_slots.iter().enumerate() {
            assert_eq!(&arena.data[idx], expected, "slot mismatch at index {idx}");
        }

        // Check primitive value getting works
        assert!(matches!(arena.get(null_id), Value::Null));
        assert!(matches!(arena.get(bool_id), Value::Bool(true)));
        assert!(matches!(arena.get(string_id), Value::String(id) if id == hello_str));

        // Check that getters for nested values work by checking that root
        // object is exactly how it should be
        let root_object = match arena.get(root_id) {
            Value::Object(object) => object,
            other => panic!("expected object at root_id, got {other:?}"),
        };
        let mut root_object_fields = root_object.fields();

        let flag_field = root_object_fields.next().expect("flag field");
        assert_eq!(flag_field.name, flag_str);
        assert!(matches!(flag_field.value, Value::Integer(7)));

        let container_field = root_object_fields.next().expect("container field");
        assert_eq!(container_field.name, container_str);
        let container_object = match container_field.value {
            Value::Object(object) => object,
            other => panic!("expected object for container field, got {other:?}"),
        };
        assert!(root_object_fields.next().is_none());

        let mut container_object_fields = container_object.fields();

        let numbers_field = container_object_fields.next().expect("numbers field");
        assert_eq!(numbers_field.name, numbers_str);
        let numbers_array = match numbers_field.value {
            Value::Array(array) => array,
            other => panic!("expected array for numbers field, got {other:?}"),
        };

        // Check the numbers array
        let mut numbers_array_items = numbers_array.items();
        let first_numbers_item = numbers_array_items.next().expect("first array item");
        assert!(matches!(
            first_numbers_item,
            ArrayItem {
                id: ValueId(10),
                value: Value::Integer(1)
            }
        ));

        let second_numbers_item = numbers_array_items.next().expect("second array item");
        assert_eq!(second_numbers_item.id, ValueId(11));
        let deep_array = match second_numbers_item.value {
            Value::Array(array) => array,
            other => panic!("expected nested array, got {other:?}"),
        };
        let mut deep_array_items = deep_array.items();
        assert!(matches!(
            deep_array_items.next().expect("deep array first item"),
            ArrayItem {
                id: ValueId(12),
                value: Value::String(id)
            } if id == deep_str
        ));
        assert!(matches!(
            deep_array_items.next().expect("deep array second item"),
            ArrayItem {
                id: ValueId(13),
                value: Value::Integer(99)
            }
        ));
        assert!(deep_array_items.next().is_none());

        let third_numbers_item = numbers_array_items.next().expect("third array item");
        assert_eq!(third_numbers_item.id, ValueId(14));
        let array_object = match third_numbers_item.value {
            Value::Object(object) => object,
            other => panic!("expected nested object, got {other:?}"),
        };

        let mut array_object_fields = array_object.fields();

        let value_field = array_object_fields.next().expect("value field");
        assert_eq!(value_field.name, value_str);
        assert!(matches!(value_field.value, Value::Float(2.5)));

        let label_field = array_object_fields.next().expect("label field");
        assert_eq!(label_field.name, label_str);
        assert!(matches!(label_field.value, Value::String(id) if id == done_str));
        assert!(array_object_fields.next().is_none());

        assert!(numbers_array_items.next().is_none());

        // End of number array, continue with next field in container

        let nested_field = container_object_fields.next().expect("nested field");
        assert_eq!(nested_field.name, nested_str);
        let nested_object = match nested_field.value {
            Value::Object(object) => object,
            other => panic!("expected object for nested field, got {other:?}"),
        };

        // Check nested object
        let mut nested_object_fields = nested_object.fields();

        let status_field = nested_object_fields.next().expect("status field");
        assert_eq!(status_field.name, status_str);
        assert!(matches!(status_field.value, Value::String(id) if id == ok_str));

        let count_field = nested_object_fields.next().expect("count field");
        assert_eq!(count_field.name, count_str);
        assert!(matches!(count_field.value, Value::Integer(2)));
        assert!(nested_object_fields.next().is_none());

        // End of nested object, continue with active field

        let active_field = container_object_fields.next().expect("active field");
        assert_eq!(active_field.name, active_str);
        assert!(matches!(active_field.value, Value::Bool(false)));
        assert!(container_object_fields.next().is_none());

        // Check getting container directly by id
        let container_get = match arena.get(container_id) {
            Value::Object(object) => object,
            other => panic!("expected object at inner_id, got {other:?}"),
        };
        let inner_field_names: Vec<_> = container_get.fields().map(|f| f.name).collect();
        assert_eq!(inner_field_names, vec![numbers_str, nested_str, active_str]);

        // Check getting numbers array directly by id
        let numbers_from_get = match arena.get(numbers_id) {
            Value::Array(array) => array,
            other => panic!("expected array at numbers_id, got {other:?}"),
        };

        let mut numbers_from_get_items = numbers_from_get.items();
        assert!(matches!(
            numbers_from_get_items
                .next()
                .expect("numbers via get first"),
            ArrayItem {
                id: ValueId(10),
                value: Value::Integer(1)
            }
        ));

        let deep_from_get = numbers_from_get_items
            .next()
            .expect("numbers via get second");
        assert_eq!(deep_from_get.id, ValueId(11));
        let mut deep_from_get_iter = match deep_from_get.value {
            Value::Array(array) => array.items(),
            other => panic!("expected nested array via get, got {other:?}"),
        };
        assert!(matches!(
            deep_from_get_iter.next().expect("deep via get first"),
            ArrayItem {
                id: ValueId(12),
                value: Value::String(id)
            } if id == deep_str
        ));
        assert!(matches!(
            deep_from_get_iter.next().expect("deep via get second"),
            ArrayItem {
                id: ValueId(13),
                value: Value::Integer(99)
            }
        ));
        assert!(deep_from_get_iter.next().is_none());

        let array_from_get = numbers_from_get_items
            .next()
            .expect("numbers via get third");
        assert_eq!(array_from_get.id, ValueId(14));
        let object_from_get_iter = match array_from_get.value {
            Value::Object(object) => object,
            other => panic!("expected nested object via get, got {other:?}"),
        };
        let mut object_from_get_iter_fields = object_from_get_iter.fields();

        let value_field_get = object_from_get_iter_fields
            .next()
            .expect("nested object via get value field");
        assert_eq!(value_field_get.name, value_str);
        assert!(matches!(value_field_get.value, Value::Float(2.5)));

        let label_field_get = object_from_get_iter_fields
            .next()
            .expect("nested object via get label field");
        assert_eq!(label_field_get.name, label_str);
        assert!(matches!(label_field_get.value, Value::String(id) if id == done_str));
        assert!(object_from_get_iter_fields.next().is_none());
        assert!(numbers_from_get_items.next().is_none());

        // Check getting deeply nested object directly by id
        let array_obj_from_get = match arena.get(array_obj_id) {
            Value::Object(object) => object,
            other => panic!("expected object at array_obj_id, got {other:?}"),
        };

        let mut array_obj_from_get_fields = array_obj_from_get.fields();

        let value_field_direct = array_obj_from_get_fields
            .next()
            .expect("value field via get");
        assert_eq!(value_field_direct.name, value_str);
        assert!(matches!(value_field_direct.value, Value::Float(2.5)));
        let label_field_direct = array_obj_from_get_fields
            .next()
            .expect("label field via get");
        assert_eq!(label_field_direct.name, label_str);
        assert!(matches!(label_field_direct.value, Value::String(id) if id == done_str));
        assert!(array_obj_from_get_fields.next().is_none());

        // Check getting nested object directly by id
        let nested_from_get = match arena.get(nested_obj_id) {
            Value::Object(object) => object,
            other => panic!("expected object at nested_obj_id, got {other:?}"),
        };
        let mut nested_from_get_fields = nested_from_get.fields();

        assert!(matches!(
            nested_from_get_fields.next().expect("nested via get status").value,
            Value::String(id) if id == ok_str
        ));
        assert!(matches!(
            nested_from_get_fields
                .next()
                .expect("nested via get count")
                .value,
            Value::Integer(2)
        ));
        assert!(nested_from_get_fields.next().is_none());

        // Check field ids
        assert_eq!(
            container_field_id,
            ObjectFieldId {
                object_id: root_id,
                slot_idx: 6,
            }
        );

        assert_eq!(
            numbers_field_id,
            ObjectFieldId {
                object_id: container_id,
                slot_idx: 8,
            }
        );

        assert_eq!(
            nested_field_id,
            ObjectFieldId {
                object_id: container_id,
                slot_idx: 19,
            }
        );

        assert_eq!(
            status_field_id,
            ObjectFieldId {
                object_id: nested_obj_id,
                slot_idx: 21,
            }
        );

        assert_eq!(
            count_field_id,
            ObjectFieldId {
                object_id: nested_obj_id,
                slot_idx: 23,
            }
        );
    }
}
