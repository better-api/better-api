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

use crate::StringId;

/// Representation of a value.
#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Null,
    String(StringId),
    Bool(bool),
    Integer(i128), // i128 is large enough for i64 and u64
    Float(f64),
    Object(Object<'a>),
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
                arena,
                current: ValueId(id.0 + 1),
                end: *end,
                id,
                field_idx: 0,
            }),
            Slot::Array { end } => Value::Array(Array {
                arena,
                current: ValueId(id.0 + 1),
                end: *end,
            }),
            Slot::ObjectField(_) => {
                unreachable!("invalid conversion of Slot::ObjectField to Value")
            }
        }
    }
}

/// A field inside of the object.
pub struct ObjectField<'a> {
    pub id: ObjectFieldId,
    pub name: StringId,
    pub value: Value<'a>,
}

/// Object value returned by the [`ValueArena`].
///
/// It's an iterator where each item is an [`ObjectField`].
#[derive(derive_more::Debug, PartialEq)]
pub struct Object<'a> {
    #[debug(skip)]
    arena: &'a ValueArena,
    current: ValueId,
    end: ValueId,

    // Id of the object
    id: ValueId,
    // Current field index
    field_idx: u32,
}

impl<'a> Iterator for Object<'a> {
    type Item = ObjectField<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 >= self.end.0 {
            return None;
        }

        let name_slot = &self.arena.data[self.current.0 as usize];
        let value_slot = &self.arena.data[self.current.0 as usize + 1];

        let name = match &name_slot {
            Slot::ObjectField(name) => name,
            val => unreachable!("invalid object field in arena: {val:?}"),
        };

        let value = Value::from_slot(self.arena, ValueId(self.current.0 + 1), value_slot);

        self.current = match value {
            Value::Object(Object { end, .. }) => end,
            Value::Array(Array { end, .. }) => end,
            _ => ValueId(self.current.0 + 2),
        };

        let field_id = ObjectFieldId {
            value: self.id,
            idx: self.field_idx,
        };
        self.field_idx += 1;

        Some(ObjectField {
            id: field_id,
            name: *name,
            value,
        })
    }
}

/// Array value returned by the [`ValueArena`].
///
/// It's an iterator where each item is an [`Value`].
#[derive(derive_more::Debug, PartialEq)]
pub struct Array<'a> {
    #[debug(skip)]
    arena: &'a ValueArena,
    current: ValueId,
    end: ValueId,
}

impl<'a> Iterator for Array<'a> {
    type Item = Value<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 >= self.end.0 {
            return None;
        }

        let item = &self.arena.data[self.current.0 as usize];
        let value = Value::from_slot(self.arena, self.current, item);

        self.current = match value {
            Value::Object(Object { end, .. }) => end,
            Value::Array(Array { end, .. }) => end,
            _ => ValueId(self.current.0 + 1),
        };

        Some(value)
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

trait BuilderParent {
    fn arena(&mut self) -> &mut ValueArena;

    /// This method is called by a child builder if it's dropped without
    /// being finished successfully.
    ///
    /// For some builders this is a noop.
    fn drop_child(&mut self);

    fn data(&mut self) -> &mut Vec<Slot> {
        &mut self.arena().data
    }
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
    parent: &'p mut dyn BuilderParent,

    /// Index in the arena that contains Slot::Array of this array.
    start: ValueId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'p> BuilderParent for ArrayBuilder<'p> {
    fn arena(&mut self) -> &mut ValueArena {
        self.parent.arena()
    }

    fn drop_child(&mut self) {
        // Nothing to do
    }
}

impl<'p> ArrayBuilder<'p> {
    fn new(parent: &'p mut dyn BuilderParent) -> Self {
        let data = &mut parent.arena().data;

        let idx = data.len();
        data.push(Slot::Array { end: ValueId(0) });

        Self {
            parent,
            start: ValueId(idx as u32),
            finished: false,
        }
    }

    /// Add a new primitive value to the array.
    pub fn add_primitive(&mut self, value: PrimitiveValue) {
        self.data().push(value.into());
    }

    /// Insert an array as the next element of this array.
    pub fn start_array<'a>(&'a mut self) -> ArrayBuilder<'a> {
        ArrayBuilder::new(self)
    }

    /// Insert an object as the next element of this array.
    pub fn start_object<'a>(&'a mut self) -> ObjectBuilder<'a> {
        ObjectBuilder::new(self)
    }

    /// Constructs the final array.
    ///
    /// Returns the id of the final array in the arena.
    pub fn finish(mut self) -> ValueId {
        self.finished = true;

        let idx = self.data().len();

        let start = self.start.0 as usize;
        let head = &mut self.data()[start];
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

        let len = self.start.0 as usize;
        self.data().truncate(len);

        self.parent.drop_child();
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
    parent: &'p mut dyn BuilderParent,

    /// Index in the arena that contains Slot::Object of this object.
    start: ValueId,

    /// Number of fields in the object
    nr_fields: u32,

    /// Was finished called, used by drop implementation.
    finished: bool,

    /// Index of the last inserted field.
    ///
    /// That is arena[last_field_idx] points to the name of the field that was last inserted.
    /// Used for cleaning up when child builder is dropped without calling `.finish()`.
    ///
    /// It is enough to handle last field, because child builder has to borrow this builder as
    /// &mut, therefore you have to either drop a child builder or call `.finish()` before creating
    /// a new one.
    last_field_idx: Option<u32>,
}

impl<'p> BuilderParent for ObjectBuilder<'p> {
    fn arena(&mut self) -> &mut ValueArena {
        self.parent.arena()
    }

    fn drop_child(&mut self) {
        self.nr_fields -= 1;

        // Truncate arena length so that it ends at last inserted field.
        if let Some(len) = self.last_field_idx {
            self.arena().data.truncate(len as usize);
        }
    }
}

impl<'p> ObjectBuilder<'p> {
    fn new(parent: &'p mut dyn BuilderParent) -> Self {
        let data = &mut parent.arena().data;

        let idx = data.len();
        data.push(Slot::Object { end: ValueId(0) });

        Self {
            parent,
            start: ValueId(idx as u32),
            nr_fields: 0,
            finished: false,
            last_field_idx: None,
        }
    }

    /// Add a new field to the object with primitive value.
    pub fn add_primitive(&mut self, name: StringId, value: PrimitiveValue) -> ObjectFieldId {
        self.last_field_idx = Some(self.data().len() as u32);

        self.data().push(Slot::ObjectField(name));
        self.data().push(value.into());

        let field_id = ObjectFieldId {
            value: self.start,
            idx: self.nr_fields,
        };
        self.nr_fields += 1;

        field_id
    }

    /// Add a new field to the object with object value.
    ///
    /// Returns object builder and [`ObjectFieldId`] of the created field.
    /// If returned builder, this builder or any parent builder is dropped before
    /// calling `.finish()` on it, the returned [`ObjectFieldId`] will be invalid!
    pub fn start_object<'a>(&'a mut self, name: StringId) -> (ObjectBuilder<'a>, ObjectFieldId) {
        self.last_field_idx = Some(self.data().len() as u32);

        self.data().push(Slot::ObjectField(name));

        let field_id = ObjectFieldId {
            value: self.start,
            idx: self.nr_fields,
        };
        self.nr_fields += 1;

        (ObjectBuilder::new(self), field_id)
    }

    /// Add a new field to the object with array value.
    ///
    /// Returns array builder and [`ObjectFieldId`] of the created field.
    /// If returned builder, this builder or any parent builder is dropped before
    /// calling `.finish()` on it, the returned [`ObjectFieldId`] will be invalid!
    pub fn start_array<'a>(&'a mut self, name: StringId) -> (ArrayBuilder<'a>, ObjectFieldId) {
        self.last_field_idx = Some(self.data().len() as u32);

        self.data().push(Slot::ObjectField(name));

        let field_id = ObjectFieldId {
            value: self.start,
            idx: self.nr_fields,
        };
        self.nr_fields += 1;

        (ArrayBuilder::new(self), field_id)
    }

    /// Constructs the final object.
    ///
    /// Returns the id of the final object in the arena.
    pub fn finish(mut self) -> ValueId {
        self.finished = true;

        let idx = self.data().len();

        let start = self.start.0 as usize;
        let head = &mut self.data()[start];
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

        let len = self.start.0 as usize;
        self.data().truncate(len);

        self.parent.drop_child();
    }
}

/// Id of the value in the [`ValueArena`] .
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ValueId(u32);

/// Id of a object field.
///
/// Example of getting object field from arena:
/// ```ignore
/// let object = match arena.get(field_id.value_id()) {
///     Value::Object(obj) => obj,
///     _ => panic!("arena is invalid"),
/// };
///
/// let field = object.nth(field_id.index());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ObjectFieldId {
    value: ValueId,
    idx: u32,
}

impl ObjectFieldId {
    /// Get [`ValueId`] of the object that contains this field.
    pub fn value_id(&self) -> ValueId {
        self.value
    }

    /// Get index of the field inside an object.
    pub fn index(&self) -> u32 {
        self.idx
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

    /// Clears the value arena, removing all values.
    ///
    /// Note that this method has no effect on the allocated
    /// capacity of the arena.
    pub fn clear(&mut self) {
        self.data.clear();
    }

    /// Get value by id.
    pub fn get<'a>(&'a self, id: ValueId) -> Value<'a> {
        let val = &self.data[id.0 as usize];
        Value::from_slot(self, id, val)
    }

    /// Add primitive value to arena.
    pub fn add_primitive(&mut self, value: PrimitiveValue) -> ValueId {
        let idx = self.data.len();
        self.data.push(value.into());
        ValueId(idx as u32)
    }

    /// Insert an object into the arena.
    pub fn start_object<'a>(&'a mut self) -> ObjectBuilder<'a> {
        ObjectBuilder::new(self)
    }

    /// Insert an array into the arena.
    pub fn start_array<'a>(&'a mut self) -> ArrayBuilder<'a> {
        ArrayBuilder::new(self)
    }
}

impl BuilderParent for ValueArena {
    fn arena(&mut self) -> &mut ValueArena {
        self
    }

    fn drop_child(&mut self) {
        // Nothing to do
    }
}

#[cfg(test)]
mod test {
    use string_interner::DefaultStringInterner;

    use crate::value::{ObjectFieldId, PrimitiveValue, Value, ValueArena, ValueId};

    use super::Slot;

    #[test]
    fn builds_nested_values() {
        let mut arena = ValueArena::new();

        // Insert some strings
        let mut interner = DefaultStringInterner::default();
        let hello_str = interner.get_or_intern("hello");
        let flag_str = interner.get_or_intern("flag");
        let container_str = interner.get_or_intern("container");
        let numbers_str = interner.get_or_intern("numbers");
        let deep_str = interner.get_or_intern("deep");
        let unused_str = interner.get_or_intern("unused");
        let value_str = interner.get_or_intern("value");
        let label_str = interner.get_or_intern("label");
        let done_str = interner.get_or_intern("done");
        let nested_str = interner.get_or_intern("nested");
        let status_str = interner.get_or_intern("status");
        let ok_str = interner.get_or_intern("ok");
        let count_str = interner.get_or_intern("count");
        let active_str = interner.get_or_intern("active");

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
        nested_builder.add_primitive(status_str, PrimitiveValue::String(ok_str));
        nested_builder.add_primitive(count_str, PrimitiveValue::Integer(2));
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
        assert_eq!(arena.get(null_id), Value::Null);
        assert_eq!(arena.get(bool_id), Value::Bool(true));
        assert_eq!(arena.get(string_id), Value::String(hello_str));

        // Check that getters for nested values work by checking that root
        // object is exactly how it should be
        let mut root_object = match arena.get(root_id) {
            Value::Object(object) => object,
            other => panic!("expected object at root_id, got {other:?}"),
        };

        let flag_field = root_object.next().expect("flag field");
        assert_eq!(flag_field.name, flag_str);
        assert_eq!(flag_field.value, Value::Integer(7));

        let container_field = root_object.next().expect("container field");
        assert_eq!(container_field.name, container_str);
        let mut container_object = match container_field.value {
            Value::Object(object) => object,
            other => panic!("expected object for container field, got {other:?}"),
        };
        assert!(root_object.next().is_none());

        let numbers_field = container_object.next().expect("numbers field");
        assert_eq!(numbers_field.name, numbers_str);
        let mut numbers_array = match numbers_field.value {
            Value::Array(array) => array,
            other => panic!("expected array for numbers field, got {other:?}"),
        };

        // Check the numbers array
        let first_numbers_item = numbers_array.next().expect("first array item");
        assert_eq!(first_numbers_item, Value::Integer(1));

        let second_numbers_item = numbers_array.next().expect("second array item");
        let mut deep_array = match second_numbers_item {
            Value::Array(array) => array,
            other => panic!("expected nested array, got {other:?}"),
        };
        assert_eq!(
            deep_array.next().expect("deep array first item"),
            Value::String(deep_str)
        );
        assert_eq!(
            deep_array.next().expect("deep array second item"),
            Value::Integer(99)
        );
        assert!(deep_array.next().is_none());

        let third_numbers_item = numbers_array.next().expect("third array item");
        let mut array_object = match third_numbers_item {
            Value::Object(object) => object,
            other => panic!("expected nested object, got {other:?}"),
        };

        let value_field = array_object.next().expect("value field");
        assert_eq!(value_field.name, value_str);
        assert_eq!(value_field.value, Value::Float(2.5));

        let label_field = array_object.next().expect("label field");
        assert_eq!(label_field.name, label_str);
        assert_eq!(label_field.value, Value::String(done_str));
        assert!(array_object.next().is_none());

        assert!(numbers_array.next().is_none());

        // End of number array, continue with next field in container

        let nested_field = container_object.next().expect("nested field");
        assert_eq!(nested_field.name, nested_str);
        let mut nested_object = match nested_field.value {
            Value::Object(object) => object,
            other => panic!("expected object for nested field, got {other:?}"),
        };

        // Check nested object

        let status_field = nested_object.next().expect("status field");
        assert_eq!(status_field.name, status_str);
        assert_eq!(status_field.value, Value::String(ok_str));

        let count_field = nested_object.next().expect("count field");
        assert_eq!(count_field.name, count_str);
        assert_eq!(count_field.value, Value::Integer(2));
        assert!(nested_object.next().is_none());

        // End of nested object, continue with active field

        let active_field = container_object.next().expect("active field");
        assert_eq!(active_field.name, active_str);
        assert_eq!(active_field.value, Value::Bool(false));
        assert!(container_object.next().is_none());

        // Check getting container directly by id
        let container_get = match arena.get(container_id) {
            Value::Object(object) => object,
            other => panic!("expected object at inner_id, got {other:?}"),
        };
        let inner_field_names: Vec<_> = container_get.map(|f| f.name).collect();
        assert_eq!(inner_field_names, vec![numbers_str, nested_str, active_str]);

        // Check getting numbers array directly by id
        let mut numbers_from_get = match arena.get(numbers_id) {
            Value::Array(array) => array,
            other => panic!("expected array at numbers_id, got {other:?}"),
        };
        assert_eq!(
            numbers_from_get.next().expect("numbers via get first"),
            Value::Integer(1)
        );

        let deep_from_get = numbers_from_get.next().expect("numbers via get second");
        let mut deep_from_get_iter = match deep_from_get {
            Value::Array(array) => array,
            other => panic!("expected nested array via get, got {other:?}"),
        };
        assert_eq!(
            deep_from_get_iter.next().expect("deep via get first"),
            Value::String(deep_str)
        );
        assert_eq!(
            deep_from_get_iter.next().expect("deep via get second"),
            Value::Integer(99)
        );
        assert!(deep_from_get_iter.next().is_none());

        let array_from_get = numbers_from_get.next().expect("numbers via get third");
        let mut object_from_get_iter = match array_from_get {
            Value::Object(object) => object,
            other => panic!("expected nested object via get, got {other:?}"),
        };
        let value_field_get = object_from_get_iter
            .next()
            .expect("nested object via get value field");
        assert_eq!(value_field_get.name, value_str);
        assert_eq!(value_field_get.value, Value::Float(2.5));

        let label_field_get = object_from_get_iter
            .next()
            .expect("nested object via get label field");
        assert_eq!(label_field_get.name, label_str);
        assert_eq!(label_field_get.value, Value::String(done_str));
        assert!(object_from_get_iter.next().is_none());
        assert!(numbers_from_get.next().is_none());

        // Check getting deeply nested object directly by id
        let mut array_obj_from_get = match arena.get(array_obj_id) {
            Value::Object(object) => object,
            other => panic!("expected object at array_obj_id, got {other:?}"),
        };
        let value_field_direct = array_obj_from_get.next().expect("value field via get");
        assert_eq!(value_field_direct.name, value_str);
        assert_eq!(value_field_direct.value, Value::Float(2.5));
        let label_field_direct = array_obj_from_get.next().expect("label field via get");
        assert_eq!(label_field_direct.name, label_str);
        assert_eq!(label_field_direct.value, Value::String(done_str));
        assert!(array_obj_from_get.next().is_none());

        // Check getting nested object directly by id
        let mut nested_from_get = match arena.get(nested_obj_id) {
            Value::Object(object) => object,
            other => panic!("expected object at nested_obj_id, got {other:?}"),
        };
        assert_eq!(
            nested_from_get.next().expect("nested via get status").value,
            Value::String(ok_str)
        );
        assert_eq!(
            nested_from_get.next().expect("nested via get count").value,
            Value::Integer(2)
        );
        assert!(nested_from_get.next().is_none());

        // Check field ids.
        assert_eq!(
            container_field_id,
            ObjectFieldId {
                value: root_id,
                idx: 1,
            }
        );

        assert_eq!(
            numbers_field_id,
            ObjectFieldId {
                value: container_id,
                idx: 0,
            }
        );

        assert_eq!(
            nested_field_id,
            ObjectFieldId {
                value: container_id,
                idx: 1,
            }
        );
    }
}
