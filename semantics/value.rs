//! Defines semantic representation of values.
//!
//! The main data structure is a [`ValueArena`] that holds
//! the values. Values in the arena are referenced with [`ValueId`].
//! All values in the arena are [tracked](crate::Tracked), which allows
//! us to drop down to SyntaxNode.
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
//!
//! ## Strings and Names
//!
//! Some values hold a [`&Name`](Name) which is a wrapper around a &str.
//! Ownership of names and strings should be handled by [`StringCache`](crate::text::StringCache)
//!
//! ## Lifetimes
//!
//! - `'s` represents the lifetime of string slices stored in the
//!   [`StringCache`](crate::text::StringCache).
//! - `'a` ties a value to the lifetime of the [`ValueArena`].
use better_api_syntax::SyntaxNode;

use crate::Tracked;
use crate::name::Name;

/// Representation of a value.
#[derive(Debug, PartialEq)]
pub enum Value<'s, 'a> {
    Null,
    String(&'s str),
    Bool(bool),
    Integer(i128), // i128 is large enough for i64 and u64
    Float(f64),
    Object(Object<'s, 'a>),
    Array(Array<'s, 'a>),
}

impl<'s, 'a> From<PrimitiveValue<'s>> for Value<'s, 'a> {
    fn from(value: PrimitiveValue<'s>) -> Self {
        match value {
            PrimitiveValue::Null => Value::Null,
            PrimitiveValue::String(s) => Value::String(s),
            PrimitiveValue::Bool(b) => Value::Bool(b),
            PrimitiveValue::Integer(i) => Value::Integer(i),
            PrimitiveValue::Float(f) => Value::Float(f),
        }
    }
}

impl<'s, 'a> Value<'s, 'a> {
    fn from_slot(arena: &'a ValueArena<'s>, id: ValueId, slot: &'a Slot<'s>) -> Self {
        match slot {
            Slot::Primitive(val) => (*val).into(),
            Slot::Object { end } => Value::Object(Object {
                arena,
                current: ValueId(id.0 + 1),
                end: *end,
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
pub struct ObjectField<'s, 'a> {
    pub name: &'s Name,
    pub value: TrackedValue<'s, 'a>,
}

/// Object field together with a pointer to syntax node.
pub type TrackedObjectField<'s, 'a> = Tracked<ObjectField<'s, 'a>>;

/// Value together with a pointer to syntax node.
pub type TrackedValue<'s, 'a> = Tracked<Value<'s, 'a>>;

/// Object value returned by the [`ValueArena`].
///
/// It's an iterator where each item is an [`TrackedObjectField`].
#[derive(derive_more::Debug, PartialEq)]
pub struct Object<'s, 'a> {
    #[debug(skip)]
    arena: &'a ValueArena<'s>,
    current: ValueId,
    end: ValueId,
}

impl<'s, 'a> Iterator for Object<'s, 'a> {
    type Item = TrackedObjectField<'s, 'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 >= self.end.0 {
            return None;
        }

        let name_slot = &self.arena.data[self.current.0 as usize];
        let value_slot = &self.arena.data[self.current.0 as usize + 1];

        let name = match &name_slot.data {
            Slot::ObjectField(name) => name,
            val => unreachable!("invalid object field in arena: {val:?}"),
        };

        let value = Value::from_slot(self.arena, ValueId(self.current.0 + 1), &value_slot.data);

        self.current = match value {
            Value::Object(Object { end, .. }) => end,
            Value::Array(Array { end, .. }) => end,
            _ => ValueId(self.current.0 + 2),
        };

        Some(TrackedObjectField {
            syntax: name_slot.syntax,
            data: ObjectField {
                name,
                value: TrackedValue {
                    syntax: value_slot.syntax,
                    data: value,
                },
            },
        })
    }
}

/// Array value returned by the [`ValueArena`].
///
/// It's an iterator where each item is an [`TrackedValue`].
#[derive(derive_more::Debug, PartialEq)]
pub struct Array<'s, 'a> {
    #[debug(skip)]
    arena: &'a ValueArena<'s>,
    current: ValueId,
    end: ValueId,
}

impl<'s, 'a> Iterator for Array<'s, 'a> {
    type Item = TrackedValue<'s, 'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 >= self.end.0 {
            return None;
        }

        let item = &self.arena.data[self.current.0 as usize];
        let value = Value::from_slot(self.arena, self.current, &item.data);

        self.current = match value {
            Value::Object(Object { end, .. }) => end,
            Value::Array(Array { end, .. }) => end,
            _ => ValueId(self.current.0 + 1),
        };

        Some(TrackedValue {
            syntax: item.syntax,
            data: value,
        })
    }
}

/// Non-composite value
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveValue<'s> {
    Null,
    String(&'s str),
    Bool(bool),
    Integer(i128),
    Float(f64),
}

trait BuilderParent<'s> {
    fn arena(&mut self) -> &mut ValueArena<'s>;

    fn is_field(&self) -> bool;

    fn data(&mut self) -> &mut Vec<TrackedSlot<'s>> {
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
pub struct ArrayBuilder<'s, 'p> {
    parent: &'p mut dyn BuilderParent<'s>,

    /// Index in the arena that contains Slot::Array of this array.
    start: ValueId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'s, 'p> BuilderParent<'s> for ArrayBuilder<'s, 'p> {
    fn arena(&mut self) -> &mut ValueArena<'s> {
        self.parent.arena()
    }

    fn is_field(&self) -> bool {
        false
    }
}

impl<'s, 'p> ArrayBuilder<'s, 'p> {
    fn new(parent: &'p mut dyn BuilderParent<'s>, node: &SyntaxNode) -> Self {
        let data = &mut parent.arena().data;

        let idx = data.len();
        data.push(Tracked::new(node, Slot::Array { end: ValueId(0) }));

        Self {
            parent,
            start: ValueId(idx as u32),
            finished: false,
        }
    }

    /// Add a new primitive value to the array.
    pub fn add_primitive(&mut self, node: &SyntaxNode, value: PrimitiveValue<'s>) {
        self.data().push(Tracked::new(node, value.into()));
    }

    /// Insert an array as the next element of this array.
    pub fn start_array<'a>(&'a mut self, node: &SyntaxNode) -> ArrayBuilder<'s, 'a> {
        ArrayBuilder::new(self, node)
    }

    /// Insert an object as the next element of this array.
    pub fn start_object<'a>(&'a mut self, node: &SyntaxNode) -> ObjectBuilder<'s, 'a> {
        ObjectBuilder::new(self, node)
    }

    /// Constructs the final array.
    ///
    /// Returns the id of the final array in the arena.
    pub fn finish(mut self) -> ValueId {
        self.finished = true;

        let idx = self.data().len();

        let start = self.start.0 as usize;
        let head = &mut self.data()[start];
        match &mut head.data {
            Slot::Array { end } => *end = ValueId(idx as u32),
            _ => unreachable!("invalid ArrayBuilder start"),
        }

        self.start
    }
}

impl<'s, 'p> Drop for ArrayBuilder<'s, 'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let mut len = self.start.0 as usize;

        // Also remove the field name of the parent.
        if self.parent.is_field() {
            len -= 1;
        }

        self.data().truncate(len);
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
pub struct ObjectBuilder<'s, 'p> {
    parent: &'p mut dyn BuilderParent<'s>,

    /// Index in the arena that contains Slot::Object of this object.
    start: ValueId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'s, 'p> BuilderParent<'s> for ObjectBuilder<'s, 'p> {
    fn arena(&mut self) -> &mut ValueArena<'s> {
        self.parent.arena()
    }

    fn is_field(&self) -> bool {
        true
    }
}

impl<'s, 'p> ObjectBuilder<'s, 'p> {
    fn new(parent: &'p mut dyn BuilderParent<'s>, node: &SyntaxNode) -> Self {
        let data = &mut parent.arena().data;

        let idx = data.len();
        data.push(Tracked::new(node, Slot::Object { end: ValueId(0) }));

        Self {
            parent,
            start: ValueId(idx as u32),
            finished: false,
        }
    }

    /// Add a new field to the object with primitive value.
    ///
    /// `field_node` should be the node of the whole field (name & value).
    /// `value_node` should be the node of only the value.
    pub fn add_primitive(
        &mut self,
        field_node: &SyntaxNode,
        name: &'s Name,
        value_node: &SyntaxNode,
        value: PrimitiveValue<'s>,
    ) {
        self.data()
            .push(Tracked::new(field_node, Slot::ObjectField(name)));
        self.data().push(Tracked::new(value_node, value.into()));
    }

    /// Add a new field to the object with object value.
    ///
    /// `field_node` should be the node of the whole field (name & object).
    /// `object_node` should be the node of only the object that will be built (field value).
    pub fn start_object<'a>(
        &'a mut self,
        field_node: &SyntaxNode,
        name: &'s Name,
        object_node: &SyntaxNode,
    ) -> ObjectBuilder<'s, 'a> {
        self.data()
            .push(Tracked::new(field_node, Slot::ObjectField(name)));

        ObjectBuilder::new(self, object_node)
    }

    /// Add a new field to the object with array value.
    ///
    /// `field_node` should be the node of the whole field (name & object).
    /// `object_node` should be the node of only the array that will be built (field value).
    pub fn start_array<'a>(
        &'a mut self,
        field_node: &SyntaxNode,
        name: &'s Name,
        array_node: &SyntaxNode,
    ) -> ArrayBuilder<'s, 'a> {
        self.data()
            .push(Tracked::new(field_node, Slot::ObjectField(name)));

        ArrayBuilder::new(self, array_node)
    }

    /// Constructs the final object.
    ///
    /// Returns the id of the final object in the arena.
    pub fn finish(mut self) -> ValueId {
        self.finished = true;

        let idx = self.data().len();

        let start = self.start.0 as usize;
        let head = &mut self.data()[start];
        match &mut head.data {
            Slot::Object { end } => *end = ValueId(idx as u32),
            _ => unreachable!("invalid ObjectBuilder start"),
        }

        self.start
    }
}

impl<'s, 'p> Drop for ObjectBuilder<'s, 'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let mut len = self.start.0 as usize;

        // Also remove the field name of the parent.
        if self.parent.is_field() {
            len -= 1;
        }

        self.data().truncate(len);
    }
}

/// Id of the value in the [`ValueArena`] .
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ValueId(u32);

/// Slot in the arena
#[derive(Debug, PartialEq)]
enum Slot<'s> {
    Primitive(PrimitiveValue<'s>),
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
    ObjectField(&'s Name),
}

impl<'s> From<PrimitiveValue<'s>> for Slot<'s> {
    fn from(value: PrimitiveValue<'s>) -> Self {
        Self::Primitive(value)
    }
}

type TrackedSlot<'s> = Tracked<Slot<'s>>;

/// Arena that holds the values.
#[derive(Debug, Default, PartialEq)]
pub struct ValueArena<'s> {
    data: Vec<TrackedSlot<'s>>,
}

impl<'s> ValueArena<'s> {
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
    pub fn get<'a>(&'a self, id: ValueId) -> TrackedValue<'s, 'a> {
        let val = &self.data[id.0 as usize];
        let data = Value::from_slot(self, id, &val.data);

        TrackedValue {
            syntax: val.syntax,
            data,
        }
    }

    /// Add primitive value to arena.
    pub fn add_primitive(&mut self, node: &SyntaxNode, value: PrimitiveValue<'s>) -> ValueId {
        let idx = self.data.len();
        self.data.push(Tracked::new(node, value.into()));
        ValueId(idx as u32)
    }

    /// Insert an object into the arena.
    pub fn start_object<'a>(&'a mut self, node: &SyntaxNode) -> ObjectBuilder<'s, 'a> {
        ObjectBuilder::new(self, node)
    }

    /// Insert an array into the arena.
    pub fn start_array<'a>(&'a mut self, node: &SyntaxNode) -> ArrayBuilder<'s, 'a> {
        ArrayBuilder::new(self, node)
    }
}

impl<'s> BuilderParent<'s> for ValueArena<'s> {
    fn arena(&mut self) -> &mut ValueArena<'s> {
        self
    }

    fn is_field(&self) -> bool {
        false
    }
}

#[cfg(test)]
mod test {
    use better_api_syntax::{ast::AstNode, parse, tokenize};

    use crate::{
        name::Name,
        value::{PrimitiveValue, Value, ValueArena, ValueId},
    };

    use super::Slot;

    /// Helper for creating names used in tests.
    fn name(val: &str) -> &Name {
        Name::new(val).unwrap()
    }

    #[test]
    fn builds_nested_values() {
        let mut diags = vec![];
        let tokens = tokenize("", &mut diags);
        let res = parse(tokens);
        let node = res.root.syntax();

        let mut arena = ValueArena::new();

        let null_id = arena.add_primitive(node, PrimitiveValue::Null);
        let bool_id = arena.add_primitive(node, PrimitiveValue::Bool(true));
        let string_id = arena.add_primitive(node, PrimitiveValue::String("hello"));

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
        let mut root = arena.start_object(node);
        root.add_primitive(node, name("flag"), node, PrimitiveValue::Integer(7));

        let mut container = root.start_object(node, name("container"), node);
        let mut numbers_builder = container.start_array(node, name("numbers"), node);
        numbers_builder.add_primitive(node, PrimitiveValue::Integer(1));

        let mut deep_array_builder = numbers_builder.start_array(node);
        deep_array_builder.add_primitive(node, PrimitiveValue::String("deep"));
        deep_array_builder.add_primitive(node, PrimitiveValue::Integer(99));
        deep_array_builder.finish();

        let mut array_obj_builder = numbers_builder.start_object(node);
        {
            let mut dropped_array = array_obj_builder.start_array(node, name("unused"), node);
            dropped_array.add_primitive(node, PrimitiveValue::Integer(123));
            // Dropped without finish on purpose to ensure cleanup.
        }

        array_obj_builder.add_primitive(node, name("value"), node, PrimitiveValue::Float(2.5));
        array_obj_builder.add_primitive(node, name("label"), node, PrimitiveValue::String("done"));
        let array_obj_id = array_obj_builder.finish();

        let numbers_id = numbers_builder.finish();

        {
            let mut dropped_object = container.start_object(node, name("unused"), node);
            dropped_object.add_primitive(node, name("unused"), node, PrimitiveValue::Null);
            // Dropped without finish on purpose to ensure cleanup.
        }

        let mut nested_builder = container.start_object(node, name("nested"), node);
        nested_builder.add_primitive(node, name("status"), node, PrimitiveValue::String("ok"));
        nested_builder.add_primitive(node, name("count"), node, PrimitiveValue::Integer(2));
        let nested_obj_id = nested_builder.finish();

        container.add_primitive(node, name("active"), node, PrimitiveValue::Bool(false));
        let container_id = container.finish();

        let root_id = root.finish();

        // Check that the layout of the arena is as expected
        let expected_slots = vec![
            Slot::Primitive(PrimitiveValue::Null),
            Slot::Primitive(PrimitiveValue::Bool(true)),
            Slot::Primitive(PrimitiveValue::String("hello")),
            Slot::Object { end: ValueId(27) },
            Slot::ObjectField(name("flag")),
            Slot::Primitive(PrimitiveValue::Integer(7)),
            Slot::ObjectField(name("container")),
            Slot::Object { end: ValueId(27) },
            Slot::ObjectField(name("numbers")),
            Slot::Array { end: ValueId(19) },
            Slot::Primitive(PrimitiveValue::Integer(1)),
            Slot::Array { end: ValueId(14) },
            Slot::Primitive(PrimitiveValue::String("deep")),
            Slot::Primitive(PrimitiveValue::Integer(99)),
            Slot::Object { end: ValueId(19) },
            Slot::ObjectField(name("value")),
            Slot::Primitive(PrimitiveValue::Float(2.5)),
            Slot::ObjectField(name("label")),
            Slot::Primitive(PrimitiveValue::String("done")),
            Slot::ObjectField(name("nested")),
            Slot::Object { end: ValueId(25) },
            Slot::ObjectField(name("status")),
            Slot::Primitive(PrimitiveValue::String("ok")),
            Slot::ObjectField(name("count")),
            Slot::Primitive(PrimitiveValue::Integer(2)),
            Slot::ObjectField(name("active")),
            Slot::Primitive(PrimitiveValue::Bool(false)),
        ];

        assert_eq!(arena.data.len(), expected_slots.len());
        for (idx, expected) in expected_slots.iter().enumerate() {
            assert_eq!(
                &arena.data[idx].data, expected,
                "slot mismatch at index {idx}"
            );
        }

        // Check primitive value getting works
        assert_eq!(arena.get(null_id).data, Value::Null);
        assert_eq!(arena.get(bool_id).data, Value::Bool(true));
        assert_eq!(arena.get(string_id).data, Value::String("hello"));

        // Check that getters for nested values work by checking that root
        // object is exactly how it should be
        let mut root_object = match arena.get(root_id).data {
            Value::Object(object) => object,
            other => panic!("expected object at root_id, got {other:?}"),
        };

        let flag_field = root_object.next().expect("flag field");
        assert_eq!(flag_field.data.name, name("flag"));
        assert_eq!(flag_field.data.value.data, Value::Integer(7));

        let container_field = root_object.next().expect("container field");
        assert_eq!(container_field.data.name, name("container"));
        let mut container_object = match container_field.data.value.data {
            Value::Object(object) => object,
            other => panic!("expected object for container field, got {other:?}"),
        };
        assert!(root_object.next().is_none());

        let numbers_field = container_object.next().expect("numbers field");
        assert_eq!(numbers_field.data.name, name("numbers"));
        let mut numbers_array = match numbers_field.data.value.data {
            Value::Array(array) => array,
            other => panic!("expected array for numbers field, got {other:?}"),
        };

        // Check the numbers array
        let first_numbers_item = numbers_array.next().expect("first array item");
        assert_eq!(first_numbers_item.data, Value::Integer(1));

        let second_numbers_item = numbers_array.next().expect("second array item");
        let mut deep_array = match second_numbers_item.data {
            Value::Array(array) => array,
            other => panic!("expected nested array, got {other:?}"),
        };
        assert_eq!(
            deep_array.next().expect("deep array first item").data,
            Value::String("deep")
        );
        assert_eq!(
            deep_array.next().expect("deep array second item").data,
            Value::Integer(99)
        );
        assert!(deep_array.next().is_none());

        let third_numbers_item = numbers_array.next().expect("third array item");
        let mut array_object = match third_numbers_item.data {
            Value::Object(object) => object,
            other => panic!("expected nested object, got {other:?}"),
        };

        let value_field = array_object.next().expect("value field");
        assert_eq!(value_field.data.name, name("value"));
        assert_eq!(value_field.data.value.data, Value::Float(2.5));

        let label_field = array_object.next().expect("label field");
        assert_eq!(label_field.data.name, name("label"));
        assert_eq!(label_field.data.value.data, Value::String("done"));
        assert!(array_object.next().is_none());

        assert!(numbers_array.next().is_none());

        // End of number array, continue with next field in container

        let nested_field = container_object.next().expect("nested field");
        assert_eq!(nested_field.data.name, name("nested"));
        let mut nested_object = match nested_field.data.value.data {
            Value::Object(object) => object,
            other => panic!("expected object for nested field, got {other:?}"),
        };

        // Check nested object

        let status_field = nested_object.next().expect("status field");
        assert_eq!(status_field.data.name, name("status"));
        assert_eq!(status_field.data.value.data, Value::String("ok"));

        let count_field = nested_object.next().expect("count field");
        assert_eq!(count_field.data.name, name("count"));
        assert_eq!(count_field.data.value.data, Value::Integer(2));
        assert!(nested_object.next().is_none());

        // End of nested object, continue with active field

        let active_field = container_object.next().expect("active field");
        assert_eq!(active_field.data.name, name("active"));
        assert_eq!(active_field.data.value.data, Value::Bool(false));
        assert!(container_object.next().is_none());

        // Check getting container directly by id
        let container_get = match arena.get(container_id).data {
            Value::Object(object) => object,
            other => panic!("expected object at inner_id, got {other:?}"),
        };
        let inner_field_names: Vec<_> = container_get.map(|f| f.data.name).collect();
        assert_eq!(
            inner_field_names,
            vec![name("numbers"), name("nested"), name("active")]
        );

        // Check getting numbers array directly by id
        let mut numbers_from_get = match arena.get(numbers_id).data {
            Value::Array(array) => array,
            other => panic!("expected array at numbers_id, got {other:?}"),
        };
        assert_eq!(
            numbers_from_get.next().expect("numbers via get first").data,
            Value::Integer(1)
        );

        let deep_from_get = numbers_from_get
            .next()
            .expect("numbers via get second")
            .data;
        let mut deep_from_get_iter = match deep_from_get {
            Value::Array(array) => array,
            other => panic!("expected nested array via get, got {other:?}"),
        };
        assert_eq!(
            deep_from_get_iter.next().expect("deep via get first").data,
            Value::String("deep")
        );
        assert_eq!(
            deep_from_get_iter.next().expect("deep via get second").data,
            Value::Integer(99)
        );
        assert!(deep_from_get_iter.next().is_none());

        let array_from_get = numbers_from_get.next().expect("numbers via get third").data;
        let mut object_from_get_iter = match array_from_get {
            Value::Object(object) => object,
            other => panic!("expected nested object via get, got {other:?}"),
        };
        let value_field_get = object_from_get_iter
            .next()
            .expect("nested object via get value field");
        assert_eq!(value_field_get.data.name, name("value"));
        assert_eq!(value_field_get.data.value.data, Value::Float(2.5));

        let label_field_get = object_from_get_iter
            .next()
            .expect("nested object via get label field");
        assert_eq!(label_field_get.data.name, name("label"));
        assert_eq!(label_field_get.data.value.data, Value::String("done"));
        assert!(object_from_get_iter.next().is_none());
        assert!(numbers_from_get.next().is_none());

        // Check getting deeply nested object directly by id
        let mut array_obj_from_get = match arena.get(array_obj_id).data {
            Value::Object(object) => object,
            other => panic!("expected object at array_obj_id, got {other:?}"),
        };
        let value_field_direct = array_obj_from_get.next().expect("value field via get");
        assert_eq!(value_field_direct.data.name, name("value"));
        assert_eq!(value_field_direct.data.value.data, Value::Float(2.5));
        let label_field_direct = array_obj_from_get.next().expect("label field via get");
        assert_eq!(label_field_direct.data.name, name("label"));
        assert_eq!(label_field_direct.data.value.data, Value::String("done"));
        assert!(array_obj_from_get.next().is_none());

        // Check getting nested object directly by id
        let mut nested_from_get = match arena.get(nested_obj_id).data {
            Value::Object(object) => object,
            other => panic!("expected object at nested_obj_id, got {other:?}"),
        };
        assert_eq!(
            nested_from_get
                .next()
                .expect("nested via get status")
                .data
                .value
                .data,
            Value::String("ok")
        );
        assert_eq!(
            nested_from_get
                .next()
                .expect("nested via get count")
                .data
                .value
                .data,
            Value::Integer(2)
        );
        assert!(nested_from_get.next().is_none());
    }
}
