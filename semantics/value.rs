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
        value::{PrimitiveValue, Value, ValueArena},
    };

    #[test]
    fn asdf() {
        let mut diags = vec![];
        let tokens = tokenize("", &mut diags);
        let res = parse(tokens);

        let node = res.root.syntax();

        let mut arena = ValueArena::new();
        let mut o1 = arena.start_object(node);
        let mut o2 = o1.start_object(node, Name::new("object").unwrap(), node);

        o2.add_primitive(
            node,
            Name::new("primitive_inner").unwrap(),
            node,
            PrimitiveValue::Null,
        );
        o2.finish();

        o1.add_primitive(
            node,
            Name::new("primitive").unwrap(),
            node,
            PrimitiveValue::Bool(false),
        );
        o1.finish();

        arena.add_primitive(node, PrimitiveValue::Null);

        let mut arr = arena.start_array(node);
        arr.add_primitive(node, PrimitiveValue::Integer(1));
        arr.add_primitive(node, PrimitiveValue::Integer(2));

        let mut arr2 = arr.start_array(node);
        arr2.add_primitive(node, PrimitiveValue::Float(42.69));
        arr2.finish();

        arr.add_primitive(node, PrimitiveValue::Null);
        let arr_id = arr.finish();

        // let primitives: Vec<_> = arena.data.into_iter().map(|v| v.data).collect();
        // assert_eq!(primitives, vec![]);

        // let mut obj = match arena.get(ValueId(0)).data {
        //     Value::Object(object) => object,
        //     _ => unreachable!(),
        // };
        // let mut inner = match obj.next().unwrap().data.value.data {
        //     Value::Object(object) => object,
        //     _ => unreachable!(),
        // };
        //
        // let names: Vec<_> = inner.map(|f| f.data.value.data).collect();
        // assert_eq!(names, vec![]);

        let mut arr = match arena.get(arr_id).data {
            Value::Array(arr) => arr,
            _ => unreachable!(),
        };
        let vals: Vec<_> = arr.by_ref().map(|v| v.data).collect();
        assert_eq!(vals, vec![]);
    }
}
