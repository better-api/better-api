//! Defines semantic representation of values.

use std::ops::Range;

use crate::Tracked;
use crate::name::Name;

/// Representation of a value.
pub enum Value<'s, 'a> {
    Null,
    String(&'s str),
    Bool(bool),
    Integer(i128), // i128 is large enough for i64 and u64
    Float(f64),
    Object(Object<'s, 'a>),
    Array(Array<'s, 'a>),
}

/// A field inside of the object.
#[derive(Clone)]
pub struct ObjectField<'s> {
    pub name: &'s Name,
    pub value: ValueId,
}

/// Object field together with a pointer to syntax node.
pub type TrackedObjectField<'s> = Tracked<ObjectField<'s>>;

/// Value together with a pointer to syntax node.
pub type TrackedValue<'s, 'a> = Tracked<Value<'s, 'a>>;

/// Object value returned by the [`ValueArena`].
///
/// It's an iterator where each item is an [`TrackedObjectField`].
pub struct Object<'s, 'a> {
    arena: &'a ValueArena<'s>,
    range: Range<ObjectFieldId>,
}

impl<'s, 'a> Iterator for Object<'s, 'a> {
    type Item = TrackedObjectField<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.range.start.0 >= self.range.end.0 {
            return None;
        }

        let value = &self.arena.fields[self.range.start.0 as usize];
        self.range.start.0 += 1;
        Some(value.clone())
    }
}

/// Array value returned by the [`ValueArena`].
///
/// It's an iterator where each item is an [`TrackedValue`].
pub struct Array<'s, 'a> {
    arena: &'a ValueArena<'s>,
    range: Range<ValueId>,
}

impl<'s, 'a> Iterator for Array<'s, 'a> {
    type Item = TrackedValue<'s, 'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.range.start.0 >= self.range.end.0 {
            return None;
        }

        let value = self.arena.get(self.range.start);
        Some(value)
    }
}

/// Id of the value in the [`ValueArena`] .
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ValueId(u32);

/// Id of the object field in the [`ValueArena`] .
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct ObjectFieldId(u32);

/// Representation of value that is actually stored in the arena.
enum InternalValue<'s> {
    Null,
    String(&'s str),
    Bool(bool),
    Integer(i128), // i128 is large enough for i64 and u64
    Float(f64),
    Object(Range<ObjectFieldId>),
    Array(Range<ValueId>),
}

type TrackedInternalValue<'s> = Tracked<InternalValue<'s>>;

/// Arena that holds the values.
#[derive(Default)]
pub struct ValueArena<'s> {
    values: Vec<TrackedInternalValue<'s>>,
    fields: Vec<TrackedObjectField<'s>>,
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
        self.values.clear();
        self.fields.clear();
    }

    /// Get value
    pub fn get<'a>(&'a self, id: ValueId) -> TrackedValue<'s, 'a> {
        let val = &self.values[id.0 as usize];
        let data = match &val.data {
            InternalValue::Null => Value::Null,
            InternalValue::String(s) => Value::String(s),
            InternalValue::Bool(b) => Value::Bool(*b),
            InternalValue::Integer(i) => Value::Integer(*i),
            InternalValue::Float(f) => Value::Float(*f),
            InternalValue::Object(range) => Value::Object(Object {
                arena: self,
                range: range.clone(),
            }),
            InternalValue::Array(range) => Value::Array(Array {
                arena: self,
                range: range.clone(),
            }),
        };

        TrackedValue {
            syntax: val.syntax,
            data,
        }
    }
}
