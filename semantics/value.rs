//! Defines semantic representation of values.

use std::ops::Range;

use crate::Tracked;
use crate::name::Name;

/// Representation of a value.
pub enum Value<'s> {
    Null,
    String(&'s str),
    Bool(bool),
    Integer(i128), // i128 is large enough for i64 and u64
    Float(f64),
    Object(Range<ObjectFieldId>),
    Array(Range<ValueId>),
}

/// A field inside of the object.
pub struct ObjectField<'a> {
    pub name: &'a Name,
    pub value: ValueId,
}

/// Object field together with a pointer to syntax node.
pub type TrackedObjectField<'a> = Tracked<ObjectField<'a>>;

/// Value together with a pointer to syntax node.
pub type TrackedValue<'a> = Tracked<Value<'a>>;

/// Id of the value in the [`ValueArena`] .
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ValueId(u32);

/// Id of the object field in the [`ValueArena`] .
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ObjectFieldId(u32);

/// Arena that holds the values.
#[derive(Default)]
pub struct ValueArena<'a> {
    values: Vec<TrackedValue<'a>>,
    fields: Vec<TrackedObjectField<'a>>,
}

impl<'a> ValueArena<'a> {
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
}
