//! Defines intermediate representation of values.
//!
//! ## Querying
//!
//! Values are stored in [`ValueArena`]. Composite values are exposed through
//! [`ArrayView`](crate::spec::view::value::ArrayView) and
//! [`ObjectView`](crate::spec::view::value::ObjectView), which expose
//! iterators over items and fields.
//!
//! ## Construction
//!
//! Construction is handled by [`Analyzer`](crate::analyzer::Analyzer). It builds the internal
//! arenas and performs validation before data is exposed through
//! [`Spec`](crate::spec::Spec) views.

use crate::text::{NameId, StringId};

/// Non-composite value
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum PrimitiveValue {
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
pub(crate) struct ArrayBuilder<'p> {
    data: &'p mut Vec<Slot>,

    /// Optionally clean up the data if object is not finished successfully.
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
    pub(crate) fn add_primitive(&mut self, value: PrimitiveValue) -> ValueId {
        let idx = self.data.len();

        self.data.push(value.into());

        ValueId(idx as u32)
    }

    /// Insert an array as the next element of this array.
    pub(crate) fn start_array<'a>(&'a mut self) -> ArrayBuilder<'a> {
        ArrayBuilder::new(self.data, None)
    }

    /// Insert an object as the next element of this array.
    pub(crate) fn start_object<'a>(&'a mut self) -> ObjectBuilder<'a> {
        ObjectBuilder::new(self.data, None)
    }

    /// Constructs the final array.
    ///
    /// Returns the id of the final array in the arena.
    pub(crate) fn finish(mut self) -> ValueId {
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
pub(crate) struct ObjectBuilder<'p> {
    data: &'p mut Vec<Slot>,

    /// Optionally clean up the data if object is not finished successfully.
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
    pub(crate) fn add_primitive(&mut self, name: NameId, value: PrimitiveValue) -> ObjectFieldId {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::ObjectField(name));
        self.data.push(value.into());

        ObjectFieldId {
            object_id: self.start,
            slot_idx,
        }
    }

    /// Add a new field to the object with object value.
    pub(crate) fn start_object<'a>(
        &'a mut self,
        name: NameId,
    ) -> (ObjectBuilder<'a>, ObjectFieldId) {
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
    pub(crate) fn start_array<'a>(&'a mut self, name: NameId) -> (ArrayBuilder<'a>, ObjectFieldId) {
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
    pub(crate) fn finish(mut self) -> ValueId {
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
pub(crate) struct ValueId(u32);

/// Id of an object field.
///
/// Used for identifying a specific field inside an object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ObjectFieldId {
    object_id: ValueId,

    /// Index of the slot in the arena
    slot_idx: u32,
}

impl ObjectFieldId {
    /// Get [`ValueId`] of the object that contains this field.
    #[expect(dead_code, reason = "Should be used later on by semantic query APIs")]
    pub(crate) fn object_id(&self) -> ValueId {
        self.object_id
    }

    /// Get [`ValueId`] of the field's value.
    #[expect(dead_code, reason = "Should be used later on by semantic query APIs")]
    pub(crate) fn value_id(&self) -> ValueId {
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
    // The following slot stores the field value.
    ObjectField(NameId),
}

impl From<PrimitiveValue> for Slot {
    fn from(value: PrimitiveValue) -> Self {
        Self::Primitive(value)
    }
}

/// Range of array in the arena
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ArrayRange {
    first: ValueId,
    end: ValueId,
}

/// Range of an object in the arena
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ObjectRange {
    object_id: ValueId,
    first: u32,
    end: u32,
}

/// Raw data field in arena.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ValueData {
    Primitive(PrimitiveValue),
    Array(ArrayRange),
    Object(ObjectRange),
}

impl ValueData {
    fn from_slot(slot: &Slot, id: ValueId) -> Self {
        match slot {
            Slot::Primitive(val) => Self::Primitive(*val),
            Slot::Array { end } => Self::Array(ArrayRange {
                first: ValueId(id.0 + 1),
                end: *end,
            }),
            Slot::Object { end } => Self::Object(ObjectRange {
                object_id: id,
                first: id.0 + 1,
                end: end.0,
            }),
            Slot::ObjectField(_) => {
                unreachable!("invalid conversion of Slot::ObjectField to ValueData")
            }
        }
    }
}

/// Cursor used for iterating through an array in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ArrayCursor {
    next: ValueId,
    end: ValueId,
}

/// Cursor used for iterating through an object in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ObjectCursor {
    object_id: ValueId,
    next: u32,
    end: u32,
}

/// Data of an object field
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ObjectFieldData {
    pub id: ObjectFieldId,
    pub name: NameId,
    pub value_id: ValueId,
}

/// Arena that holds the values.
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct ValueArena {
    data: Vec<Slot>,
}

impl ValueArena {
    /// Add primitive value to arena.
    pub(crate) fn add_primitive(&mut self, value: PrimitiveValue) -> ValueId {
        let idx = self.data.len();
        self.data.push(value.into());
        ValueId(idx as u32)
    }

    /// Insert an object into the arena.
    pub(crate) fn start_object<'a>(&'a mut self) -> ObjectBuilder<'a> {
        ObjectBuilder::new(&mut self.data, None)
    }

    /// Insert an array into the arena.
    pub(crate) fn start_array<'a>(&'a mut self) -> ArrayBuilder<'a> {
        ArrayBuilder::new(&mut self.data, None)
    }

    /// Get value in the arena
    pub(crate) fn get_value(&self, id: ValueId) -> ValueData {
        ValueData::from_slot(&self.data[id.0 as usize], id)
    }

    /// Gets cursor pointing to the first element in the array
    pub(crate) fn array_cursor(&self, array: ArrayRange) -> ArrayCursor {
        ArrayCursor {
            next: array.first,
            end: array.end,
        }
    }

    /// Get cursor pointing to the first field in the object
    pub(crate) fn object_cursor(&self, object: ObjectRange) -> ObjectCursor {
        ObjectCursor {
            object_id: object.object_id,
            next: object.first,
            end: object.end,
        }
    }

    /// Get next item in the array.
    ///
    /// If cursor is pointing to the end of the array, None is returned.
    pub(crate) fn next_array_item(&self, c: ArrayCursor) -> Option<(ValueId, ArrayCursor)> {
        if c.next.0 >= c.end.0 {
            return None;
        }

        let next_id = match &self.data[c.next.0 as usize] {
            Slot::Array { end } | Slot::Object { end } => *end,
            _ => ValueId(c.next.0 + 1),
        };

        let next_cursor = ArrayCursor {
            next: next_id,
            end: c.end,
        };

        Some((c.next, next_cursor))
    }

    pub(crate) fn next_object_field(
        &self,
        c: ObjectCursor,
    ) -> Option<(ObjectFieldData, ObjectCursor)> {
        if c.next >= c.end {
            return None;
        }

        let name_slot = &self.data[c.next as usize];
        let name_id = match name_slot {
            Slot::ObjectField(name) => *name,
            val => unreachable!(
                "invalid object field in arena for id {:?}: {:?}",
                c.next, val
            ),
        };

        let value_slot = &self.data[c.next as usize + 1];
        let next_id = match value_slot {
            Slot::Array { end } | Slot::Object { end } => end.0,
            _ => c.next + 2, // We need to skip field name and field value
        };

        let next_cursor = ObjectCursor {
            object_id: c.object_id,
            next: next_id,
            end: c.end,
        };
        let data = ObjectFieldData {
            id: ObjectFieldId {
                object_id: c.object_id,
                slot_idx: c.next,
            },
            name: name_id,
            value_id: ValueId(c.next + 1),
        };

        Some((data, next_cursor))
    }
}

#[cfg(test)]
mod test {
    use super::{ObjectFieldId, PrimitiveValue, Slot, ValueData, ValueId};
    use crate::{spec::Spec, text::NameId};

    #[test]
    fn builds_values_and_walks_ranges_and_cursors() {
        let mut spec = Spec::new_test();

        let done = spec.strings.get_or_intern("done");
        let flag_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("flag")) };
        let items_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("items")) };
        let meta_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("meta")) };
        let label_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("label")) };
        let ok_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("ok")) };
        let unused_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("unused")) };

        let null_id = spec.values.add_primitive(PrimitiveValue::Null);
        let bool_id = spec.values.add_primitive(PrimitiveValue::Bool(true));

        let mut root = spec.values.start_object();
        let flag_field_id = root.add_primitive(flag_name, PrimitiveValue::Integer(7));

        let (mut items, items_field_id) = root.start_array(items_name);
        let item_0 = items.add_primitive(PrimitiveValue::Integer(1));
        let mut nested_item_object = items.start_object();
        let nested_item_object_field_id =
            nested_item_object.add_primitive(ok_name, PrimitiveValue::Bool(true));
        let nested_item_object_id = nested_item_object.finish();
        let items_id = items.finish();

        {
            let (mut dropped, _) = root.start_object(unused_name);
            dropped.add_primitive(unused_name, PrimitiveValue::Null);
        }

        let (mut meta, meta_field_id) = root.start_object(meta_name);
        let meta_label_field_id = meta.add_primitive(label_name, PrimitiveValue::String(done));
        let meta_id = meta.finish();
        let root_id = root.finish();

        let expected_slots = vec![
            Slot::Primitive(PrimitiveValue::Null),
            Slot::Primitive(PrimitiveValue::Bool(true)),
            Slot::Object { end: ValueId(15) },
            Slot::ObjectField(flag_name),
            Slot::Primitive(PrimitiveValue::Integer(7)),
            Slot::ObjectField(items_name),
            Slot::Array { end: ValueId(11) },
            Slot::Primitive(PrimitiveValue::Integer(1)),
            Slot::Object { end: ValueId(11) },
            Slot::ObjectField(ok_name),
            Slot::Primitive(PrimitiveValue::Bool(true)),
            Slot::ObjectField(meta_name),
            Slot::Object { end: ValueId(15) },
            Slot::ObjectField(label_name),
            Slot::Primitive(PrimitiveValue::String(done)),
        ];
        assert_eq!(spec.values.data, expected_slots);

        assert_eq!(
            spec.values.get_value(null_id),
            ValueData::Primitive(PrimitiveValue::Null)
        );
        assert_eq!(
            spec.values.get_value(bool_id),
            ValueData::Primitive(PrimitiveValue::Bool(true))
        );

        let root_range = match spec.values.get_value(root_id) {
            ValueData::Object(range) => range,
            other => panic!("expected object range for root, got {other:?}"),
        };
        assert_eq!(root_range.first, 3);
        assert_eq!(root_range.end, 15);

        let root_cursor = spec.values.object_cursor(root_range);
        let (flag_field, next_root_cursor) = spec
            .values
            .next_object_field(root_cursor)
            .expect("root flag field");
        assert_eq!(flag_field.id, flag_field_id);
        assert_eq!(flag_field.name, flag_name);
        assert_eq!(flag_field.value_id, ValueId(4));

        let (items_field, next_root_cursor) = spec
            .values
            .next_object_field(next_root_cursor)
            .expect("root items field");
        assert_eq!(items_field.id, items_field_id);
        assert_eq!(items_field.name, items_name);
        assert_eq!(items_field.value_id, items_id);

        let items_range = match spec.values.get_value(items_field.value_id) {
            ValueData::Array(range) => range,
            other => panic!("expected array range for items, got {other:?}"),
        };
        assert_eq!(items_range.first, ValueId(7));
        assert_eq!(items_range.end, ValueId(11));

        let items_cursor = spec.values.array_cursor(items_range);
        let (first_item_id, next_items_cursor) = spec
            .values
            .next_array_item(items_cursor)
            .expect("first array item");
        assert_eq!(first_item_id, item_0);

        let (second_item_id, next_items_cursor) = spec
            .values
            .next_array_item(next_items_cursor)
            .expect("second array item");
        assert_eq!(second_item_id, nested_item_object_id);
        assert!(spec.values.next_array_item(next_items_cursor).is_none());

        let nested_item_range = match spec.values.get_value(second_item_id) {
            ValueData::Object(range) => range,
            other => panic!("expected object range for nested array item, got {other:?}"),
        };
        let nested_cursor = spec.values.object_cursor(nested_item_range);
        let (nested_field, nested_cursor) = spec
            .values
            .next_object_field(nested_cursor)
            .expect("nested object field");
        assert_eq!(nested_field.id, nested_item_object_field_id);
        assert_eq!(nested_field.value_id, ValueId(10));
        assert!(spec.values.next_object_field(nested_cursor).is_none());

        let (meta_field, next_root_cursor) = spec
            .values
            .next_object_field(next_root_cursor)
            .expect("root meta field");
        assert_eq!(meta_field.id, meta_field_id);
        assert_eq!(meta_field.value_id, meta_id);
        assert!(spec.values.next_object_field(next_root_cursor).is_none());

        let meta_range = match spec.values.get_value(meta_id) {
            ValueData::Object(range) => range,
            other => panic!("expected object range for meta, got {other:?}"),
        };
        let meta_cursor = spec.values.object_cursor(meta_range);
        let (meta_label_field, meta_cursor) = spec
            .values
            .next_object_field(meta_cursor)
            .expect("meta label field");
        assert_eq!(meta_label_field.id, meta_label_field_id);
        assert_eq!(meta_label_field.name, label_name);
        assert_eq!(
            spec.values.get_value(meta_label_field.value_id),
            ValueData::Primitive(PrimitiveValue::String(done))
        );
        assert!(spec.values.next_object_field(meta_cursor).is_none());

        assert_eq!(
            flag_field_id,
            ObjectFieldId {
                object_id: root_id,
                slot_idx: 3,
            }
        );
    }
}
