//! Defines semantic representation of types.
//!
//! The main data structure is a [`TypeArena`] that holds semantic types
//! resolved from syntax. Types in the arena are referenced with [`TypeId`].
//!
//! ## Building An Arena
//!
//! [Primitive types](PrimitiveType) can be added directly with
//! [`TypeArena::add_primitive`]. Composite types (records and unions) are built
//! through the builder API returned by [`TypeArena::start_record`] and
//! [`TypeArena::start_union`]. Builders allow us to build the composite types
//! without additional heap allocations.
//!
//! ## Getting Types
//!
//! To retrieve a type, pass a [`TypeId`] to [`TypeArena::get`]. This returns a [`Type`].

use crate::spec::value;
use crate::string::StringId;

/// Representation of a type.
#[derive(Debug, Clone, derive_more::Display)]
pub enum Type<'a> {
    #[display("`i32`")]
    I32,
    #[display("`i64`")]
    I64,
    #[display("`u32`")]
    U32,
    #[display("`u64`")]
    U64,
    #[display("`f32`")]
    F32,
    #[display("`f64`")]
    F64,
    #[display("`date`")]
    Date,
    #[display("`timestamp`")]
    Timestamp,
    #[display("`bool`")]
    Bool,
    #[display("`string`")]
    String,
    #[display("`file`")]
    File,
    #[display("`option`")]
    Option(Reference<'a>),
    #[display("`array`")]
    Array(Reference<'a>),
    #[display("`record`")]
    Record(Record<'a>),
    #[display("`union`")]
    Union(Union<'a>),
    #[display("`enum`")]
    Enum(Enum<'a>),
    #[display("`response`")]
    Response(Response<'a>),

    /// Reference to a named type.
    #[display("`reference`")]
    Reference(StringId),
}

impl From<PrimitiveType> for Type<'_> {
    fn from(value: PrimitiveType) -> Self {
        match value {
            PrimitiveType::I32 => Type::I32,
            PrimitiveType::I64 => Type::I64,
            PrimitiveType::U32 => Type::U32,
            PrimitiveType::U64 => Type::U64,
            PrimitiveType::F32 => Type::F32,
            PrimitiveType::F64 => Type::F64,
            PrimitiveType::Date => Type::Date,
            PrimitiveType::Timestamp => Type::Timestamp,
            PrimitiveType::Bool => Type::Bool,
            PrimitiveType::String => Type::String,
            PrimitiveType::File => Type::File,
            PrimitiveType::Reference(name) => Type::Reference(name),
        }
    }
}

impl<'a> Type<'a> {
    fn from_slot(arena: &'a TypeArena, id: TypeId, slot: &'a Slot) -> Self {
        match slot {
            Slot::Primitive(simple) => (*simple).into(),
            Slot::Array { .. } => Type::Array(Reference {
                arena,
                id: TypeId(id.0 + 1),
            }),
            Slot::Option { .. } => Type::Option(Reference {
                arena,
                id: TypeId(id.0 + 1),
            }),
            Slot::Enum { typ, values } => Type::Enum(Enum {
                typ: Reference { arena, id: *typ },
                values: *values,
            }),
            Slot::Response {
                body,
                headers,
                content_type,
            } => Type::Response(Response {
                body: Reference { arena, id: *body },
                headers: headers.map(|id| Reference { arena, id }),
                content_type: *content_type,
            }),

            Slot::Record { end } => Type::Record(Record {
                id,
                fields: TypeFieldIterator {
                    arena,
                    current: TypeId(id.0 + 1),
                    end: *end,
                    id,
                },
            }),
            Slot::Union { discriminator, end } => Type::Union(Union {
                id,
                disriminator: *discriminator,
                fields: TypeFieldIterator {
                    arena,
                    current: TypeId(id.0 + 1),
                    end: *end,
                    id,
                },
            }),
            Slot::TypeField { .. } => unreachable!("invalid conversion of Slot::TypeField to Type"),
        }
    }
}

/// Reference to a type stored in the [`TypeArena`].
#[derive(derive_more::Debug, Clone)]
pub struct Reference<'a> {
    /// Id of the type in the arena.
    pub id: TypeId,

    #[debug(skip)]
    arena: &'a TypeArena,
}

impl<'a> Reference<'a> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> Type<'a> {
        self.arena.get(self.id)
    }
}

/// Field exposed by a record or union.
///
/// Default is stored in the [`value::ValueArena`].
pub struct TypeField<'a> {
    pub id: TypeFieldId,
    pub name: StringId,
    pub default: Option<value::ValueId>,
    pub typ: Type<'a>,
}

/// Iterator returned for composite types.
///
/// Each item is a [`TypeField`].
#[derive(derive_more::Debug, Clone)]
pub struct TypeFieldIterator<'a> {
    #[debug(skip)]
    arena: &'a TypeArena,
    current: TypeId,
    end: TypeId,

    // Id of the record or union
    id: TypeId,
}

impl<'a> Iterator for TypeFieldIterator<'a> {
    type Item = TypeField<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 >= self.end.0 {
            return None;
        }

        let field_id = TypeFieldId {
            container_id: self.id,
            slot_idx: self.current.0,
        };
        let field = self.arena.get_field(field_id);

        let field_type_slot = &self.arena.data[field_id.slot_idx as usize + 1];

        self.current = match &field_type_slot {
            Slot::Option { end } => *end,
            Slot::Array { end } => *end,
            Slot::Primitive(_) => TypeId(self.current.0 + 2),
            typ => unreachable!("invalid type's field type in arena: {typ:?}"),
        };

        Some(field)
    }
}

/// Semantic representation of a record.
///
/// Fields are exposed through [`Record::fields`].
#[derive(Debug, Clone)]
pub struct Record<'a> {
    // Id of the record
    pub id: TypeId,

    fields: TypeFieldIterator<'a>,
}

impl<'a> Record<'a> {
    /// Returns iterator over [record fields](TypeField)
    pub fn fields(&self) -> TypeFieldIterator<'a> {
        self.fields.clone()
    }
}

/// Semantic representation of a tagged union.
///
/// Fields are exposed through [`Union::fields`].
#[derive(Debug, Clone)]
pub struct Union<'a> {
    // Id of the union
    pub id: TypeId,
    pub disriminator: StringId,

    fields: TypeFieldIterator<'a>,
}

impl<'a> Union<'a> {
    /// Returns iterator over [union fields](TypeField)
    pub fn fields(&self) -> TypeFieldIterator<'a> {
        self.fields.clone()
    }
}

/// Semantic representation of an enum.
///
/// Optional `typ` describes the type of enum values, while `values` points to
/// an array in the [`value::ValueArena`].
#[derive(Debug, Clone)]
pub struct Enum<'a> {
    pub typ: Reference<'a>,
    pub values: value::ValueId,
}

/// Semantic information about a response type.
#[derive(Debug, Clone)]
pub struct Response<'a> {
    /// Type of response body
    pub body: Reference<'a>,

    /// Optional headers
    pub headers: Option<Reference<'a>>,

    /// Possible Content-Type header values.
    pub content_type: Option<value::MimeTypesId>,
}

/// Id of a type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

/// Id of a field in a record or union.
///
/// Used for getting specific [`TypeField`] with [`TypeArena::get_field`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeFieldId {
    container_id: TypeId,

    /// Index of the slot in the arena
    slot_idx: u32,
}

impl TypeFieldId {
    /// Get [`TypeId`] of the record or union that contains this field.
    pub fn container_id(&self) -> TypeId {
        self.container_id
    }

    /// Get [`TypeId`] of the field's type.
    pub fn type_id(&self) -> TypeId {
        TypeId(self.slot_idx + 1)
    }
}

/// Simple types.
///
/// These types can be constructed without using a builder.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    Date,
    Timestamp,
    Bool,
    String,
    File,
    Reference(StringId),
}

/// Slot in the type arena.
///
/// Most of the representations are obvious, the two notable that deserve a separate
/// comment are Array and Option. Array and Option can be intertwined to form a complex
/// nested type (ie. `[[[i32?]]?]?`). They can also be nested this way inside Record or Union
/// field, which means we need an efficient way to skip the whole composition.
///
/// Array and Object work by having the next type in the arena be the type wrapped in them.
/// For instance `[i32]` in arena is represented as `[Slot::Array, Slot::Primitive(_)]`
/// This allows us to nest stuff, like example above:
/// ```text
/// [Slot::Option, Slot::Array, Slot::Option, Slot::Array, Slot::Array, Slot::Option,
/// Slot::Primitive(_)]
/// ```
/// The `end` property of Array and Option represent the TypeId of the type after the whole nesting
/// and can be used to skip the whole nested type when doing field iteration.
#[derive(Debug, Clone, PartialEq)]
enum Slot {
    Primitive(PrimitiveType),
    Option {
        // Id after the last type in the nested Option<...> type.
        // Used for skipping the whole Option during iteration.
        end: TypeId,
    },
    Array {
        // Id after the last type in the nested Array<...> type.
        // Used for skipping the whole Array during iteration.
        end: TypeId,
    },
    Enum {
        typ: TypeId,
        values: value::ValueId,
    },
    Response {
        body: TypeId,
        headers: Option<TypeId>,
        content_type: Option<value::MimeTypesId>,
    },
    Record {
        // Id after the last field in the record.
        // Used for skipping the whole record during iteration.
        end: TypeId,
    },
    Union {
        discriminator: StringId,
        // Id after the last field in the union.
        // Used for skipping the whole record during iteration.
        end: TypeId,
    },
    // Name of the field in record or union.
    // In TrackedSlot syntax pointer points to the whole field
    // and not just the name.
    TypeField {
        name: StringId,
        default: Option<value::ValueId>,
    },
}

impl From<PrimitiveType> for Slot {
    fn from(value: PrimitiveType) -> Self {
        Self::Primitive(value)
    }
}

/// Helper type for adding records and unions to arena.
///
/// Constructed via [`TypeArena::start_record`] or [`TypeArena::start_union`].
/// Call [`finish`](FieldBuilder::finish) once all fields are added.
///
/// Dropping the builder without finishing rolls back any changes.
pub struct FieldBuilder<'p> {
    data: &'p mut Vec<Slot>,

    /// Index in the arena that contains Slot::Record or Slot::Union.
    start: TypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'p> FieldBuilder<'p> {
    fn new_record(arena: &'p mut TypeArena) -> Self {
        let idx = arena.data.len();
        arena.data.push(Slot::Record { end: TypeId(0) });

        Self {
            data: &mut arena.data,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    fn new_union(arena: &'p mut TypeArena, discriminator: StringId) -> Self {
        let idx = arena.data.len();
        arena.data.push(Slot::Union {
            discriminator,
            end: TypeId(0),
        });

        Self {
            data: &mut arena.data,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    /// Add a new field with a primitive type.
    ///
    /// `default` is a default value stored in the [`value::ValueArena`].
    pub fn add_primitive(
        &mut self,
        name: StringId,
        typ: PrimitiveType,
        default: Option<value::ValueId>,
    ) -> TypeFieldId {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField { name, default });
        self.data.push(typ.into());

        TypeFieldId {
            container_id: self.start,
            slot_idx,
        }
    }

    /// Add a new field of array type.
    ///
    /// Returns array builder and [`TypeFieldId`] of the created field.
    /// If returned builder, this builder or any parent builder is dropped before
    /// calling `.finish()` on it, the returned [`TypeFieldId`] will be invalid!
    pub fn start_array<'a>(
        &'a mut self,
        name: StringId,
        default: Option<value::ValueId>,
    ) -> (OptionArrayBuilder<'a>, TypeFieldId) {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField { name, default });

        let field_id = TypeFieldId {
            container_id: self.start,
            slot_idx,
        };
        let builder = OptionArrayBuilder::new_array(self.data, Some(slot_idx));

        (builder, field_id)
    }

    /// Add a new field of option type.
    ///
    /// Returns option builder and [`TypeFieldId`] of the created field.
    /// If returned builder, this builder or any parent builder is dropped before
    /// calling `.finish()` on it, the returned [`TypeFieldId`] will be invalid!
    pub fn start_option<'a>(
        &'a mut self,
        name: StringId,
        default: Option<value::ValueId>,
    ) -> (OptionArrayBuilder<'a>, TypeFieldId) {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField { name, default });

        let field_id = TypeFieldId {
            container_id: self.start,
            slot_idx,
        };
        let builder = OptionArrayBuilder::new_option(self.data, Some(slot_idx));

        (builder, field_id)
    }

    /// Finalize the record or union currently being built.
    ///
    /// Returns the id of the composite type inside the arena.
    pub fn finish(mut self) -> TypeId {
        self.finished = true;

        let idx = TypeId(self.data.len() as u32);

        let start = self.start.0 as usize;
        let head = &mut self.data[start];
        match head {
            Slot::Record { end } => *end = idx,
            Slot::Union { end, .. } => *end = idx,
            _ => unreachable!("invalid FieldBuilder start"),
        }

        self.start
    }
}

impl<'a> Drop for FieldBuilder<'a> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let len = self.start.0 as usize;
        self.data.truncate(len);
    }
}

/// Helper type for adding Array and Option type to arena.
///
/// This type is constructed with [`TypeArena::start_array`],
/// [`TypeArena::start_option`] or [`FieldBuilder`] equivalent.
/// After you are done, you should call [`finish`](OptionArrayBuilder::finish),
/// which returns the id of the final type in the arena.
///
/// If builder is dropped before calling finish, added types are removed from the arena.
pub struct OptionArrayBuilder<'p> {
    data: &'p mut Vec<Slot>,

    /// Optionally clean up the data if array is not finished successfully.
    truncate: Option<u32>,

    /// Index in the arena that contains the first Slot::Array or Slot::Option.
    start: TypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

/// Result returned when [`OptionArrayBuilder`] is finished.
pub struct OptionArray {
    /// Id of the final primitive type inserted into the builder.
    pub primitive_id: TypeId,

    /// Id of the constructed Option or Array.
    pub container_id: TypeId,
}

impl<'p> OptionArrayBuilder<'p> {
    fn new_array(data: &'p mut Vec<Slot>, truncate: Option<u32>) -> Self {
        let idx = data.len();
        data.push(Slot::Array { end: TypeId(0) });

        Self {
            data,
            truncate,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    fn new_option(data: &'p mut Vec<Slot>, truncate: Option<u32>) -> Self {
        let idx = data.len();
        data.push(Slot::Option { end: TypeId(0) });

        Self {
            data,
            truncate,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    /// Start a new array inside the current array or option.
    ///
    /// Returns id of the array type that was started. If the builder
    /// is not finished, the returned id is invalid.
    pub fn start_array(&mut self) -> TypeId {
        let idx = self.data.len();
        self.data.push(Slot::Array { end: TypeId(0) });
        TypeId(idx as u32)
    }

    /// Start a new option inside the current array or option.
    ///
    /// Returns id of the option type that was started. If the builder
    /// is not finished, the returned id is invalid.
    pub fn start_option(&mut self) -> TypeId {
        let idx = self.data.len();
        self.data.push(Slot::Option { end: TypeId(0) });
        TypeId(idx as u32)
    }

    /// Finish building this type.
    pub fn finish(mut self, typ: PrimitiveType) -> OptionArray {
        self.finished = true;

        let start = self.start.0 as usize;
        let len = self.data.len();
        let end_id = TypeId(len as u32 + 1);

        // Update the in between types
        for slot in &mut self.data[start..len] {
            match slot {
                Slot::Option { end } => *end = end_id,
                Slot::Array { end } => *end = end_id,
                _ => unreachable!(),
            }
        }

        self.data.push(typ.into());

        OptionArray {
            primitive_id: TypeId(len as u32),
            container_id: self.start,
        }
    }
}

impl<'p> Drop for OptionArrayBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        // Truncate the builder to `truncate` parameter, or self start.
        let len = self.truncate.unwrap_or(self.start.0);
        self.data.truncate(len as usize);
    }
}

/// Arena that holds semantic types.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct TypeArena {
    data: Vec<Slot>,
}

impl TypeArena {
    /// Create a new type arena.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a [`Type`] by id.
    pub fn get<'a>(&'a self, id: TypeId) -> Type<'a> {
        let typ = &self.data[id.0 as usize];
        Type::from_slot(self, id, typ)
    }

    /// Get [`TypeField`] by id.
    pub fn get_field<'a>(&'a self, id: TypeFieldId) -> TypeField<'a> {
        let field_slot = &self.data[id.slot_idx as usize];
        let typ_slot = &self.data[id.slot_idx as usize + 1];

        let (name, default) = match &field_slot {
            Slot::TypeField { name, default } => (*name, *default),
            val => unreachable!("invalid type field in arena for id {id:?}: {val:?}"),
        };

        let typ = Type::from_slot(self, TypeId(id.slot_idx + 1), typ_slot);

        TypeField {
            id,
            name,
            default,
            typ,
        }
    }

    /// Add a primitive type to the arena.
    ///
    /// Returns the [`TypeId`] assigned to the new type.
    pub fn add_primitive(&mut self, typ: PrimitiveType) -> TypeId {
        let idx = self.data.len();
        self.data.push(typ.into());
        TypeId(idx as u32)
    }

    /// Add an enum to the arena.
    ///
    /// - `typ` is the type of the values in the enum.
    /// - `values` should be a [`ValueId`](value::ValueId) pointing to array of possible values for
    ///   this arena.
    ///
    /// Returns the [`TypeId`] assigned to the enum.
    pub fn add_enum(&mut self, typ: TypeId, values: value::ValueId) -> TypeId {
        let idx = self.data.len();
        self.data.push(Slot::Enum { typ, values });
        TypeId(idx as u32)
    }

    /// Add a response to the arena.
    ///
    /// - `body` is the type of the response body.
    /// - `headers` is the type of the headers.
    /// - `content_type` should be a [`ValueId`](value::ValueId) pointing to a string defining the
    ///   content type.
    ///
    /// Returns the [`TypeId`] assigned to the response.
    pub fn add_response(
        &mut self,
        body: TypeId,
        headers: Option<TypeId>,
        content_type: Option<value::MimeTypesId>,
    ) -> TypeId {
        let idx = self.data.len();
        self.data.push(Slot::Response {
            body,
            headers,
            content_type,
        });
        TypeId(idx as u32)
    }

    /// Start building a record type.
    ///
    /// Returns a [`FieldBuilder`] rooted at the new record slot.
    pub fn start_record<'a>(&'a mut self) -> FieldBuilder<'a> {
        FieldBuilder::new_record(self)
    }

    /// Start building a union type.
    ///
    /// Returns a [`FieldBuilder`] configured with the provided discriminator.
    pub fn start_union<'a>(&'a mut self, discriminator: StringId) -> FieldBuilder<'a> {
        FieldBuilder::new_union(self, discriminator)
    }

    /// Add array to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested array.
    pub fn start_array<'a>(&'a mut self) -> OptionArrayBuilder<'a> {
        OptionArrayBuilder::new_array(&mut self.data, None)
    }

    /// Add option to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested option.
    pub fn start_option<'a>(&'a mut self) -> OptionArrayBuilder<'a> {
        OptionArrayBuilder::new_option(&mut self.data, None)
    }
}

// #[cfg(test)]
// mod test {
//     use crate::string::StringInterner;
//     use crate::typ::{PrimitiveType, Type, TypeArena, TypeFieldId, TypeId};
//
//     use super::Slot;
//
//     #[test]
//     fn builds_nested_types() {
//         let mut arena = TypeArena::new();
//
//         // Insert some strings
//         let mut interner = StringInterner::default();
//         let id_str = interner.insert("id");
//         let simple_array_str = interner.insert("simple_array");
//         let values_str = interner.insert("values");
//         let metadata_str = interner.insert("metadata");
//         let unused_str = interner.insert("unused");
//         let success_str = interner.insert("success");
//         let error_str = interner.insert("error");
//
//         // Add some primitive types
//         let i32_id = arena.add_primitive(PrimitiveType::I32);
//         let string_id = arena.add_primitive(PrimitiveType::String);
//         let bool_id = arena.add_primitive(PrimitiveType::Bool);
//
//         // Build a complex nested type structure:
//         // record Root {
//         //   id: i64,
//         //   simple_array: [f32],
//         //   values: [[[string?]]],
//         //   metadata: [[bool]?]
//         // }
//         let mut root = arena.start_record();
//         root.add_primitive(id_str, PrimitiveType::I64, None);
//
//         let (simple_array, simple_array_field_id) = root.start_array(simple_array_str, None);
//         simple_array.finish(PrimitiveType::F32);
//
//         // Build nested array type: [[[string?]]]
//         let (mut values_builder, values_field_id) = root.start_array(values_str, None);
//         values_builder.start_array();
//         values_builder.start_array();
//         values_builder.start_option();
//         let values_id = values_builder.finish(PrimitiveType::String);
//
//         // Test dropped builder (should not appear in root)
//         {
//             let _ = root.start_array(unused_str, None);
//             // Dropped without finish
//         }
//
//         // Build nested array with option: [[bool]?]
//         let (mut metadata_builder, metadata_field_id) = root.start_array(metadata_str, None);
//         metadata_builder.start_array();
//         metadata_builder.start_option();
//         metadata_builder.finish(PrimitiveType::Bool);
//
//         let root_id = root.finish();
//
//         // Test dropped builder (should not appear in arena)
//         {
//             let mut dropped_union = arena.start_union(None);
//             dropped_union.add_primitive(unused_str, PrimitiveType::I32, None);
//             // Dropped without finish
//         }
//
//         // Build a union type separately
//         let mut union_builder = arena.start_union(None);
//         union_builder.add_primitive(success_str, PrimitiveType::Bool, None);
//         union_builder.add_primitive(error_str, PrimitiveType::String, None);
//         let union_id = union_builder.finish();
//
//         // Verify the arena structure
//         let expected_slots = vec![
//             Slot::Primitive(PrimitiveType::I32),
//             Slot::Primitive(PrimitiveType::String),
//             Slot::Primitive(PrimitiveType::Bool),
//             Slot::Record { end: TypeId(20) },
//             Slot::TypeField {
//                 name: id_str,
//                 default: None,
//             },
//             Slot::Primitive(PrimitiveType::I64),
//             Slot::TypeField {
//                 name: simple_array_str,
//                 default: None,
//             },
//             Slot::Array { end: TypeId(9) },
//             Slot::Primitive(PrimitiveType::F32),
//             Slot::TypeField {
//                 name: values_str,
//                 default: None,
//             },
//             Slot::Array { end: TypeId(15) },
//             Slot::Array { end: TypeId(15) },
//             Slot::Array { end: TypeId(15) },
//             Slot::Option { end: TypeId(15) },
//             Slot::Primitive(PrimitiveType::String),
//             Slot::TypeField {
//                 name: metadata_str,
//                 default: None,
//             },
//             Slot::Array { end: TypeId(20) },
//             Slot::Array { end: TypeId(20) },
//             Slot::Option { end: TypeId(20) },
//             Slot::Primitive(PrimitiveType::Bool),
//             Slot::Union {
//                 discriminator: None,
//                 end: TypeId(25),
//             },
//             Slot::TypeField {
//                 name: success_str,
//                 default: None,
//             },
//             Slot::Primitive(PrimitiveType::Bool),
//             Slot::TypeField {
//                 name: error_str,
//                 default: None,
//             },
//             Slot::Primitive(PrimitiveType::String),
//         ];
//
//         assert!(arena.data.len() == expected_slots.len());
//         for (idx, expected) in expected_slots.iter().enumerate() {
//             assert!(&arena.data[idx] == expected, "slot mismatch at index {idx}");
//         }
//
//         // Test getting primitive types
//         assert!(matches!(arena.get(i32_id), Type::I32));
//         assert!(matches!(arena.get(string_id), Type::String));
//         assert!(matches!(arena.get(bool_id), Type::Bool));
//
//         // Test getting the root record
//         let root_type = arena.get(root_id);
//         let root_record = match root_type {
//             Type::Record(record) => record,
//             other => panic!("expected record at root_id, got {other:?}"),
//         };
//
//         let mut root_record_fields = root_record.fields();
//
//         // Check id field
//         let id_field = root_record_fields.next().expect("id field");
//         assert!(id_field.name == id_str);
//         assert!(matches!(id_field.typ, Type::I64));
//         assert!(id_field.default.is_none());
//
//         // Check simple_array field
//         let simple_array_field = root_record_fields.next().expect("simple_array field");
//         assert!(simple_array_field.name == simple_array_str);
//         match simple_array_field.typ {
//             Type::Array(ref inner) => {
//                 assert!(matches!(inner.typ(), Type::F32));
//             }
//             other => panic!("expected array type, got {other:?}"),
//         }
//
//         // Check values field with nested arrays
//         let values_field = root_record_fields.next().expect("values field");
//         assert!(values_field.name == values_str);
//
//         // Navigate through [[[string?]]]
//         let level1 = match values_field.typ {
//             Type::Array(ref inner) => inner.typ(),
//             other => panic!("expected array at level 1, got {other:?}"),
//         };
//
//         let level2 = match level1 {
//             Type::Array(ref inner) => inner.typ(),
//             other => panic!("expected array at level 2, got {other:?}"),
//         };
//
//         let level3 = match level2 {
//             Type::Array(ref inner) => inner.typ(),
//             other => panic!("expected array at level 3, got {other:?}"),
//         };
//
//         let option_inner = match level3 {
//             Type::Option(ref inner) => inner.typ(),
//             other => panic!("expected option, got {other:?}"),
//         };
//
//         assert!(matches!(option_inner, Type::String));
//
//         // Check metadata field
//         let metadata_field = root_record_fields.next().expect("metadata field");
//         assert!(metadata_field.name == metadata_str);
//
//         assert!(root_record_fields.next().is_none());
//
//         // Test getting the union directly
//         let union_type = arena.get(union_id);
//         let mut union_fields = match union_type {
//             Type::Union(u) => {
//                 assert!(u.disriminator.is_none());
//                 u.fields
//             }
//             other => panic!("expected union at union_id, got {other:?}"),
//         };
//
//         let success_field = union_fields.next().expect("success field");
//         assert!(success_field.name == success_str);
//         assert!(matches!(success_field.typ, Type::Bool));
//
//         let error_field = union_fields.next().expect("error field");
//         assert!(error_field.name == error_str);
//         assert!(matches!(error_field.typ, Type::String));
//
//         assert!(union_fields.next().is_none());
//
//         // Test getting values array type directly
//         let values_type = arena.get(values_id.container_id);
//         match values_type {
//             Type::Array(ref inner) => {
//                 let level1 = inner.typ();
//                 match level1 {
//                     Type::Array(ref inner2) => {
//                         let level2 = inner2.typ();
//                         match level2 {
//                             Type::Array(ref inner3) => {
//                                 let level3 = inner3.typ();
//                                 match level3 {
//                                     Type::Option(ref inner4) => {
//                                         assert!(matches!(inner4.typ(), Type::String));
//                                     }
//                                     other => {
//                                         panic!("expected option at deepest level, got {other:?}")
//                                     }
//                                 }
//                             }
//                             other => panic!("expected array at level 3, got {other:?}"),
//                         }
//                     }
//                     other => panic!("expected array at level 2, got {other:?}"),
//                 }
//             }
//             other => panic!("expected array at values_id, got {other:?}"),
//         }
//
//         assert!(values_id.primitive_id == TypeId(14));
//
//         // Check field ids
//         assert!(
//             simple_array_field_id
//                 == TypeFieldId {
//                     container_id: root_id,
//                     slot_idx: 6,
//                 }
//         );
//
//         assert!(
//             values_field_id
//                 == TypeFieldId {
//                     container_id: root_id,
//                     slot_idx: 9,
//                 }
//         );
//
//         assert!(
//             metadata_field_id
//                 == TypeFieldId {
//                     container_id: root_id,
//                     slot_idx: 15,
//                 }
//         );
//     }
// }
