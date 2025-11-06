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

use crate::{StringId, value};

/// Representation of a type.
#[derive(Debug, PartialEq, derive_more::Display)]
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
    Record(TypeFieldIterator<'a>),
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
                typ: typ.map(|id| Reference { arena, id }),
                values: *values,
            }),
            Slot::Response {
                body,
                headers,
                content_type,
            } => Type::Response(Response {
                body: body.map(|id| Reference { arena, id }),
                headers: headers.map(|id| Reference { arena, id }),
                content_type: *content_type,
            }),

            Slot::Record { end } => Type::Record(TypeFieldIterator {
                arena,
                current: TypeId(id.0 + 1),
                end: *end,
                id,
            }),
            Slot::Union { discriminator, end } => Type::Union(Union {
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
#[derive(derive_more::Debug, PartialEq)]
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
#[derive(derive_more::Debug, PartialEq)]
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
            type_id: self.id,
            slot: self.current.0,
        };
        let field = self.arena.get_field(field_id);

        let field_type_slot = &self.arena.data[field_id.slot as usize + 1];

        self.current = match &field_type_slot {
            Slot::Option { end } => *end,
            Slot::Array { end } => *end,
            Slot::Primitive(_) => TypeId(self.current.0 + 2),
            typ => unreachable!("invalid type's field type in arena: {typ:?}"),
        };

        Some(field)
    }
}

/// Semantic representation of a tagged union.
///
/// Fields are exposed through a [`TypeFieldIterator`].
#[derive(Debug, PartialEq)]
pub struct Union<'a> {
    pub disriminator: Option<StringId>,
    pub fields: TypeFieldIterator<'a>,
}

/// Semantic representation of an enum.
///
/// Optional `typ` describes the type of enum values, while `values` points to
/// an array in the [`value::ValueArena`].
#[derive(Debug, PartialEq)]
pub struct Enum<'a> {
    pub typ: Option<Reference<'a>>,
    pub values: value::ValueId,
}

/// Semantic information about a response type.
#[derive(Debug, PartialEq)]
pub struct Response<'a> {
    pub body: Option<Reference<'a>>,
    pub headers: Option<Reference<'a>>,
    pub content_type: Option<value::ValueId>,
}

/// Id of a type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

/// Id of a field in a record or union.
///
/// Used for getting specific [`TypeField`] with [`TypeArena::get_field`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeFieldId {
    type_id: TypeId,

    /// Index of the slot in the arena
    slot: u32,
}

impl TypeFieldId {
    /// Get [`TypeId`] of the record or union that contains this field.
    pub fn type_id(&self) -> TypeId {
        self.type_id
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
        typ: Option<TypeId>,
        values: value::ValueId,
    },
    Response {
        body: Option<TypeId>,
        headers: Option<TypeId>,
        content_type: Option<value::ValueId>,
    },
    Record {
        // Id after the last field in the record.
        // Used for skipping the whole record during iteration.
        end: TypeId,
    },
    Union {
        discriminator: Option<StringId>,
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

trait BuilderParent {
    fn arena(&mut self) -> &mut TypeArena;

    /// This method is called by a child builder if it's dropped without
    /// being finished successfully.
    ///
    /// For some builders this is a noop.
    fn drop_child(&mut self);

    fn data(&mut self) -> &mut Vec<Slot> {
        &mut self.arena().data
    }
}

/// Helper type for adding records and unions to arena.
///
/// Constructed via [`TypeArena::start_record`] or [`TypeArena::start_union`].
/// Call [`finish`](FieldBuilder::finish) once all fields are added.
///
/// Dropping the builder without finishing rolls back any changes.
pub struct FieldBuilder<'p> {
    arena: &'p mut TypeArena,

    /// Index in the arena that contains Slot::Record or Slot::Union.
    start: TypeId,

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

impl<'p> FieldBuilder<'p> {
    fn new_record(arena: &'p mut TypeArena) -> Self {
        let idx = arena.data.len();
        arena.data.push(Slot::Record { end: TypeId(0) });

        Self {
            arena,
            start: TypeId(idx as u32),
            finished: false,
            last_field_idx: None,
        }
    }

    fn new_union(arena: &'p mut TypeArena, discriminator: Option<StringId>) -> Self {
        let idx = arena.data.len();
        arena.data.push(Slot::Union {
            discriminator,
            end: TypeId(0),
        });

        Self {
            arena,
            start: TypeId(idx as u32),
            finished: false,
            last_field_idx: None,
        }
    }

    fn data(&mut self) -> &mut Vec<Slot> {
        &mut self.arena.data
    }

    /// Add a new field with a simple type.
    ///
    /// `default` is a default value stored in the [`value::ValueArena`].
    pub fn add_simple(
        &mut self,
        name: StringId,
        typ: PrimitiveType,
        default: Option<value::ValueId>,
    ) {
        self.last_field_idx = Some(self.data().len() as u32);

        self.data().push(Slot::TypeField { name, default });
        self.data().push(typ.into());
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
    ) -> OptionArrayBuilder<'a> {
        self.last_field_idx = Some(self.data().len() as u32);

        self.data().push(Slot::TypeField { name, default });

        OptionArrayBuilder::new_array(self)
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
    ) -> OptionArrayBuilder<'a> {
        self.last_field_idx = Some(self.data().len() as u32);

        self.data().push(Slot::TypeField { name, default });

        OptionArrayBuilder::new_option(self)
    }

    /// Finalize the record or union currently being built.
    ///
    /// Returns the id of the composite type inside the arena.
    pub fn finish(mut self) -> TypeId {
        self.finished = true;

        let idx = TypeId(self.data().len() as u32);

        let start = self.start.0 as usize;
        let head = &mut self.data()[start];
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
        self.data().truncate(len);
    }
}

impl<'a> BuilderParent for FieldBuilder<'a> {
    fn arena(&mut self) -> &mut TypeArena {
        self.arena
    }

    fn drop_child(&mut self) {
        // Truncate arena length so that it ends at last inserted field.
        if let Some(len) = self.last_field_idx {
            self.arena().data.truncate(len as usize);
        }
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
    parent: &'p mut dyn BuilderParent,

    /// Index in the arena that contains the first Slot::Array or Slot::Option.
    start: TypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'p> OptionArrayBuilder<'p> {
    fn new_array(parent: &'p mut dyn BuilderParent) -> Self {
        let idx = parent.data().len();
        parent.data().push(Slot::Array { end: TypeId(0) });

        Self {
            parent,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    fn new_option(parent: &'p mut dyn BuilderParent) -> Self {
        let idx = parent.data().len();
        parent.data().push(Slot::Option { end: TypeId(0) });

        Self {
            parent,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    /// Start a new array inside the current array or option.
    pub fn start_array(&mut self) {
        self.parent.data().push(Slot::Array { end: TypeId(0) });
    }

    /// Start a new option inside the current array or option.
    pub fn start_option(&mut self) {
        self.parent.data().push(Slot::Option { end: TypeId(0) });
    }

    /// Finish building this type.
    pub fn finish(mut self, typ: PrimitiveType) -> TypeId {
        self.finished = true;

        let start = self.start.0 as usize;
        let len = self.parent.data().len();
        let end_id = TypeId(len as u32 + 1);

        // Update the in between types
        for slot in &mut self.parent.data()[start..len] {
            match slot {
                Slot::Option { end } => *end = end_id,
                Slot::Array { end } => *end = end_id,
                _ => unreachable!(),
            }
        }

        self.parent.data().push(typ.into());

        self.start
    }
}

impl<'p> Drop for OptionArrayBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let len = self.start.0 as usize;
        self.parent.data().truncate(len);

        self.parent.drop_child();
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
        let field_slot = &self.data[id.slot as usize];
        let typ_slot = &self.data[id.slot as usize + 1];

        let (name, default) = match &field_slot {
            Slot::TypeField { name, default } => (*name, *default),
            val => unreachable!("invalid type field in arena for id {id:?}: {val:?}"),
        };

        let typ = Type::from_slot(self, TypeId(id.slot + 1), typ_slot);

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
    pub fn add_enum(&mut self, typ: Option<TypeId>, values: value::ValueId) -> TypeId {
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
        body: Option<TypeId>,
        headers: Option<TypeId>,
        content_type: Option<value::ValueId>,
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
    pub fn start_union<'a>(&'a mut self, discriminator: Option<StringId>) -> FieldBuilder<'a> {
        FieldBuilder::new_union(self, discriminator)
    }

    /// Add array to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested array.
    pub fn start_array<'a>(&'a mut self) -> OptionArrayBuilder<'a> {
        OptionArrayBuilder::new_array(self)
    }

    /// Add option to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested option.
    pub fn start_option<'a>(&'a mut self) -> OptionArrayBuilder<'a> {
        OptionArrayBuilder::new_option(self)
    }
}

impl BuilderParent for TypeArena {
    fn arena(&mut self) -> &mut TypeArena {
        self
    }

    fn drop_child(&mut self) {
        // Nothing to do
    }
}

#[cfg(test)]
mod test {
    use string_interner::DefaultStringInterner;

    use crate::typ::{PrimitiveType, Type, TypeArena, TypeId};

    use super::Slot;

    #[test]
    fn builds_nested_types() {
        let mut arena = TypeArena::new();

        // Insert some strings
        let mut interner = DefaultStringInterner::default();
        let id_str = interner.get_or_intern("id");
        let simple_array_str = interner.get_or_intern("simple_array");
        let values_str = interner.get_or_intern("values");
        let metadata_str = interner.get_or_intern("metadata");
        let unused_str = interner.get_or_intern("unused");
        let success_str = interner.get_or_intern("success");
        let error_str = interner.get_or_intern("error");

        // Add some primitive types
        let i32_id = arena.add_primitive(PrimitiveType::I32);
        let string_id = arena.add_primitive(PrimitiveType::String);
        let bool_id = arena.add_primitive(PrimitiveType::Bool);

        // Build a complex nested type structure:
        // record Root {
        //   id: i64,
        //   simple_array: [f32],
        //   values: [[[string?]]],
        //   metadata: [[bool]?]
        // }
        let mut root = arena.start_record();
        root.add_simple(id_str, PrimitiveType::I64, None);

        let simple_array = root.start_array(simple_array_str, None);
        simple_array.finish(PrimitiveType::F32);

        // Build nested array type: [[[string?]]]
        let mut values_builder = root.start_array(values_str, None);
        values_builder.start_array();
        values_builder.start_array();
        values_builder.start_option();
        let values_id = values_builder.finish(PrimitiveType::String);

        // Test dropped builder (should not appear in root)
        {
            let _ = root.start_array(unused_str, None);
            // Dropped without finish
        }

        // Build nested array with option: [[bool]?]
        let mut metadata_builder = root.start_array(metadata_str, None);
        metadata_builder.start_array();
        metadata_builder.start_option();
        metadata_builder.finish(PrimitiveType::Bool);

        let root_id = root.finish();

        // Test dropped builder (should not appear in arena)
        {
            let mut dropped_union = arena.start_union(None);
            dropped_union.add_simple(unused_str, PrimitiveType::I32, None);
            // Dropped without finish
        }

        // Build a union type separately
        let mut union_builder = arena.start_union(None);
        union_builder.add_simple(success_str, PrimitiveType::Bool, None);
        union_builder.add_simple(error_str, PrimitiveType::String, None);
        let union_id = union_builder.finish();

        // Verify the arena structure
        let expected_slots = vec![
            Slot::Primitive(PrimitiveType::I32),
            Slot::Primitive(PrimitiveType::String),
            Slot::Primitive(PrimitiveType::Bool),
            Slot::Record { end: TypeId(20) },
            Slot::TypeField {
                name: id_str,
                default: None,
            },
            Slot::Primitive(PrimitiveType::I64),
            Slot::TypeField {
                name: simple_array_str,
                default: None,
            },
            Slot::Array { end: TypeId(9) },
            Slot::Primitive(PrimitiveType::F32),
            Slot::TypeField {
                name: values_str,
                default: None,
            },
            Slot::Array { end: TypeId(15) },
            Slot::Array { end: TypeId(15) },
            Slot::Array { end: TypeId(15) },
            Slot::Option { end: TypeId(15) },
            Slot::Primitive(PrimitiveType::String),
            Slot::TypeField {
                name: metadata_str,
                default: None,
            },
            Slot::Array { end: TypeId(20) },
            Slot::Array { end: TypeId(20) },
            Slot::Option { end: TypeId(20) },
            Slot::Primitive(PrimitiveType::Bool),
            Slot::Union {
                discriminator: None,
                end: TypeId(25),
            },
            Slot::TypeField {
                name: success_str,
                default: None,
            },
            Slot::Primitive(PrimitiveType::Bool),
            Slot::TypeField {
                name: error_str,
                default: None,
            },
            Slot::Primitive(PrimitiveType::String),
        ];

        assert_eq!(arena.data.len(), expected_slots.len());
        for (idx, expected) in expected_slots.iter().enumerate() {
            assert_eq!(&arena.data[idx], expected, "slot mismatch at index {idx}");
        }

        // Test getting primitive types
        assert_eq!(arena.get(i32_id), Type::I32);
        assert_eq!(arena.get(string_id), Type::String);
        assert_eq!(arena.get(bool_id), Type::Bool);

        // Test getting the root record
        let root_type = arena.get(root_id);
        let mut root_fields = match root_type {
            Type::Record(fields) => fields,
            other => panic!("expected record at root_id, got {other:?}"),
        };

        // Check id field
        let id_field = root_fields.next().expect("id field");
        assert_eq!(id_field.name, id_str);
        assert_eq!(id_field.typ, Type::I64);
        assert_eq!(id_field.default, None);

        // Check simple_array field
        let simple_array_field = root_fields.next().expect("simple_array field");
        assert_eq!(simple_array_field.name, simple_array_str);
        match simple_array_field.typ {
            Type::Array(ref inner) => {
                assert_eq!(inner.typ(), Type::F32);
            }
            other => panic!("expected array type, got {other:?}"),
        }

        // Check values field with nested arrays
        let values_field = root_fields.next().expect("values field");
        assert_eq!(values_field.name, values_str);

        // Navigate through [[[string?]]]
        let level1 = match values_field.typ {
            Type::Array(ref inner) => inner.typ(),
            other => panic!("expected array at level 1, got {other:?}"),
        };

        let level2 = match level1 {
            Type::Array(ref inner) => inner.typ(),
            other => panic!("expected array at level 2, got {other:?}"),
        };

        let level3 = match level2 {
            Type::Array(ref inner) => inner.typ(),
            other => panic!("expected array at level 3, got {other:?}"),
        };

        let option_inner = match level3 {
            Type::Option(ref inner) => inner.typ(),
            other => panic!("expected option, got {other:?}"),
        };

        assert_eq!(option_inner, Type::String);

        // Check metadata field
        let metadata_field = root_fields.next().expect("metadata field");
        assert_eq!(metadata_field.name, metadata_str);

        assert!(root_fields.next().is_none());

        // Test getting the union directly
        let union_type = arena.get(union_id);
        let mut union_fields = match union_type {
            Type::Union(u) => {
                assert_eq!(u.disriminator, None);
                u.fields
            }
            other => panic!("expected union at union_id, got {other:?}"),
        };

        let success_field = union_fields.next().expect("success field");
        assert_eq!(success_field.name, success_str);
        assert_eq!(success_field.typ, Type::Bool);

        let error_field = union_fields.next().expect("error field");
        assert_eq!(error_field.name, error_str);
        assert_eq!(error_field.typ, Type::String);

        assert!(union_fields.next().is_none());

        // Test getting values array type directly
        let values_type = arena.get(values_id);
        match values_type {
            Type::Array(ref inner) => {
                let level1 = inner.typ();
                match level1 {
                    Type::Array(ref inner2) => {
                        let level2 = inner2.typ();
                        match level2 {
                            Type::Array(ref inner3) => {
                                let level3 = inner3.typ();
                                match level3 {
                                    Type::Option(ref inner4) => {
                                        assert_eq!(inner4.typ(), Type::String);
                                    }
                                    other => {
                                        panic!("expected option at deepest level, got {other:?}")
                                    }
                                }
                            }
                            other => panic!("expected array at level 3, got {other:?}"),
                        }
                    }
                    other => panic!("expected array at level 2, got {other:?}"),
                }
            }
            other => panic!("expected array at values_id, got {other:?}"),
        }
    }
}
