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

use crate::spec::SpecContext;
use crate::spec::value::{self, Value, ValueId};
use crate::string::StringId;

/// Primitive types.
#[derive(Debug, Clone, Copy, PartialEq, derive_more::Display)]
pub enum PrimitiveTy {
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
}

/// Type that can be inlined with reference to T.
///
/// For simple inlined types, T will be SimpleTy. For normal types it will be Type
#[derive(Debug, Clone, derive_more::Display)]
pub enum InlineTy<'a, T> {
    Primitive(PrimitiveTy),

    #[display("`option`")]
    Option(Reference<'a, InlineTy<'a, T>>),
    #[display("`array`")]
    Array(Reference<'a, InlineTy<'a, T>>),

    NamedReference(NamedReference<'a, T>),
}

impl<T> From<PrimitiveTy> for InlineTy<'_, T> {
    fn from(value: PrimitiveTy) -> Self {
        Self::Primitive(value)
    }
}

impl<'a, T> FromSlot<'a> for InlineTy<'a, T> {
    fn from_slot(ctx: SpecContext<'a>, id: TypeId, slot: &Slot) -> Self {
        match slot {
            Slot::Primitive(prim) => (*prim).into(),
            Slot::Array { .. } => Self::Array(Reference {
                ctx,
                id: TypeId(id.0 + 1),
                _data: Default::default(),
            }),
            Slot::Option { .. } => Self::Option(Reference {
                ctx,
                id: TypeId(id.0 + 1),
                _data: Default::default(),
            }),
            Slot::Reference(id) => {
                let typ_def = ctx
                    .symbol_table
                    .get(id)
                    .expect("references in the TypeArena should be resolvable");
                let type_id = match typ_def.typ {
                    RootTypeId::Type(id) => id,
                    RootTypeId::Response(_) => {
                        unreachable!("invalid reference to Response from InlineTy in TypeArena")
                    }
                };

                let name = ctx.strings.resolve(*id);

                Self::NamedReference(NamedReference {
                    name,
                    id: type_id,
                    ctx,
                    _data: Default::default(),
                })
            }

            // The arena and oracle should make it impossible to construct invalid conversion
            _ => unreachable!("invalid conversion of {slot:?} to InlineTy"),
        }
    }
}

#[derive(Debug, Clone, derive_more::Display)]
pub enum SimpleTy<'a> {
    Inline(InlineTy<'a, SimpleTy<'a>>),

    #[display("`enum`")]
    Enum(Enum<'a>),
}

impl<'a> From<InlineTy<'a, SimpleTy<'a>>> for SimpleTy<'a> {
    fn from(value: InlineTy<'a, SimpleTy<'a>>) -> Self {
        Self::Inline(value)
    }
}

impl From<PrimitiveTy> for SimpleTy<'_> {
    fn from(value: PrimitiveTy) -> Self {
        Self::Inline(value.into())
    }
}

impl<'a> FromSlot<'a> for SimpleTy<'a> {
    fn from_slot(ctx: SpecContext<'a>, id: TypeId, slot: &Slot) -> Self {
        match slot {
            Slot::Primitive(_) | Slot::Array { .. } | Slot::Option { .. } | Slot::Reference(_) => {
                let inline = InlineTy::from_slot(ctx, id, slot);
                Self::Inline(inline)
            }
            Slot::Enum { typ, end } => {
                let enm = Enum {
                    id,
                    typ: *typ,
                    members: EnumMemberIterator {
                        ctx,
                        current: TypeId(id.0 + 1),
                        end: *end,
                    },
                };
                Self::Enum(enm)
            }

            // The arena and oracle should make it impossible to construct invalid conversion
            _ => unreachable!("invalid conversion of {slot:?} to SimpleTy"),
        }
    }
}

/// Any type without response.
///
/// This type doesn't include response, because it can't be a child of any other type.
/// This way you stay inside valid types when traversing Type.
#[derive(Debug, Clone, derive_more::Display)]
pub enum Type<'a> {
    Inline(InlineTy<'a, Type<'a>>),

    #[display("`record`")]
    Record(Record<'a>),
    #[display("`union`")]
    Union(Union<'a>),
    #[display("`enum`")]
    Enum(Enum<'a>),
}

impl<'a> From<InlineTy<'a, Type<'a>>> for Type<'a> {
    fn from(value: InlineTy<'a, Type<'a>>) -> Self {
        Self::Inline(value)
    }
}

impl From<PrimitiveTy> for Type<'_> {
    fn from(value: PrimitiveTy) -> Self {
        Self::Inline(value.into())
    }
}

/// Any type including response.
///
/// Used primarily for type definition.
#[derive(Debug, Clone)]
pub(crate) enum RootTypeId {
    Type(TypeId),
    Response(ResponseId),
}

impl From<TypeId> for RootTypeId {
    fn from(value: TypeId) -> Self {
        Self::Type(value)
    }
}

impl From<ResponseId> for RootTypeId {
    fn from(value: ResponseId) -> Self {
        Self::Response(value)
    }
}

impl<'a> FromSlot<'a> for Type<'a> {
    fn from_slot(ctx: SpecContext<'a>, id: TypeId, slot: &Slot) -> Self {
        match slot {
            Slot::Primitive(_) | Slot::Array { .. } | Slot::Option { .. } | Slot::Reference(_) => {
                let inline = InlineTy::from_slot(ctx, id, slot);
                Self::Inline(inline)
            }
            Slot::Enum { typ, end } => {
                let enm = Enum {
                    id,
                    typ: *typ,
                    members: EnumMemberIterator {
                        ctx,
                        current: TypeId(id.0 + 1),
                        end: *end,
                    },
                };
                Self::Enum(enm)
            }
            Slot::Record { end } => Type::Record(Record {
                id,
                fields: TypeFieldIterator {
                    ctx,
                    current: id.0 + 1,
                    end: end.0,
                    id,
                    _data: Default::default(),
                },
            }),
            Slot::Union { end } => Type::Union(Union {
                id,
                fields: TypeFieldIterator {
                    ctx,
                    current: id.0 + 1,
                    end: end.0,
                    id,
                    _data: Default::default(),
                },
            }),

            // The arena and oracle should make it impossible to construct invalid conversion
            _ => unreachable!("invalid conversion of {slot:?} to Type"),
        }
    }
}

/// Reference to [`SimpleTy`] or [`Type`].
#[derive(derive_more::Debug, Clone)]
pub struct Reference<'a, T> {
    /// Id of the type in the arena.
    pub(crate) id: TypeId,

    #[debug(skip)]
    ctx: SpecContext<'a>,

    _data: std::marker::PhantomData<T>,
}

impl<'a> Reference<'a, InlineTy<'a, SimpleTy<'a>>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> InlineTy<'a, SimpleTy<'a>> {
        self.ctx.resolve_from_slot(self.id)
    }
}

impl<'a> Reference<'a, InlineTy<'a, Type<'a>>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> InlineTy<'a, Type<'a>> {
        self.ctx.resolve_from_slot(self.id)
    }
}

#[derive(Clone, derive_more::Debug, derive_more::Display)]
#[display("reference `{name}`")]
pub struct NamedReference<'a, T> {
    pub name: &'a str,

    pub(crate) id: TypeId,

    #[display(skip)]
    #[debug(skip)]
    ctx: SpecContext<'a>,

    _data: std::marker::PhantomData<T>,
}

impl<'a> NamedReference<'a, InlineTy<'a, SimpleTy<'a>>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> InlineTy<'a, SimpleTy<'a>> {
        self.ctx.resolve_from_slot(self.id)
    }
}

impl<'a> NamedReference<'a, InlineTy<'a, Type<'a>>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> InlineTy<'a, Type<'a>> {
        self.ctx.resolve_from_slot(self.id)
    }
}

/// Internal representation of record and union field.
struct TypeFieldInner<'a, T> {
    id: TypeFieldId,
    name: &'a str,
    default: Option<Value<'a>>,
    docs: Option<&'a str>,
    typ: InlineTy<'a, T>,
}

/// Record field.
#[derive(Clone)]
pub struct RecordField<'a> {
    pub(crate) id: TypeFieldId,
    pub name: &'a str,
    pub default: Option<Value<'a>>,
    pub docs: Option<&'a str>,
    pub typ: InlineTy<'a, Type<'a>>,
}

impl<'a> From<TypeFieldInner<'a, Type<'a>>> for RecordField<'a> {
    fn from(value: TypeFieldInner<'a, Type<'a>>) -> Self {
        Self {
            id: value.id,
            name: value.name,
            default: value.default,
            docs: value.docs,
            typ: value.typ,
        }
    }
}

/// Field of a simple record
#[derive(Clone)]
pub struct SimpleRecordField<'a> {
    pub(crate) id: TypeFieldId,

    pub name: &'a str,
    pub default: Option<Value<'a>>,
    pub docs: Option<&'a str>,
    pub typ: InlineTy<'a, SimpleTy<'a>>,
}

impl<'a> From<TypeFieldInner<'a, SimpleTy<'a>>> for SimpleRecordField<'a> {
    fn from(value: TypeFieldInner<'a, SimpleTy<'a>>) -> Self {
        Self {
            id: value.id,
            name: value.name,
            default: value.default,
            docs: value.docs,
            typ: value.typ,
        }
    }
}

/// Union field.
#[derive(Clone)]
pub struct UnionField<'a> {
    pub(crate) id: TypeFieldId,
    pub name: &'a str,
    pub docs: Option<&'a str>,
    pub typ: InlineTy<'a, Type<'a>>,
}

impl<'a> From<TypeFieldInner<'a, Type<'a>>> for UnionField<'a> {
    fn from(value: TypeFieldInner<'a, Type<'a>>) -> Self {
        Self {
            id: value.id,
            name: value.name,
            docs: value.docs,
            typ: value.typ,
        }
    }
}

/// Iterator returned for composite types.
///
/// Each item is a [`TypeField`].
#[derive(derive_more::Debug, Clone)]
struct TypeFieldIterator<'a, T> {
    #[debug(skip)]
    ctx: SpecContext<'a>,
    current: u32,
    end: u32,

    // Id of the record or union
    id: TypeId,

    _data: std::marker::PhantomData<T>,
}

impl<'a, T> Iterator for TypeFieldIterator<'a, T> {
    type Item = TypeFieldInner<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current >= self.end {
            return None;
        }

        let field_id = TypeFieldId {
            container_id: self.id,
            slot_idx: self.current,
        };
        let field = self.ctx.get_type_field(field_id);

        let field_type_slot = &self.ctx.types.data[field_id.slot_idx as usize + 1];

        self.current = match &field_type_slot {
            Slot::Option { end } => end.0,
            Slot::Array { end } => end.0,
            Slot::Primitive(_) => self.current + 2,
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
    pub(crate) id: TypeId,

    fields: TypeFieldIterator<'a, Type<'a>>,
}

impl<'a> Record<'a> {
    /// Returns iterator over [record fields](RecordField)
    pub fn fields(&self) -> impl Iterator<Item = RecordField<'a>> {
        self.fields.clone().map(|it| it.into())
    }
}

/// Semantic representation of a simple record.
///
/// Fields are exposed through [`SimpleRecord::fields`].
#[derive(Debug, Clone)]
pub struct SimpleRecord<'a> {
    // Id of the record
    pub(crate) id: SimpleRecordId,

    fields: TypeFieldIterator<'a, SimpleTy<'a>>,
}

impl<'a> SimpleRecord<'a> {
    pub fn fields(&self) -> impl Iterator<Item = SimpleRecordField<'a>> {
        self.fields.clone().map(|it| it.into())
    }
}

/// Semantic representation of a tagged union.
///
/// Fields are exposed through [`Union::fields`].
#[derive(Debug, Clone)]
pub struct Union<'a> {
    // Id of the union
    pub(crate) id: TypeId,

    fields: TypeFieldIterator<'a, Type<'a>>,
}

impl<'a> Union<'a> {
    /// Returns iterator over [union fields](UnionField)
    pub fn fields(&self) -> impl Iterator<Item = UnionField<'a>> {
        self.fields.clone().map(|it| it.into())
    }
}

/// Valid enum types
#[derive(Debug, Clone, Copy, PartialEq, derive_more::Display)]
pub enum EnumTy {
    #[display("`i32`")]
    I32,
    #[display("`i64`")]
    I64,
    #[display("`u32`")]
    U32,
    #[display("`u64`")]
    U64,
    #[display("`string`")]
    String,
}

/// Semantic representation of an enum.
///
/// Field `typ` describes the type of enum values.
#[derive(Debug, Clone)]
pub struct Enum<'a> {
    // Id of the union
    pub(crate) id: TypeId,

    /// Type of the enum members
    pub typ: EnumTy,

    members: EnumMemberIterator<'a>,
}

impl<'a> Enum<'a> {
    pub fn members(&self) -> EnumMemberIterator<'a> {
        self.members.clone()
    }
}

/// A single valid value of an enum.
#[derive(Debug, Clone)]
pub struct EnumMember<'a> {
    pub value: Value<'a>,
    pub docs: Option<&'a str>,
}

/// Iterator over enum members
#[derive(derive_more::Debug, Clone)]
pub struct EnumMemberIterator<'a> {
    #[debug(skip)]
    ctx: SpecContext<'a>,
    current: TypeId,
    end: TypeId,
}

impl<'a> Iterator for EnumMemberIterator<'a> {
    type Item = EnumMember<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.0 >= self.end.0 {
            return None;
        }

        match &self.ctx.types.data[self.current.0 as usize] {
            Slot::EnumMember { value, docs } => {
                let value = self.ctx.get_value(*value);
                let docs = docs.map(|id| self.ctx.strings.resolve(id));
                Some(EnumMember { value, docs })
            }

            typ => unreachable!("invalid enum member slot in arena: {typ:?}"),
        }
    }
}

/// Semantic information about a response type.
#[derive(Debug, Clone)]
pub struct Response<'a> {
    /// Type of response body
    pub body: Type<'a>,

    /// Optional headers
    pub headers: Option<SimpleRecord<'a>>,

    /// Possible Content-Type header values.
    pub content_type: Option<value::MimeTypes<'a>>,
}

/// Type definition.
///
/// Used to store doc comment for type id that can be set
/// during type definition
#[derive(Debug, Clone)]
pub(crate) struct TypeDef {
    pub docs: Option<StringId>,
    pub typ: RootTypeId,
}

/// Id of a type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TypeId(u32);

/// Id of a simple record type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SimpleRecordId(u32);

impl SimpleRecordId {
    /// Flag given value id as valid simple record type.
    ///
    /// ## Safety
    ///
    /// The caller is responsible for ensuring that type id
    /// is valid simple record type. That is, it has to be a record where
    /// all fields have only InlineTy<SimpleTy> types.
    pub(crate) unsafe fn new_unchecked(id: TypeId) -> Self {
        Self(id.0)
    }
}

/// Id of a response type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ResponseId(u32);

/// Id of a field in a record or union.
///
/// Used for getting specific [`TypeField`] with [`TypeArena::get_field`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TypeFieldId {
    container_id: TypeId,

    /// Index of the slot in the arena
    slot_idx: u32,
}

impl TypeFieldId {
    /// Get [`TypeId`] of the record or union that contains this field.
    pub(crate) fn container_id(&self) -> TypeId {
        self.container_id
    }

    /// Get [`TypeId`] of the field's type.
    pub(crate) fn type_id(&self) -> TypeId {
        TypeId(self.slot_idx + 1)
    }
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
    Primitive(PrimitiveTy),
    Reference(StringId),
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
        /// Id of the enum type (string, i32, ...)
        typ: EnumTy,

        // Id after the last enum member.
        // Used for skipping the whole Enum during iteration.
        end: TypeId,
    },
    EnumMember {
        value: ValueId,
        docs: Option<StringId>,
    },
    Response {
        body: TypeId,
        headers: Option<SimpleRecordId>,
        content_type: Option<value::MimeTypesId>,
    },
    Record {
        // Id after the last field in the record.
        // Used for skipping the whole record during iteration.
        end: TypeId,
    },
    Union {
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
        docs: Option<StringId>,
    },
}

impl From<PrimitiveTy> for Slot {
    fn from(value: PrimitiveTy) -> Self {
        Self::Primitive(value)
    }
}

trait FromSlot<'a> {
    fn from_slot(ctx: SpecContext<'a>, id: TypeId, slot: &Slot) -> Self;
}

/// Helper type for adding records and unions to arena.
///
/// Constructed via [`TypeArena::start_record`] or [`TypeArena::start_union`].
/// Call [`finish`](FieldBuilder::finish) once all fields are added.
///
/// Dropping the builder without finishing rolls back any changes.
pub(crate) struct FieldBuilder<'p> {
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

    fn new_union(arena: &'p mut TypeArena) -> Self {
        let idx = arena.data.len();
        arena.data.push(Slot::Union { end: TypeId(0) });

        Self {
            data: &mut arena.data,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    /// Add a new field with a primitive type.
    ///
    /// `default` is a default value stored in the [`value::ValueArena`].
    pub(crate) fn add_primitive(
        &mut self,
        name: StringId,
        typ: PrimitiveTy,
        default: Option<value::ValueId>,
        docs: Option<StringId>,
    ) -> TypeFieldId {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField {
            name,
            default,
            docs,
        });
        self.data.push(typ.into());

        TypeFieldId {
            container_id: self.start,
            slot_idx,
        }
    }

    pub(crate) fn add_reference(
        &mut self,
        name: StringId,
        reference: StringId,
        default: Option<value::ValueId>,
        docs: Option<StringId>,
    ) -> TypeFieldId {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField {
            name,
            default,
            docs,
        });
        self.data.push(Slot::Reference(reference));

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
    pub(crate) fn start_array<'a>(
        &'a mut self,
        name: StringId,
        default: Option<value::ValueId>,
        docs: Option<StringId>,
    ) -> (OptionArrayBuilder<'a>, TypeFieldId) {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField {
            name,
            default,
            docs,
        });

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
    pub(crate) fn start_option<'a>(
        &'a mut self,
        name: StringId,
        default: Option<value::ValueId>,
        docs: Option<StringId>,
    ) -> (OptionArrayBuilder<'a>, TypeFieldId) {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField {
            name,
            default,
            docs,
        });

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
    pub(crate) fn finish(mut self) -> TypeId {
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
pub(crate) struct OptionArrayBuilder<'p> {
    data: &'p mut Vec<Slot>,

    /// Optionally clean up the data if array is not finished successfully.
    truncate: Option<u32>,

    /// Index in the arena that contains the first Slot::Array or Slot::Option.
    start: TypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

/// Result returned when [`OptionArrayBuilder`] is finished.
pub(crate) struct OptionArray {
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
    pub(crate) fn start_array(&mut self) -> TypeId {
        let idx = self.data.len();
        self.data.push(Slot::Array { end: TypeId(0) });
        TypeId(idx as u32)
    }

    /// Start a new option inside the current array or option.
    ///
    /// Returns id of the option type that was started. If the builder
    /// is not finished, the returned id is invalid.
    pub(crate) fn start_option(&mut self) -> TypeId {
        let idx = self.data.len();
        self.data.push(Slot::Option { end: TypeId(0) });
        TypeId(idx as u32)
    }

    /// Helper function for finishing
    fn finish(mut self, slot: Slot) -> OptionArray {
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

        self.data.push(slot);

        OptionArray {
            primitive_id: TypeId(len as u32),
            container_id: self.start,
        }
    }

    /// Finish building this type.
    pub(crate) fn finish_primitive(self, typ: PrimitiveTy) -> OptionArray {
        self.finish(typ.into())
    }

    pub(crate) fn finish_reference(self, reference: StringId) -> OptionArray {
        self.finish(Slot::Reference(reference))
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

/// Helper type for adding enums to arena.
///
/// Constructed via TODO: define constructor funcs
/// Call [`finish`](EnumBuilder::finish) once all members are added.
///
/// Dropping the builder without finishing rolls back any changes.
pub(crate) struct EnumBuilder<'p> {
    data: &'p mut Vec<Slot>,

    /// Index in the arena that contains Slot::Enum
    start: TypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'p> EnumBuilder<'p> {
    fn new(data: &'p mut Vec<Slot>, typ: EnumTy) -> Self {
        let idx = data.len();
        data.push(Slot::Enum {
            typ,
            end: TypeId(0),
        });

        Self {
            data,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    /// Add new member to the enum.
    pub(crate) fn add_member(&mut self, value: ValueId, docs: Option<StringId>) {
        self.data.push(Slot::EnumMember { value, docs });
    }

    /// Finish building the enum
    pub(crate) fn finish(mut self) -> TypeId {
        self.finished = true;

        let idx = TypeId(self.data.len() as u32);

        let start = self.start.0 as usize;
        let head = &mut self.data[start];
        match head {
            Slot::Enum { end, .. } => *end = idx,
            _ => unreachable!("invalid EnumBuilder start"),
        }

        self.start
    }
}

impl<'p> Drop for EnumBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let len = self.start.0 as usize;
        self.data.truncate(len);
    }
}

/// Arena that holds semantic types.
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct TypeArena {
    data: Vec<Slot>,
}

impl TypeArena {
    /// Create a new type arena.
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Add a primitive type to the arena.
    ///
    /// Returns the [`TypeId`] assigned to the new type.
    pub(crate) fn add_primitive(&mut self, typ: PrimitiveTy) -> TypeId {
        let idx = self.data.len();
        self.data.push(typ.into());
        TypeId(idx as u32)
    }

    /// Add a reference to the arena.
    ///
    /// Returns the [`TypeId`] assigned to the new type.
    pub(crate) fn add_reference(&mut self, reference: StringId) -> TypeId {
        let idx = self.data.len();
        self.data.push(Slot::Reference(reference));
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
    pub(crate) fn add_response(
        &mut self,
        body: TypeId,
        headers: Option<SimpleRecordId>,
        content_type: Option<value::MimeTypesId>,
    ) -> ResponseId {
        let idx = self.data.len();
        self.data.push(Slot::Response {
            body,
            headers,
            content_type,
        });
        ResponseId(idx as u32)
    }

    /// Start building an num
    ///
    /// Parameter `typ` is the type of the values in the enum.
    pub(crate) fn start_enum<'a>(&'a mut self, typ: EnumTy) -> EnumBuilder<'a> {
        EnumBuilder::new(&mut self.data, typ)
    }

    /// Start building a record type.
    ///
    /// Returns a [`FieldBuilder`] rooted at the new record slot.
    pub(crate) fn start_record<'a>(&'a mut self) -> FieldBuilder<'a> {
        FieldBuilder::new_record(self)
    }

    /// Start building a union type.
    ///
    /// Returns a [`FieldBuilder`] configured with the provided discriminator.
    pub(crate) fn start_union<'a>(&'a mut self) -> FieldBuilder<'a> {
        FieldBuilder::new_union(self)
    }

    /// Add array to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested array.
    pub(crate) fn start_array<'a>(&'a mut self) -> OptionArrayBuilder<'a> {
        OptionArrayBuilder::new_array(&mut self.data, None)
    }

    /// Add option to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested option.
    pub(crate) fn start_option<'a>(&'a mut self) -> OptionArrayBuilder<'a> {
        OptionArrayBuilder::new_option(&mut self.data, None)
    }
}

impl<'a> SpecContext<'a> {
    /// Helper method for resolving type ids
    fn resolve_from_slot<T: FromSlot<'a>>(&self, id: TypeId) -> T {
        let slot = &self.types.data[id.0 as usize];
        T::from_slot(*self, id, slot)
    }

    /// Get [`Type`] by id.
    pub(crate) fn get_type(&self, id: TypeId) -> Type<'a> {
        self.resolve_from_slot(id)
    }

    /// Get [`SimpleRecord`] by id.
    pub(crate) fn get_simple_record(&self, id: SimpleRecordId) -> SimpleRecord<'a> {
        let Slot::Record { end } = &self.types.data[id.0 as usize] else {
            unreachable!("invalid SimpleRecordId that doesn't point to a simple record");
        };

        SimpleRecord {
            id,
            fields: TypeFieldIterator {
                ctx: *self,
                current: id.0 + 1,
                end: end.0,
                id: TypeId(id.0),
                _data: Default::default(),
            },
        }
    }

    /// Get [`Response`] by id.
    pub(crate) fn get_response(&self, id: ResponseId) -> Response<'a> {
        let Slot::Response {
            body,
            headers,
            content_type,
        } = &self.types.data[id.0 as usize]
        else {
            unreachable!("invalid ResponseId that doesn't point to response");
        };

        let body = self.get_type(*body);
        let headers = headers.map(|id| self.get_simple_record(id));
        let content_type = content_type.map(|id| self.get_mime_types(id));

        Response {
            body,
            headers,
            content_type,
        }
    }

    /// Auxiliary function for getting inner type field representation by id
    fn get_type_field<T>(&self, id: TypeFieldId) -> TypeFieldInner<'a, T> {
        let field_slot = &self.types.data[id.slot_idx as usize];
        let typ_slot = &self.types.data[id.slot_idx as usize + 1];

        let (name_id, default_id, docs_id) = match &field_slot {
            Slot::TypeField {
                name,
                default,
                docs,
            } => (*name, *default, *docs),
            val => unreachable!("invalid type field in arena for id {id:?}: {val:?}"),
        };

        let name = self.strings.resolve(name_id);
        let default = default_id.map(|id| self.get_value(id));
        let docs = docs_id.map(|id| self.strings.resolve(id));

        let typ = InlineTy::from_slot(*self, TypeId(id.slot_idx + 1), typ_slot);

        TypeFieldInner {
            id,
            name,
            default,
            docs,
            typ,
        }
    }
}

#[cfg(test)]
mod test {
    use super::{PrimitiveTy, Slot, Type, TypeFieldId, TypeId};
    use crate::spec::{Spec, typ::InlineTy};

    #[test]
    fn builds_nested_types() {
        let mut spec = Spec::new_test();

        // Insert some strings
        let id_str = spec.strings.get_or_intern("id");
        let simple_array_str = spec.strings.get_or_intern("simple_array");
        let values_str = spec.strings.get_or_intern("values");
        let metadata_str = spec.strings.get_or_intern("metadata");
        let unused_str = spec.strings.get_or_intern("unused");
        let success_str = spec.strings.get_or_intern("success");
        let error_str = spec.strings.get_or_intern("error");

        // Add some primitive types
        let i32_id = spec.types.add_primitive(PrimitiveTy::I32);
        let string_id = spec.types.add_primitive(PrimitiveTy::String);
        let bool_id = spec.types.add_primitive(PrimitiveTy::Bool);

        // Build a complex nested type structure:
        // record Root {
        //   id: i64,
        //   simple_array: [f32],
        //   values: [[[string?]]],
        //   metadata: [[bool]?]
        // }
        let mut root = spec.types.start_record();
        root.add_primitive(id_str, PrimitiveTy::I64, None, None);

        let (simple_array, simple_array_field_id) = root.start_array(simple_array_str, None, None);
        simple_array.finish_primitive(PrimitiveTy::F32);

        // Build nested array type: [[[string?]]]
        let (mut values_builder, values_field_id) = root.start_array(values_str, None, None);
        values_builder.start_array();
        values_builder.start_array();
        values_builder.start_option();
        let values_id = values_builder.finish_primitive(PrimitiveTy::String);

        // Test dropped builder (should not appear in root)
        {
            let _ = root.start_array(unused_str, None, None);
            // Dropped without finish
        }

        // Build nested array with option: [[bool]?]
        let (mut metadata_builder, metadata_field_id) = root.start_array(metadata_str, None, None);
        metadata_builder.start_array();
        metadata_builder.start_option();
        metadata_builder.finish_primitive(PrimitiveTy::Bool);

        let root_id = root.finish();

        // Test dropped builder (should not appear in arena)
        {
            let mut dropped_union = spec.types.start_union();
            dropped_union.add_primitive(unused_str, PrimitiveTy::I32, None, None);
            // Dropped without finish
        }

        // Build a union type separately
        let mut union_builder = spec.types.start_union();
        union_builder.add_primitive(success_str, PrimitiveTy::Bool, None, None);
        union_builder.add_primitive(error_str, PrimitiveTy::String, None, None);
        let union_id = union_builder.finish();

        // Verify the arena structure
        let expected_slots = vec![
            Slot::Primitive(PrimitiveTy::I32),
            Slot::Primitive(PrimitiveTy::String),
            Slot::Primitive(PrimitiveTy::Bool),
            Slot::Record { end: TypeId(20) },
            Slot::TypeField {
                name: id_str,
                default: None,
                docs: None,
            },
            Slot::Primitive(PrimitiveTy::I64),
            Slot::TypeField {
                name: simple_array_str,
                default: None,
                docs: None,
            },
            Slot::Array { end: TypeId(9) },
            Slot::Primitive(PrimitiveTy::F32),
            Slot::TypeField {
                name: values_str,
                default: None,
                docs: None,
            },
            Slot::Array { end: TypeId(15) },
            Slot::Array { end: TypeId(15) },
            Slot::Array { end: TypeId(15) },
            Slot::Option { end: TypeId(15) },
            Slot::Primitive(PrimitiveTy::String),
            Slot::TypeField {
                name: metadata_str,
                default: None,
                docs: None,
            },
            Slot::Array { end: TypeId(20) },
            Slot::Array { end: TypeId(20) },
            Slot::Option { end: TypeId(20) },
            Slot::Primitive(PrimitiveTy::Bool),
            Slot::Union { end: TypeId(25) },
            Slot::TypeField {
                name: success_str,
                default: None,
                docs: None,
            },
            Slot::Primitive(PrimitiveTy::Bool),
            Slot::TypeField {
                name: error_str,
                default: None,
                docs: None,
            },
            Slot::Primitive(PrimitiveTy::String),
        ];

        assert!(spec.types.data.len() == expected_slots.len());
        for (idx, expected) in expected_slots.iter().enumerate() {
            assert!(
                &spec.types.data[idx] == expected,
                "slot mismatch at index {idx}"
            );
        }

        // Test getting primitive types
        assert!(matches!(
            spec.ctx().get_type(i32_id),
            Type::Inline(InlineTy::Primitive(PrimitiveTy::I32))
        ));
        assert!(matches!(
            spec.ctx().get_type(string_id),
            Type::Inline(InlineTy::Primitive(PrimitiveTy::String))
        ));
        assert!(matches!(
            spec.ctx().get_type(bool_id),
            Type::Inline(InlineTy::Primitive(PrimitiveTy::Bool))
        ));

        // Test getting the root record
        let root_type = spec.ctx().get_type(root_id);
        let root_record = match root_type {
            Type::Record(record) => record,
            other => panic!("expected record at root_id, got {other:?}"),
        };

        let mut root_record_fields = root_record.fields();

        // Check id field
        let id_field = root_record_fields.next().expect("id field");
        assert!(id_field.name == "id");
        assert!(matches!(
            id_field.typ,
            InlineTy::Primitive(PrimitiveTy::I64)
        ));
        assert!(id_field.default.is_none());

        // Check simple_array field
        let simple_array_field = root_record_fields.next().expect("simple_array field");
        assert!(simple_array_field.name == "simple_array");
        match simple_array_field.typ {
            InlineTy::Array(ref inner) => {
                assert!(matches!(inner.typ(), InlineTy::Primitive(PrimitiveTy::F32)));
            }
            other => panic!("expected array type, got {other:?}"),
        }

        // Check values field with nested arrays
        let values_field = root_record_fields.next().expect("values field");
        assert!(values_field.name == "values");

        // Navigate through [[[string?]]]
        let level1 = match values_field.typ {
            InlineTy::Array(ref inner) => inner.typ(),
            other => panic!("expected array at level 1, got {other:?}"),
        };

        let level2 = match level1 {
            InlineTy::Array(ref inner) => inner.typ(),
            other => panic!("expected array at level 2, got {other:?}"),
        };

        let level3 = match level2 {
            InlineTy::Array(ref inner) => inner.typ(),
            other => panic!("expected array at level 3, got {other:?}"),
        };

        let option_inner = match level3 {
            InlineTy::Option(ref inner) => inner.typ(),
            other => panic!("expected option, got {other:?}"),
        };

        assert!(matches!(
            option_inner,
            InlineTy::Primitive(PrimitiveTy::String)
        ));

        // Check metadata field
        let metadata_field = root_record_fields.next().expect("metadata field");
        assert!(metadata_field.name == "metadata");

        assert!(root_record_fields.next().is_none());

        // Test getting the union directly
        let union_type = spec.ctx().get_type(union_id);
        let mut union_fields = match union_type {
            Type::Union(u) => u.fields,
            other => panic!("expected union at union_id, got {other:?}"),
        };

        let success_field = union_fields.next().expect("success field");
        assert!(success_field.name == "success");
        assert!(matches!(
            success_field.typ,
            InlineTy::Primitive(PrimitiveTy::Bool)
        ));

        let error_field = union_fields.next().expect("error field");
        assert!(error_field.name == "error");
        assert!(matches!(
            error_field.typ,
            InlineTy::Primitive(PrimitiveTy::String)
        ));

        assert!(union_fields.next().is_none());

        // Test getting values array type directly
        let values_type = spec.ctx().get_type(values_id.container_id);
        match values_type {
            Type::Inline(InlineTy::Array(ref inner)) => {
                let level1 = inner.typ();
                match level1 {
                    InlineTy::Array(ref inner2) => {
                        let level2 = inner2.typ();
                        match level2 {
                            InlineTy::Array(ref inner3) => {
                                let level3 = inner3.typ();
                                match level3 {
                                    InlineTy::Option(ref inner4) => {
                                        assert!(matches!(
                                            inner4.typ(),
                                            InlineTy::Primitive(PrimitiveTy::String)
                                        ));
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

        assert!(values_id.primitive_id == TypeId(14));

        // Check field ids
        assert!(
            simple_array_field_id
                == TypeFieldId {
                    container_id: root_id,
                    slot_idx: 6,
                }
        );

        assert!(
            values_field_id
                == TypeFieldId {
                    container_id: root_id,
                    slot_idx: 9,
                }
        );

        assert!(
            metadata_field_id
                == TypeFieldId {
                    container_id: root_id,
                    slot_idx: 15,
                }
        );
    }
}
