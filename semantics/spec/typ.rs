//! Defines semantic representation of types.
//!
//! ## Querying
//!
//! Types are queried from [`Spec`](crate::spec::Spec) through
//! [`SpecContext`](crate::spec::SpecContext). Use [`SpecContext::get_type`],
//! [`SpecContext::get_root_type`], and [`SpecContext::get_response_type`] for
//! lookups, and [`SpecContext::get_simple_record`] for endpoint parameter
//! records.
//!
//! Within this module, [`Type`] represents the named/composite variants you
//! typically work with. The full representation, including responses and other
//! root-only variants, is [`RootType`].
//!
//! ## Construction
//!
//! Construction is handled by [`Oracle`](crate::Oracle). It builds the internal
//! arenas and performs validation before data is exposed through `SpecContext`.

use crate::spec::SpecContext;
use crate::spec::value::{self, Value, ValueContext, ValueId};
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

/// Inline type tree that can reference T.
///
/// For simple inline trees, T is [`SimpleTy`]. For normal ones, T is [`Type`].
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
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
        match slot {
            Slot::Primitive(prim) => (*prim).into(),
            Slot::Array { .. } => Self::Array(Reference {
                ctx,
                id: slot_idx + 1,
                _data: Default::default(),
            }),
            Slot::Option { .. } => Self::Option(Reference {
                ctx,
                id: slot_idx + 1,
                _data: Default::default(),
            }),
            Slot::Reference(_) => {
                let reference = NamedReference::from_slot(ctx, slot_idx, slot);
                Self::NamedReference(reference)
            }

            // The arena and oracle should construct a valid arena
            _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to InlineTy"),
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
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
        match slot {
            Slot::Primitive(_)
            | Slot::Array { .. }
            | Slot::Option { .. }
            | Slot::Reference { .. } => {
                let inline = InlineTy::from_slot(ctx, slot_idx, slot);
                Self::Inline(inline)
            }
            Slot::Enum { typ, end } => {
                let enm = Enum {
                    id: TypeId(slot_idx),
                    typ: *typ,
                    members: EnumMemberIterator {
                        ctx,
                        current: TypeId(slot_idx + 1),
                        end: *end,
                    },
                };
                Self::Enum(enm)
            }

            // The arena and oracle should construct a valid arena
            _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to SimpleTy"),
        }
    }
}

/// Any non-response type.
///
/// `Type` is the named/composite subset used in most contexts.
/// The full set of possible types (including responses) is [`RootType`].
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

impl<'a> FromSlot<'a> for Type<'a> {
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
        match slot {
            Slot::Primitive(_)
            | Slot::Array { .. }
            | Slot::Option { .. }
            | Slot::Reference { .. } => {
                let inline = InlineTy::from_slot(ctx, slot_idx, slot);
                Self::Inline(inline)
            }
            Slot::Enum { typ, end } => {
                let enm = Enum {
                    id: TypeId(slot_idx),
                    typ: *typ,
                    members: EnumMemberIterator {
                        ctx,
                        current: TypeId(slot_idx + 1),
                        end: *end,
                    },
                };
                Self::Enum(enm)
            }
            Slot::Record { end } => Type::Record(Record {
                id: TypeId(slot_idx),
                fields: TypeFieldIterator {
                    ctx,
                    current: slot_idx + 1,
                    end: end.0,
                    id: TypeId(slot_idx),
                    _data: Default::default(),
                },
            }),
            Slot::Union { end } => Type::Union(Union {
                id: TypeId(slot_idx),
                fields: TypeFieldIterator {
                    ctx,
                    current: slot_idx + 1,
                    end: end.0,
                    id: TypeId(slot_idx),
                    _data: Default::default(),
                },
            }),

            // The arena and oracle should construct a valid arena
            _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to Type"),
        }
    }
}

/// Reference to an inline type node within the arena.
#[derive(derive_more::Debug, Clone)]
pub struct Reference<'a, T> {
    id: u32,

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

/// Reference to a named [`Type`] definition.
///
/// Validated by the oracle before insertion into the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub(crate) struct TypeRef(pub StringId);

/// Reference to a named [`RootType`] definition.
///
/// Validated by the oracle before insertion into the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub(crate) struct RootRef(pub StringId);

impl From<TypeRef> for RootRef {
    fn from(value: TypeRef) -> Self {
        Self(value.0)
    }
}

#[derive(Clone, derive_more::Debug, derive_more::Display)]
#[display("reference `{name}`")]
pub struct NamedReference<'a, T> {
    pub name: &'a str,

    id: u32,

    #[display(skip)]
    #[debug(skip)]
    ctx: SpecContext<'a>,

    _data: std::marker::PhantomData<T>,
}

impl<'a, T> FromSlot<'a> for NamedReference<'a, T> {
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
        match slot {
            Slot::Reference(name_id) => {
                let name = ctx.strings.resolve(*name_id);

                let type_def = ctx
                    .symbol_table
                    .get(name_id)
                    .expect("invalid type arena: missing reference in symbol table");

                NamedReference {
                    name,
                    id: type_def.typ.0,
                    ctx,
                    _data: Default::default(),
                }
            }

            // The arena and oracle should construct a valid arena
            _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to NamedReference"),
        }
    }
}

impl<'a> NamedReference<'a, SimpleTy<'a>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> SimpleTy<'a> {
        self.ctx.resolve_from_slot(self.id)
    }
}

impl<'a> NamedReference<'a, Type<'a>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> Type<'a> {
        self.ctx.resolve_from_slot(self.id)
    }
}

impl<'a> NamedReference<'a, SimpleRecordReference<'a>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> SimpleRecordReference<'a> {
        self.ctx.resolve_from_slot(self.id)
    }
}

impl<'a> NamedReference<'a, ResponseReference<'a>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> ResponseReference<'a> {
        self.ctx.resolve_from_slot(self.id)
    }
}

impl<'a> NamedReference<'a, RootType<'a>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> RootType<'a> {
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
/// Each item is a field in the record or union.
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
            Slot::Primitive(_) | Slot::Reference { .. } => self.current + 2,
            typ => unreachable!("invalid field type for {field_id:?}: {typ:?}"),
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
    /// Returns iterator over [record fields](RecordField).
    pub fn fields(&self) -> impl Iterator<Item = RecordField<'a>> + use<'a> {
        self.fields.clone().map(|it| it.into())
    }
}

/// Semantic representation of a simple record.
///
/// Fields are exposed through [`SimpleRecord::fields`].
#[derive(Debug, Clone)]
pub struct SimpleRecord<'a> {
    fields: TypeFieldIterator<'a, SimpleTy<'a>>,
}

impl<'a> FromSlot<'a> for SimpleRecord<'a> {
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
        match slot {
            Slot::Record { end } => Self {
                fields: TypeFieldIterator {
                    ctx,
                    current: slot_idx + 1,
                    end: end.0,
                    id: TypeId(slot_idx),
                    _data: Default::default(),
                },
            },

            // The arena and oracle should construct a valid arena
            _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to SimpleRecord"),
        }
    }
}

impl<'a> SimpleRecord<'a> {
    /// Returns iterator over [simple record fields](SimpleRecordField).
    pub fn fields(&self) -> impl Iterator<Item = SimpleRecordField<'a>> + use<'a> {
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
    /// Returns iterator over [union fields](UnionField).
    pub fn fields(&self) -> impl Iterator<Item = UnionField<'a>> + use<'a> {
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
    // Id of the enum
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
                let val_ctx: ValueContext = self.ctx.into();
                let value = val_ctx.get_value(*value);
                let docs = docs.map(|id| self.ctx.strings.resolve(id));
                let member = EnumMember { value, docs };
                self.current = TypeId(self.current.0 + 1);
                Some(member)
            }

            slot => unreachable!(
                "invalid enum member slot at {current:?}: {slot:?}",
                current = self.current
            ),
        }
    }
}

/// Semantic information about a response type.
#[derive(Debug, Clone)]
pub struct ResponseTy<'a> {
    /// Type of response body
    pub body: InlineTy<'a, Type<'a>>,

    /// Optional headers
    pub headers: Option<NamedReference<'a, SimpleRecordReference<'a>>>,

    /// Possible Content-Type header values.
    pub content_type: Option<value::MimeTypes<'a>>,
}

impl<'a> FromSlot<'a> for ResponseTy<'a> {
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
        let Slot::Response {
            body,
            headers,
            content_type,
        } = slot
        else {
            unreachable!("invalid ResponseId {slot_idx} that doesn't point to response");
        };

        let body = ctx.get_inline_type(*body);
        let headers = headers.map(|id| ctx.get_simple_record_reference(id));

        let val_ctx: ValueContext = ctx.into();
        let content_type = content_type.map(|id| val_ctx.get_mime_types(id));

        ResponseTy {
            body,
            headers,
            content_type,
        }
    }
}

/// Any type including response.
#[derive(Debug, Clone, derive_more::Display)]
pub enum RootType<'a> {
    #[display("`response`")]
    Response(ResponseTy<'a>),
    Type(Type<'a>),

    NamedReference(NamedReference<'a, RootType<'a>>),
}

impl<'a> FromSlot<'a> for RootType<'a> {
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
        match slot {
            Slot::Response { .. } => {
                let resp = ResponseTy::from_slot(ctx, slot_idx, slot);
                Self::Response(resp)
            }
            Slot::Reference(_) => {
                let reference = NamedReference::from_slot(ctx, slot_idx, slot);
                Self::NamedReference(reference)
            }
            _ => {
                let typ = Type::from_slot(ctx, slot_idx, slot);
                Self::Type(typ)
            }
        }
    }
}

/// Reference that terminates with response type.
///
/// This type accommodates deep references as well, like Foo -> Bar -> Baz -> resp
/// Usually it's wrapped in a `NamedReference`, ie `NamedReference<ResponseReference>`. This
/// way types guarantee that you don't have a direct ResponseTy without a reference in between.
#[derive(Debug, Clone, derive_more::Display)]
pub enum ResponseReference<'a> {
    #[display("`response`")]
    Response(ResponseTy<'a>),
    NamedReference(NamedReference<'a, ResponseReference<'a>>),
}

impl<'a> FromSlot<'a> for ResponseReference<'a> {
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
        match slot {
            Slot::Response { .. } => {
                let resp = ResponseTy::from_slot(ctx, slot_idx, slot);
                Self::Response(resp)
            }
            Slot::Reference(_) => {
                let reference = NamedReference::from_slot(ctx, slot_idx, slot);
                Self::NamedReference(reference)
            }

            // The arena and oracle should construct a valid arena
            _ => {
                unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to ResponseReference")
            }
        }
    }
}

/// Reference that terminates with simple record type.
///
/// This type accommodates deep references as well, like Foo -> Bar -> Baz -> simple record
/// Usually it's wrapped in a `NamedReference`, ie `NamedReference<SimpleRecordReference>`. This
/// way types guarantee that you don't have a direct SimpleRecord without a reference in between.
#[derive(Debug, Clone, derive_more::Display)]
pub enum SimpleRecordReference<'a> {
    #[display("`record`")]
    SimpleRecord(SimpleRecord<'a>),
    NamedReference(NamedReference<'a, SimpleRecordReference<'a>>),
}

impl<'a> FromSlot<'a> for SimpleRecordReference<'a> {
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
        match slot {
            Slot::Record { .. } => {
                let simple_rec = SimpleRecord::from_slot(ctx, slot_idx, slot);
                Self::SimpleRecord(simple_rec)
            }
            Slot::Reference(_) => {
                let reference = NamedReference::from_slot(ctx, slot_idx, slot);
                Self::NamedReference(reference)
            }

            // The arena and oracle should construct a valid arena
            _ => {
                unreachable!(
                    "invalid conversion of slot {slot_idx}: {slot:?} to SimpleRecordReference"
                )
            }
        }
    }
}

/// Type definition.
pub struct TypeDef<'a> {
    pub docs: Option<&'a str>,
    pub typ: RootType<'a>,
    pub name: &'a str,
}

/// Type definition.
///
/// Stores the doc comment for a named type definition.
#[derive(Debug, Clone)]
pub(crate) struct TypeDefData {
    pub docs: Option<StringId>,
    pub typ: RootTypeId,
    pub name: StringId,
}

/// Id of any type including response.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RootTypeId(u32);

impl From<TypeId> for RootTypeId {
    fn from(value: TypeId) -> Self {
        Self(value.0)
    }
}

impl From<ResponseTyId> for RootTypeId {
    fn from(value: ResponseTyId) -> Self {
        Self(value.0)
    }
}

/// Id of a type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TypeId(u32);

impl TypeId {
    /// Treat a [`RootTypeId`] as a [`TypeId`] without validation.
    ///
    /// ## Safety
    ///
    /// The caller must ensure the id does not refer to a response type.
    pub(crate) unsafe fn new_unchecked(id: RootTypeId) -> Self {
        Self(id.0)
    }
}

/// Id of a inline type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct InlineTyId(u32);

impl InlineTyId {
    /// Treat a [`RootTypeId`] as a [`InlineTyId`] without validation.
    ///
    /// ## Safety
    ///
    /// The caller must ensure the id refers to an inline type that isn't a response.
    pub(crate) unsafe fn new_unchecked(id: RootTypeId) -> Self {
        Self(id.0)
    }
}

/// Id of a named reference to stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SimpleRecordReferenceId(u32);

impl SimpleRecordReferenceId {
    /// Treat a [`RootTypeId`] as a [`SimpleRecordReferenceId`] without validation.
    ///
    /// ## Safety
    ///
    /// The caller must ensure the id refers to a named reference that terminates
    /// with a [`SimpleRecord`]. That is, id must be a valid
    /// `NamedReference<SimpleRecordReference>`
    pub(crate) unsafe fn new_unchecked(id: RootTypeId) -> Self {
        Self(id.0)
    }
}

/// Id of a named reference to stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ResponseReferenceId(u32);

impl ResponseReferenceId {
    /// Treat a [`RootTypeId`] as a [`ResponseReferenceId`] without validation.
    ///
    /// ## Safety
    ///
    /// The caller must ensure the id refers to a named reference that terminates
    /// with a [`ResponseTy`]. That is, id must be a valid `NamedReference<ResponseReference>`.
    pub(crate) unsafe fn new_unchecked(id: RootTypeId) -> Self {
        Self(id.0)
    }
}

/// Id of a response type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ResponseTyId(u32);

/// Id of a field in a record or union.
///
/// Used for getting a specific field through the context.
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
/// Array and Option can be nested to form complex inline types
/// (for example `[[[i32?]]?]?`). They can also appear as record/union
/// field types, so we need an efficient way to skip the whole nesting
/// when iterating fields.
///
/// Array and Option work by storing the wrapped type in the next slot.
/// For example, `[i32]` is `[Slot::Array, Slot::Primitive(_)]`.
/// The `end` field marks the slot after the whole nesting so field
/// iteration can skip the full inline tree.
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
        body: InlineTyId,
        headers: Option<SimpleRecordReferenceId>,
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
    fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self;
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
        reference: TypeRef,
        default: Option<value::ValueId>,
        docs: Option<StringId>,
    ) -> TypeFieldId {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField {
            name,
            default,
            docs,
        });
        self.data.push(Slot::Reference(reference.0));

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
            _ => unreachable!(
                "invalid FieldBuilder start at {start:?}",
                start = self.start
            ),
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
/// After you are done, call [`OptionArrayBuilder::finish_primitive`] or
/// [`OptionArrayBuilder::finish_reference`], which returns ids for both
/// the leaf and the container.
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
    /// Id of the final leaf type inserted into the builder.
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
                _ => unreachable!("invalid slot while finishing OptionArrayBuilder: {slot:?}"),
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

    pub(crate) fn finish_reference(self, reference: TypeRef) -> OptionArray {
        self.finish(Slot::Reference(reference.0))
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
/// Constructed via [`TypeArena::start_enum`].
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
            _ => unreachable!("invalid EnumBuilder start at {start:?}", start = self.start),
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
    pub(crate) fn add_reference(&mut self, reference: RootRef) -> RootTypeId {
        let idx = self.data.len();
        self.data.push(Slot::Reference(reference.0));
        RootTypeId(idx as u32)
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
        body: InlineTyId,
        headers: Option<SimpleRecordReferenceId>,
        content_type: Option<value::MimeTypesId>,
    ) -> ResponseTyId {
        let idx = self.data.len();
        self.data.push(Slot::Response {
            body,
            headers,
            content_type,
        });
        ResponseTyId(idx as u32)
    }

    /// Start building an enum.
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
    /// Returns a [`FieldBuilder`] rooted at the new union slot.
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
    fn resolve_from_slot<T: FromSlot<'a>>(&self, id: u32) -> T {
        let slot = &self.types.data[id as usize];
        T::from_slot(*self, id, slot)
    }

    /// Get [`Type`] by id.
    pub(crate) fn get_type(&self, id: TypeId) -> Type<'a> {
        self.resolve_from_slot(id.0)
    }

    /// Get [`InlineTy`] type by id.
    pub(crate) fn get_inline_type(&self, id: InlineTyId) -> InlineTy<'a, Type<'a>> {
        self.resolve_from_slot(id.0)
    }

    /// Get [`SimpleRecordReference`] type by id.
    pub(crate) fn get_simple_record_reference(
        &self,
        id: SimpleRecordReferenceId,
    ) -> NamedReference<'a, SimpleRecordReference<'a>> {
        self.resolve_from_slot(id.0)
    }

    /// Get [`ResponseReference`] type by id.
    pub(crate) fn get_response_reference(
        &self,
        id: ResponseReferenceId,
    ) -> NamedReference<'a, ResponseReference<'a>> {
        self.resolve_from_slot(id.0)
    }

    /// Get [`ResponseTy`] type by id.
    pub(crate) fn get_response_type(&self, id: ResponseTyId) -> ResponseTy<'a> {
        self.resolve_from_slot(id.0)
    }

    /// Get [`RootType`] by id.
    pub(crate) fn get_root_type(&self, id: RootTypeId) -> RootType<'a> {
        self.resolve_from_slot(id.0)
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

        let val_ctx: ValueContext = (*self).into();
        let default = default_id.map(|id| val_ctx.get_value(id));
        let docs = docs_id.map(|id| self.strings.resolve(id));

        let typ = InlineTy::from_slot(*self, id.slot_idx + 1, typ_slot);

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
    use super::{
        EnumTy, PrimitiveTy, RootRef, RootType, SimpleTy, Slot, Type, TypeFieldId, TypeId,
    };
    use crate::spec::typ::{InlineTyId, SimpleRecordReference, SimpleRecordReferenceId};
    use crate::spec::value::{PrimitiveValue, Value};
    use crate::spec::{
        Spec,
        typ::{InlineTy, TypeDefData, TypeRef},
    };

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
            Type::Union(u) => u.fields(),
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

    #[test]
    fn named_refs() {
        let mut spec = Spec::new_test();

        // Build spec like this:
        // type Foo: string
        // type Bar: rec {
        //   name: Foo
        // }

        let foo_name = spec.strings.get_or_intern("Foo");
        let name_field = spec.strings.get_or_intern("name");

        let foo_typ = spec.types.add_primitive(PrimitiveTy::String);
        spec.symbol_table.insert(
            foo_name,
            TypeDefData {
                docs: None,
                typ: foo_typ.into(),
                name: foo_name,
            },
        );

        let mut bar_builder = spec.types.start_record();
        bar_builder.add_reference(name_field, TypeRef(foo_name), None, None);
        let bar_typ = bar_builder.finish();

        // Get Bar type and verify
        let record = match spec.ctx().get_type(bar_typ) {
            Type::Record(record) => record,
            _ => panic!("expected record type for Bar"),
        };

        // get name field
        let mut fields = record.fields();
        let name_field = fields.next().expect("name field should exist");

        // Check no more fields
        assert!(fields.next().is_none());

        match name_field.typ {
            InlineTy::NamedReference(reference) => {
                assert_eq!(reference.name, "Foo");
                assert!(matches!(
                    reference.typ(),
                    Type::Inline(InlineTy::Primitive(PrimitiveTy::String))
                ))
            }
            _ => panic!("expected named reference for name field"),
        }
    }

    #[test]
    fn simple_record_with_enum_option() {
        let mut spec = Spec::new_test();

        let record_name = spec.strings.get_or_intern("SimpleRec");
        let status_name = spec.strings.get_or_intern("Status");
        let open_str = spec.strings.get_or_intern("open");
        let closed_str = spec.strings.get_or_intern("closed");
        let id_str = spec.strings.get_or_intern("id");
        let title_str = spec.strings.get_or_intern("title");
        let status_str = spec.strings.get_or_intern("status");

        let open_value = spec.values.add_primitive(PrimitiveValue::String(open_str));
        let closed_value = spec
            .values
            .add_primitive(PrimitiveValue::String(closed_str));

        let mut enum_builder = spec.types.start_enum(EnumTy::String);
        enum_builder.add_member(open_value, None);
        enum_builder.add_member(closed_value, None);
        let enum_id = enum_builder.finish();

        spec.symbol_table.insert(
            status_name,
            TypeDefData {
                docs: None,
                typ: enum_id.into(),
                name: status_name,
            },
        );

        let mut record_builder = spec.types.start_record();
        record_builder.add_primitive(id_str, PrimitiveTy::I64, None, None);
        record_builder.add_primitive(title_str, PrimitiveTy::String, None, None);
        let (status_builder, _) = record_builder.start_option(status_str, None, None);
        status_builder.finish_reference(TypeRef(status_name));
        let record_id = record_builder.finish();

        spec.symbol_table.insert(
            record_name,
            TypeDefData {
                docs: None,
                typ: record_id.into(),
                name: record_name,
            },
        );

        let ref_id = unsafe {
            SimpleRecordReferenceId::new_unchecked(spec.types.add_reference(RootRef(record_name)))
        };

        let simple_rec_ref = spec.ctx().get_simple_record_reference(ref_id);
        let record = match simple_rec_ref.typ() {
            SimpleRecordReference::SimpleRecord(record) => record,
            SimpleRecordReference::NamedReference(_) => {
                panic!("expected simple record, got named reference")
            }
        };

        let mut fields = record.fields();

        let id_field = fields.next().expect("id field");
        assert_eq!(id_field.name, "id");
        assert!(matches!(
            id_field.typ,
            InlineTy::Primitive(PrimitiveTy::I64)
        ));

        let title_field = fields.next().expect("title field");
        assert_eq!(title_field.name, "title");
        assert!(matches!(
            title_field.typ,
            InlineTy::Primitive(PrimitiveTy::String)
        ));

        let status_field = fields.next().expect("status field");
        assert_eq!(status_field.name, "status");
        let option_inner = match status_field.typ {
            InlineTy::Option(ref inner) => inner.typ(),
            other => panic!("expected option for status field, got {other:?}"),
        };

        let status_enum = match option_inner {
            InlineTy::NamedReference(reference) => {
                assert_eq!(reference.name, "Status");
                match reference.typ() {
                    SimpleTy::Enum(enm) => enm,
                    other => panic!("expected enum for status reference, got {other:?}"),
                }
            }
            other => panic!("expected named reference for status field, got {other:?}"),
        };

        let mut members = status_enum.members();
        let open_member = members.next().expect("open enum member");
        assert!(matches!(open_member.value, Value::String("open")));
        let closed_member = members.next().expect("closed enum member");
        assert!(matches!(closed_member.value, Value::String("closed")));
        assert!(members.next().is_none());

        assert!(fields.next().is_none());
    }

    #[test]
    fn root_reference_response_enum_body() {
        let mut spec = Spec::new_test();

        let enum_name = spec.strings.get_or_intern("Enum");
        let response_name = spec.strings.get_or_intern("StatusResponse");
        let ok_str = spec.strings.get_or_intern("ok");
        let error_str = spec.strings.get_or_intern("error");

        let ok_value = spec.values.add_primitive(PrimitiveValue::String(ok_str));
        let error_value = spec.values.add_primitive(PrimitiveValue::String(error_str));

        let mut enum_builder = spec.types.start_enum(EnumTy::String);
        enum_builder.add_member(ok_value, None);
        enum_builder.add_member(error_value, None);
        let enum_id = enum_builder.finish();

        spec.symbol_table.insert(
            enum_name,
            TypeDefData {
                docs: None,
                typ: enum_id.into(),
                name: enum_name,
            },
        );
        let enum_ref =
            unsafe { InlineTyId::new_unchecked(spec.types.add_reference(RootRef(enum_name))) };

        let response_id = spec.types.add_response(enum_ref, None, None);

        spec.symbol_table.insert(
            response_name,
            TypeDefData {
                docs: None,
                typ: response_id.into(),
                name: response_name,
            },
        );

        let response_ref_id = spec.types.add_reference(RootRef(response_name));
        let response_root = spec.ctx().get_root_type(response_ref_id);

        let response = match response_root {
            RootType::NamedReference(reference) => match reference.typ() {
                RootType::Response(response) => response,
                other => panic!("expected response type, got {other:?}"),
            },

            other => panic!("expected reference to response, got {other:?}"),
        };

        assert!(response.headers.is_none());
        assert!(response.content_type.is_none());

        let status_enum = match response.body {
            InlineTy::NamedReference(reference) => match reference.typ() {
                Type::Enum(enm) => enm,
                other => panic!("expected enum response body, got {other:?}"),
            },
            other => panic!("expected named reference response body, got {other:?}"),
        };
        assert_eq!(status_enum.typ, EnumTy::String);

        let mut members = status_enum.members();
        let ok_member = members.next().expect("ok enum member");
        assert!(matches!(ok_member.value, Value::String("ok")));
        let error_member = members.next().expect("error enum member");
        assert!(matches!(error_member.value, Value::String("error")));
        assert!(members.next().is_none());
    }
}
