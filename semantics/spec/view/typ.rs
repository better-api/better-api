use crate::spec::Spec;
use crate::spec::arena::typ::EnumTy;
use crate::spec::arena::typ::PrimitiveTy;
use crate::spec::arena::typ::arena::EnumCursor;
use crate::spec::arena::typ::arena::RecordCursor;
use crate::spec::arena::typ::arena::UnionCursor;
use crate::spec::arena::typ::arena::{
    EnumData, InlineTypeData, RecordRange, ReferenceData, ResponseData, RootTypeData, TypeData,
    UnionRange,
};
use crate::spec::arena::typ::id::{InlineTypeId, ResponseTypeId, RootTypeId, TypeId};
use crate::spec::view::value::ValueView;
use crate::text::Name;

/// Inline type tree that can reference T.
///
/// For simple inline trees, T is [`SimpleTy`]. For normal ones, T is [`Type`].
#[derive(Debug, Clone, derive_more::Display)]
pub enum InlineTyView<'a> {
    Primitive(PrimitiveTy),

    #[display("`option`")]
    Option(ReferenceView<'a>),
    #[display("`array`")]
    Array(ReferenceView<'a>),

    NamedReference(NamedTypeRefView<'a>),
}

impl From<PrimitiveTy> for InlineTyView<'_> {
    fn from(value: PrimitiveTy) -> Self {
        Self::Primitive(value)
    }
}

impl<'a> InlineTyView<'a> {
    fn new(spec: &'a Spec, id: InlineTypeId) -> Self {
        Self::from_data(spec, spec.types.get_inline_type(id))
    }

    fn from_data(spec: &'a Spec, data: InlineTypeData) -> Self {
        match data {
            InlineTypeData::Primitive(prim) => Self::Primitive(prim),
            InlineTypeData::Option(opt) => Self::Option(ReferenceView::new(spec, opt.inner_id)),
            InlineTypeData::Array(arr) => Self::Option(ReferenceView::new(spec, arr.inner_id)),
            InlineTypeData::Reference(r) => {
                Self::NamedReference(NamedTypeRefView::from_data(spec, r))
            }
        }
    }
}

/// Any non-response type.
///
/// `Type` is the named/composite subset used in most contexts.
/// The full set of possible types (including responses) is [`RootType`].
#[derive(Debug, Clone, derive_more::Display)]
pub enum TypeView<'a> {
    Inline(InlineTyView<'a>),

    #[display("`record`")]
    Record(RecordView<'a>),
    #[display("`union`")]
    Union(UnionView<'a>),
    #[display("`enum`")]
    Enum(EnumView<'a>),
}

impl<'a> From<InlineTyView<'a>> for TypeView<'a> {
    fn from(value: InlineTyView<'a>) -> Self {
        Self::Inline(value)
    }
}

impl From<PrimitiveTy> for TypeView<'_> {
    fn from(value: PrimitiveTy) -> Self {
        Self::Inline(value.into())
    }
}

impl<'a> TypeView<'a> {
    fn new(spec: &'a Spec, id: TypeId) -> Self {
        Self::from_data(spec, spec.types.get_type(id))
    }

    fn from_data(spec: &'a Spec, data: TypeData) -> Self {
        match data {
            TypeData::Inline(inline) => InlineTyView::from_data(spec, inline).into(),
            TypeData::Enum(data) => Self::Enum(EnumView::from_data(spec, data)),
            TypeData::Record(data) => Self::Record(RecordView::from_data(spec, data)),
            TypeData::Union(data) => Self::Union(UnionView::from_data(spec, data)),
        }
    }
}

/// Reference to an inline type node within the arena.
#[derive(derive_more::Debug, Clone)]
pub struct ReferenceView<'a> {
    id: InlineTypeId,

    #[debug(skip)]
    spec: &'a Spec,
}

impl<'a> ReferenceView<'a> {
    fn new(spec: &'a Spec, inner_id: InlineTypeId) -> Self {
        Self { id: inner_id, spec }
    }

    /// Resolve the referenced type.
    pub fn typ(&self) -> InlineTyView<'a> {
        InlineTyView::new(self.spec, self.id)
    }
}

/// A named reference to another type.
#[derive(Clone, derive_more::Debug, derive_more::Display)]
#[display("reference `{name}`")]
pub struct NamedTypeRefView<'a> {
    pub name: &'a str,
    id: TypeId,

    #[display(skip)]
    #[debug(skip)]
    spec: &'a Spec,
}

impl<'a> NamedTypeRefView<'a> {
    fn from_data(spec: &'a Spec, data: ReferenceData<TypeId>) -> Self {
        let name = spec.strings.resolve(data.name);
        Self {
            name,
            id: data.target,
            spec,
        }
    }
}

/// A named reference to root type.
#[derive(Clone, derive_more::Debug, derive_more::Display)]
#[display("reference `{name}`")]
pub struct NamedRootTypeRefView<'a> {
    pub name: &'a str,
    id: RootTypeId,

    #[display(skip)]
    #[debug(skip)]
    spec: &'a Spec,
}

impl<'a> NamedRootTypeRefView<'a> {
    fn from_data(spec: &'a Spec, data: ReferenceData<RootTypeId>) -> Self {
        let name = spec.strings.resolve(data.name);
        Self {
            name,
            id: data.target,
            spec,
        }
    }
}

/// Record field.
#[derive(Clone)]
pub struct RecordFieldView<'a> {
    pub name: &'a Name,
    pub default: Option<ValueView<'a>>,
    pub docs: Option<&'a str>,
    pub typ: InlineTyView<'a>,
}

/// Union field.
#[derive(Clone)]
pub struct UnionFieldView<'a> {
    pub name: &'a Name,
    pub docs: Option<&'a str>,
    pub typ: InlineTyView<'a>,
}

/// Semantic representation of a record.
///
/// Fields are exposed through [`Record::fields`].
#[derive(derive_more::Debug, Clone)]
pub struct RecordView<'a> {
    range: RecordRange,

    #[debug(skip)]
    spec: &'a Spec,
}

#[derive(derive_more::Debug, Clone)]
pub struct RecordFieldIter<'a> {
    #[debug(skip)]
    spec: &'a Spec,
    cursor: RecordCursor,
}

impl<'a> RecordView<'a> {
    fn from_data(spec: &'a Spec, data: RecordRange) -> Self {
        Self { spec, range: data }
    }

    pub fn fields(&self) -> RecordFieldIter<'a> {
        let cursor = self.spec.types.record_cursor(self.range);
        RecordFieldIter {
            spec: self.spec,
            cursor,
        }
    }
}

impl<'a> Iterator for RecordFieldIter<'a> {
    type Item = RecordFieldView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (field, next) = self.spec.types.next_record_field(self.cursor)?;

        let name = self.spec.strings.resolve_name(field.name);
        let default = field.default.map(|id| self.spec.get_value(id));
        let typ = InlineTyView::from_data(self.spec, field.typ);
        let docs = field.docs.map(|id| self.spec.strings.resolve(id));

        self.cursor = next;
        Some(RecordFieldView {
            name,
            default,
            docs,
            typ,
        })
    }
}

/// Semantic representation of a tagged union.
///
/// Fields are exposed through [`Union::fields`].
#[derive(derive_more::Debug, Clone)]
pub struct UnionView<'a> {
    range: UnionRange,

    #[debug(skip)]
    spec: &'a Spec,
}

#[derive(derive_more::Debug, Clone)]
pub struct UnionFieldIter<'a> {
    #[debug(skip)]
    spec: &'a Spec,
    cursor: UnionCursor,
}

impl<'a> UnionView<'a> {
    fn from_data(spec: &'a Spec, data: UnionRange) -> Self {
        Self { spec, range: data }
    }

    pub fn fields(&self) -> UnionFieldIter<'a> {
        let cursor = self.spec.types.union_cursor(self.range);
        UnionFieldIter {
            spec: self.spec,
            cursor,
        }
    }
}

impl<'a> Iterator for UnionFieldIter<'a> {
    type Item = UnionFieldView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (field, next) = self.spec.types.next_union_field(self.cursor)?;

        let name = self.spec.strings.resolve_name(field.name);
        let typ = InlineTyView::from_data(self.spec, field.typ);
        let docs = field.docs.map(|id| self.spec.strings.resolve(id));

        self.cursor = next;
        Some(UnionFieldView { name, docs, typ })
    }
}

/// Semantic representation of an enum.
///
/// Field `typ` describes the type of enum values.
#[derive(derive_more::Debug, Clone)]
pub struct EnumView<'a> {
    /// Type of the enum members
    pub typ: EnumTy,

    #[debug(skip)]
    spec: &'a Spec,
    data: EnumData,
}

impl<'a> EnumView<'a> {
    fn from_data(spec: &'a Spec, data: EnumData) -> Self {
        Self {
            typ: data.typ,
            spec,
            data,
        }
    }

    pub fn members(&self) -> EnumMemberIter<'a> {
        let cursor = self.spec.types.enum_cursor(self.data);
        EnumMemberIter {
            spec: self.spec,
            cursor,
        }
    }
}

/// A single valid value of an enum.
#[derive(Debug, Clone)]
pub struct EnumMemberView<'a> {
    pub value: ValueView<'a>,
    pub docs: Option<&'a str>,
}

/// Iterator over enum members
#[derive(derive_more::Debug, Clone)]
pub struct EnumMemberIter<'a> {
    #[debug(skip)]
    spec: &'a Spec,

    cursor: EnumCursor,
}

impl<'a> Iterator for EnumMemberIter<'a> {
    type Item = EnumMemberView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (data, next) = self.spec.types.next_enum_member(self.cursor)?;

        let value = self.spec.get_value(data.value);
        let docs = data.docs.map(|id| self.spec.strings.resolve(id));

        self.cursor = next;
        Some(EnumMemberView { value, docs })
    }
}

/// Semantic information about a response type.
#[derive(Debug, Clone)]
pub struct ResponseTyView<'a> {
    /// Type of response body
    pub body: InlineTyView<'a>,

    /// Optional headers
    pub headers: Option<NamedTypeRefView<'a>>,

    /// Possible Content-Type header values.
    // TODO:  Mime type
    pub content_type: Option<()>,
}

impl<'a> ResponseTyView<'a> {
    fn new(spec: &'a Spec, id: ResponseTypeId) -> Self {
        Self::from_data(spec, spec.types.get_response(id))
    }

    fn from_data(spec: &'a Spec, data: ResponseData) -> Self {
        Self {
            body: InlineTyView::new(spec, data.body),
            headers: todo!(),
            content_type: None, // TODO: mime types
        }
    }
}

/// Any type including response.
#[derive(Debug, Clone, derive_more::Display)]
pub enum RootTypeView<'a> {
    #[display("`response`")]
    Response(ResponseTyView<'a>),
    Type(TypeView<'a>),

    NamedReference(NamedRootTypeRefView<'a>),
}

impl<'a> RootTypeView<'a> {
    fn new(spec: &'a Spec, id: RootTypeId) -> Self {
        match spec.types.get_root_type(id) {
            RootTypeData::Type(data) => Self::Type(TypeView::from_data(spec, data)),
            RootTypeData::Response(data) => Self::Response(ResponseTyView::from_data(spec, data)),
            RootTypeData::Reference(data) => {
                Self::NamedReference(NamedRootTypeRefView::from_data(spec, data))
            }
        }
    }
}

/// Type definition.
pub struct TypeDefView<'a> {
    pub docs: Option<&'a str>,
    pub typ: RootTypeView<'a>,
    pub name: &'a str,
}
