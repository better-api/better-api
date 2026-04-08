use crate::spec::Spec;
use crate::spec::arena::typ::EnumTy;
use crate::spec::arena::typ::arena::{
    EnumData, InlineTypeData, RecordRange, ReferenceData, ResponseData, RootTypeData, TypeData,
    UnionRange,
};
use crate::spec::arena::typ::id::{
    InlineTypeId, ResponseReferenceId, ResponseTypeId, RootTypeId, TypeId,
};
use crate::spec::arena::typ::{PrimitiveTy, id::TypeFieldId};
use crate::spec::view::value::ValueView;
use crate::text::Name;

/// Inline type tree that can reference T.
///
/// For simple inline trees, T is [`SimpleTy`]. For normal ones, T is [`Type`].
#[derive(Debug, Clone, derive_more::Display)]
pub enum InlineTyView<'a, T> {
    Primitive(PrimitiveTy),

    #[display("`option`")]
    Option(ReferenceView<'a, InlineTyView<'a, T>>),
    #[display("`array`")]
    Array(ReferenceView<'a, InlineTyView<'a, T>>),

    NamedReference(NamedReferenceView<'a, T>),
}

impl<T> From<PrimitiveTy> for InlineTyView<'_, T> {
    fn from(value: PrimitiveTy) -> Self {
        Self::Primitive(value)
    }
}

impl<'a, T> InlineTyView<'a, T> {
    fn new(spec: &'a Spec, id: InlineTypeId) -> Self {
        Self::from_data(spec, spec.types.get_inline_type(id))
    }

    fn from_data(spec: &'a Spec, data: InlineTypeData) -> Self {
        match data {
            InlineTypeData::Primitive(prim) => Self::Primitive(prim),
            InlineTypeData::Option(opt) => Self::Option(ReferenceView {
                id: opt.inner_id,
                spec,
                _data: std::marker::PhantomData,
            }),
            InlineTypeData::Array(arr) => Self::Option(ReferenceView {
                id: arr.inner_id,
                spec,
                _data: std::marker::PhantomData,
            }),
            InlineTypeData::Reference(r) => Self::NamedReference(NamedReferenceView::new(spec, r)),
        }
    }
}

#[derive(Debug, Clone, derive_more::Display)]
pub enum SimpleTyView<'a> {
    Inline(InlineTyView<'a, SimpleTyView<'a>>),

    #[display("`enum`")]
    Enum(EnumView<'a>),
}

impl<'a> From<InlineTyView<'a, SimpleTyView<'a>>> for SimpleTyView<'a> {
    fn from(value: InlineTyView<'a, SimpleTyView<'a>>) -> Self {
        Self::Inline(value)
    }
}

impl From<PrimitiveTy> for SimpleTyView<'_> {
    fn from(value: PrimitiveTy) -> Self {
        Self::Inline(value.into())
    }
}

impl<'a> FromRootTy<'a> for SimpleTyView<'a> {
    fn from_root(_spec: &'a Spec, ty: RootTypeView<'a>) -> Self {
        match ty {
            // We can just "cast" the inner types by reconstructing the marker. If anything is not
            // ok (arena is not constructed correctly), the typ()/resolve() methods will panic.
            RootTypeView::Type(ty) => match ty {
                TypeView::Inline(ty) => {
                    let inline: InlineTyView<'a, SimpleTyView<'a>> = match ty {
                        InlineTyView::Primitive(p) => InlineTyView::Primitive(p),
                        InlineTyView::Option(o) => InlineTyView::Option(ReferenceView {
                            id: o.id,
                            spec: o.spec,
                            _data: std::marker::PhantomData,
                        }),
                        InlineTyView::Array(a) => InlineTyView::Array(ReferenceView {
                            id: a.id,
                            spec: a.spec,
                            _data: std::marker::PhantomData,
                        }),
                        InlineTyView::NamedReference(r) => InlineTyView::NamedReference(r.cast()),
                    };
                    Self::Inline(inline)
                }
                TypeView::Enum(view) => Self::Enum(view),

                TypeView::Record(_) => {
                    unreachable!("invalid Record slot for expected SimpleTyView")
                }
                TypeView::Union(_) => unreachable!("invalid Union slot for expected SimpleTyView"),
            },
            RootTypeView::NamedReference(r) => Self::Inline(InlineTyView::NamedReference(r.cast())),

            RootTypeView::Response(_) => {
                unreachable!("invalid Response slot for expected SimpleTyView")
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
    Inline(InlineTyView<'a, TypeView<'a>>),

    #[display("`record`")]
    Record(RecordView<'a>),
    #[display("`union`")]
    Union(UnionView<'a>),
    #[display("`enum`")]
    Enum(EnumView<'a>),
}

impl<'a> From<InlineTyView<'a, TypeView<'a>>> for TypeView<'a> {
    fn from(value: InlineTyView<'a, TypeView<'a>>) -> Self {
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

impl<'a> FromRootTy<'a> for TypeView<'a> {
    fn from_root(_spec: &'a Spec, ty: RootTypeView<'a>) -> Self {
        match ty {
            RootTypeView::Type(ty) => ty,

            // We cast reference to any type, into reference to Type. References are all the same,
            // except the _data marker. If the reference doesn't actually point to Type, but
            // instead to ie. Response, the NamedReferenceView::typ() will panic.
            RootTypeView::NamedReference(r) => Self::Inline(InlineTyView::NamedReference(r.cast())),

            RootTypeView::Response(_) => {
                unreachable!("invalid Response slot for expected TypeView")
            }
        }
    }
}

/// Reference to an inline type node within the arena.
#[derive(derive_more::Debug, Clone)]
pub struct ReferenceView<'a, T> {
    id: InlineTypeId,

    #[debug(skip)]
    spec: &'a Spec,

    _data: std::marker::PhantomData<T>,
}

impl<'a, T> ReferenceView<'a, InlineTyView<'a, T>> {
    /// Resolve the referenced type.
    pub fn typ(&self) -> InlineTyView<'a, SimpleTyView<'a>> {
        InlineTyView::new(self.spec, self.id)
    }
}

/// A named reference to another type.
#[derive(Clone, derive_more::Debug, derive_more::Display)]
#[display("reference `{name}`")]
pub struct NamedReferenceView<'a, T> {
    pub name: &'a str,

    id: RootTypeId,

    #[display(skip)]
    #[debug(skip)]
    spec: &'a Spec,

    _data: std::marker::PhantomData<T>,
}

impl<'a, T> NamedReferenceView<'a, T> {
    fn new<Id: Into<RootTypeId>>(spec: &'a Spec, data: ReferenceData<Id>) -> Self {
        let name = spec.strings.resolve(data.name);
        Self {
            name,
            id: data.target.into(),
            spec,
            _data: std::marker::PhantomData,
        }
    }

    /// Casts named reference targeting T, into named reference targeting U.
    /// If cast is invalid subsequent calls to .typ() will panic!
    fn cast<U>(self) -> NamedReferenceView<'a, U> {
        NamedReferenceView {
            name: self.name,
            id: self.id,
            spec: self.spec,
            _data: std::marker::PhantomData,
        }
    }
}

impl<'a, T> FromRootTy<'a> for NamedReferenceView<'a, T> {
    fn from_root(_spec: &'a Spec, ty: RootTypeView<'a>) -> Self {
        match ty {
            RootTypeView::NamedReference(view) => Self {
                name: view.name,
                id: view.id,
                spec: view.spec,
                _data: std::marker::PhantomData,
            },

            RootTypeView::Type(TypeView::Inline(InlineTyView::NamedReference(r))) => r.cast(),
            RootTypeView::Type(_) => {
                unreachable!("invalid Type slot for expected NamedReferenceView")
            }

            RootTypeView::Response(_) => {
                unreachable!("invalid Response slot for expected NamedReferenceView")
            }
        }
    }
}

fn resolve_named_reference<'a, T: FromRootTy<'a>>(r: &NamedReferenceView<'a, T>) -> T {
    let ty = RootTypeView::new(r.spec, r.id);
    T::from_root(r.spec, ty)
}

fn collapse_named_reference<'a, T: FromRootTy<'a>>(r: &NamedReferenceView<'a, T>) -> T {
    let mut id = r.id;

    loop {
        let ty = RootTypeView::new(r.spec, id);
        match ty {
            RootTypeView::NamedReference(reference) => {
                id = reference.id;
            }
            _ => return T::from_root(r.spec, ty),
        }
    }
}

// We want to implement deref methods only for some of the possible
// generic parameter T of NamedReference, for which we use a macro.
// It would be nice to have a less "jank" way to do this if I can come
// up with it...
macro_rules! impl_named_reference {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl<'a> NamedReferenceView<'a, $ty> {
                /// Resolve the referenced type.
                pub fn typ(&self) -> $ty {
                    resolve_named_reference(self)
                }

                /// Resolve named reference until non-reference type.
                ///
/// For example, if we have:
/// ```text
/// type Foo: string
/// type Bar: Foo
/// ```
/// than `collapse` on `Bar` will return `string` type.
                ///
                /// This is different to [`Self::typ`], which returns named
                /// reference to `Foo` when called on `Bar`.
                pub fn collapse(&self) -> $ty {
                    collapse_named_reference(self)
                }
            }
        )+
    };
}

impl_named_reference!(
    SimpleTyView<'a>,
    TypeView<'a>,
    SimpleRecordReferenceView<'a>,
    ResponseReferenceView<'a>,
    RootTypeView<'a>,
);

/// Record field.
#[derive(Clone)]
pub struct RecordFieldView<'a> {
    pub name: &'a Name,
    pub default: Option<ValueView<'a>>,
    pub docs: Option<&'a str>,
    pub typ: InlineTyView<'a, TypeView<'a>>,
}

/// Field of a simple record
#[derive(Clone)]
pub struct SimpleRecordFieldView<'a> {
    pub(crate) id: TypeFieldId,

    pub name: &'a Name,
    pub default: Option<ValueView<'a>>,
    pub docs: Option<&'a str>,
    pub typ: InlineTyView<'a, SimpleTyView<'a>>,
}

/// Union field.
#[derive(Clone)]
pub struct UnionFieldView<'a> {
    pub name: &'a Name,
    pub docs: Option<&'a str>,
    pub typ: InlineTyView<'a, TypeView<'a>>,
}

/// Semantic representation of a record.
///
/// Fields are exposed through [`Record::fields`].
#[derive(Debug, Clone)]
pub struct RecordView<'a> {
    spec: &'a Spec,
}

impl<'a> RecordView<'a> {
    fn from_data(spec: &'a Spec, data: RecordRange) -> Self {
        Self { spec }
    }
    // /// Returns iterator over [record fields](RecordField).
    // pub fn fields(&self) -> impl Iterator<Item = RecordFieldView<'a>> + use<'a> {
    //     self.fields.clone().map(|it| it.into())
    // }
}

/// Semantic representation of a simple record.
///
/// Fields are exposed through [`SimpleRecord::fields`].
#[derive(Debug, Clone)]
pub struct SimpleRecordView<'a> {
    spec: &'a Spec,
}

impl<'a> SimpleRecordView<'a> {
    // /// Returns iterator over [simple record fields](SimpleRecordField).
    // pub fn fields(&self) -> impl Iterator<Item = SimpleRecordFieldView<'a>> + use<'a> {
    //     self.fields.clone().map(|it| it.into())
    // }
}

/// Semantic representation of a tagged union.
///
/// Fields are exposed through [`Union::fields`].
#[derive(Debug, Clone)]
pub struct UnionView<'a> {
    spec: &'a Spec,
}

impl<'a> UnionView<'a> {
    fn from_data(spec: &'a Spec, data: UnionRange) -> Self {
        Self { spec }
    }

    // /// Returns iterator over [union fields](UnionField).
    // pub fn fields(&self) -> impl Iterator<Item = UnionFieldView<'a>> + use<'a> {
    //     self.fields.clone().map(|it| it.into())
    // }
}

/// Semantic representation of an enum.
///
/// Field `typ` describes the type of enum values.
#[derive(Debug, Clone)]
pub struct EnumView<'a> {
    /// Type of the enum members
    pub typ: EnumTy,

    members: EnumMemberIter<'a>,
}

impl<'a> EnumView<'a> {
    fn from_data(spec: &'a Spec, data: EnumData) -> Self {
        Self {
            typ: data.typ,
            members: todo!(),
        }
    }

    pub fn members(&self) -> EnumMemberIter<'a> {
        self.members.clone()
    }
}

/// A single valid value of an enum.
#[derive(Debug, Clone)]
pub struct EnumMember<'a> {
    pub value: ValueView<'a>,
    pub docs: Option<&'a str>,
}

/// Iterator over enum members
#[derive(derive_more::Debug, Clone)]
pub struct EnumMemberIter<'a> {
    #[debug(skip)]
    spec: &'a Spec,

    current: TypeId,
    end: TypeId,
}

/// Semantic information about a response type.
#[derive(Debug, Clone)]
pub struct ResponseTyView<'a> {
    /// Type of response body
    pub body: InlineTyView<'a, TypeView<'a>>,

    /// Optional headers
    pub headers: Option<NamedReferenceView<'a, SimpleRecordReferenceView<'a>>>,

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

    NamedReference(NamedReferenceView<'a, RootTypeView<'a>>),
}

impl<'a> RootTypeView<'a> {
    fn new(spec: &'a Spec, id: RootTypeId) -> Self {
        match spec.types.get_root_type(id) {
            RootTypeData::Type(data) => Self::Type(TypeView::from_data(spec, data)),
            RootTypeData::Response(data) => Self::Response(ResponseTyView::from_data(spec, data)),
            RootTypeData::Reference(data) => {
                Self::NamedReference(NamedReferenceView::new(spec, data))
            }
        }
    }
}

impl<'a> FromRootTy<'a> for RootTypeView<'a> {
    fn from_root(_spec: &'a Spec, ty: RootTypeView<'a>) -> Self {
        ty
    }
}

/// Reference that terminates with response type.
///
/// This type accommodates deep references as well, like Foo -> Bar -> Baz -> resp
/// Usually it's wrapped in a `NamedReference`, ie `NamedReference<ResponseReference>`. This
/// way types guarantee that you don't have a direct ResponseTy without a reference in between.
#[derive(Debug, Clone, derive_more::Display)]
pub enum ResponseReferenceView<'a> {
    #[display("`response`")]
    Response(ResponseTyView<'a>),
    NamedReference(NamedReferenceView<'a, ResponseReferenceView<'a>>),
}

impl<'a> ResponseReferenceView<'a> {
    fn new(spec: &'a Spec, id: ResponseReferenceId) -> NamedReferenceView<'a, Self> {
        let ty = RootTypeView::new(spec, id.into());
        NamedReferenceView::from_root(spec, ty)
    }
}

impl<'a> FromRootTy<'a> for ResponseReferenceView<'a> {
    fn from_root(_spec: &'a Spec, ty: RootTypeView<'a>) -> Self {
        match ty {
            RootTypeView::Response(resp) => Self::Response(resp),
            RootTypeView::NamedReference(r) => Self::NamedReference(r.cast()),

            RootTypeView::Type(TypeView::Inline(InlineTyView::NamedReference(r))) => {
                Self::NamedReference(r.cast())
            }
            RootTypeView::Type(_) => {
                unreachable!("invalid Type slot for expected ResponseReferenceView")
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
pub enum SimpleRecordReferenceView<'a> {
    #[display("`record`")]
    SimpleRecord(SimpleRecordView<'a>),
    NamedReference(NamedReferenceView<'a, SimpleRecordReferenceView<'a>>),
}

impl<'a> FromRootTy<'a> for SimpleRecordReferenceView<'a> {
    fn from_root(spec: &'a Spec, ty: RootTypeView<'a>) -> Self {
        match ty {
            RootTypeView::Type(TypeView::Record(_)) => {
                Self::SimpleRecord(SimpleRecordView { spec })
            }
            RootTypeView::Type(TypeView::Inline(InlineTyView::NamedReference(r))) => {
                Self::NamedReference(r.cast())
            }
            RootTypeView::Type(_) => {
                unreachable!("invalid Type slot for expected SimpleRecordReferenceView")
            }

            RootTypeView::NamedReference(r) => Self::NamedReference(r.cast()),
            RootTypeView::Response(_) => {
                unreachable!("invalid Response slot for expected SimpleRecordReferenceView")
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

/// Helper trait for resolving references.
///
/// It differs from standard `From` trait
/// in that just because type implements it, it doesn't mean you should call it.
/// Basically, when calling it, you should expect arena is constructed correctly
/// and the context you are using it is correct.
trait FromRootTy<'a> {
    fn from_root(spec: &'a Spec, ty: RootTypeView<'a>) -> Self;
}
