use crate::spec::arena::typ::PrimitiveTy;

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

/// A named reference to another type.
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

fn resolve_named_reference<'a, T: FromSlot<'a>>(r: &NamedReference<'a, T>) -> T {
    r.ctx.resolve_from_slot(r.id)
}

fn collapse_named_reference<'a, T: FromSlot<'a>>(r: &NamedReference<'a, T>) -> T {
    let mut id = r.id;

    loop {
        let slot = &r.ctx.types.data[id as usize];
        match slot {
            Slot::Reference(name_id) => {
                let def = r
                    .ctx
                    .symbol_table
                    .get(name_id)
                    .expect("symbol table and type arena should be valid");
                id = def.typ.0;
            }
            _ => return T::from_slot(r.ctx, id, slot),
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
            impl<'a> NamedReference<'a, $ty> {
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
    SimpleTy<'a>,
    Type<'a>,
    SimpleRecordReference<'a>,
    ResponseReference<'a>,
    RootType<'a>,
);

/// Internal representation of record and union field.
struct TypeFieldInner<'a, T> {
    id: TypeFieldId,
    name: &'a Name,
    default: Option<Value<'a>>,
    docs: Option<&'a str>,
    typ: InlineTy<'a, T>,
}

/// Record field.
#[derive(Clone)]
pub struct RecordField<'a> {
    pub name: &'a Name,
    pub default: Option<Value<'a>>,
    pub docs: Option<&'a str>,
    pub typ: InlineTy<'a, Type<'a>>,
}

impl<'a> From<TypeFieldInner<'a, Type<'a>>> for RecordField<'a> {
    fn from(value: TypeFieldInner<'a, Type<'a>>) -> Self {
        Self {
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

    pub name: &'a Name,
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
    pub name: &'a Name,
    pub docs: Option<&'a str>,
    pub typ: InlineTy<'a, Type<'a>>,
}

impl<'a> From<TypeFieldInner<'a, Type<'a>>> for UnionField<'a> {
    fn from(value: TypeFieldInner<'a, Type<'a>>) -> Self {
        Self {
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
    fields: TypeFieldIterator<'a, Type<'a>>,
}

impl<'a> Union<'a> {
    /// Returns iterator over [union fields](UnionField).
    pub fn fields(&self) -> impl Iterator<Item = UnionField<'a>> + use<'a> {
        self.fields.clone().map(|it| it.into())
    }
}

/// Semantic representation of an enum.
///
/// Field `typ` describes the type of enum values.
#[derive(Debug, Clone)]
pub struct Enum<'a> {
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

/// Any type including response.
#[derive(Debug, Clone, derive_more::Display)]
pub enum RootType<'a> {
    #[display("`response`")]
    Response(ResponseTy<'a>),
    Type(Type<'a>),

    NamedReference(NamedReference<'a, RootType<'a>>),
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

/// Type definition.
pub struct TypeDef<'a> {
    pub docs: Option<&'a str>,
    pub typ: RootType<'a>,
    pub name: &'a str,
}
