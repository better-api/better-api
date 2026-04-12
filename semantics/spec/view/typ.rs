use crate::spec::arena::typ::arena::EnumCursor;
use crate::spec::arena::typ::arena::RecordCursor;
use crate::spec::arena::typ::arena::UnionCursor;
use crate::spec::arena::typ::arena::{
    EnumData, InlineTypeData, RecordRange, ReferenceData, ResponseData, RootTypeData, TypeData,
    UnionRange,
};
use crate::spec::arena::typ::id::{InlineTypeId, ResponseTypeId, RootTypeId, TypeId};
pub use crate::spec::arena::typ::{EnumTy, PrimitiveTy};
use crate::spec::view::value::ValueView;
use crate::spec::Spec;
use crate::text::Name;

/// Inline type tree.
///
/// Primitive values are stored directly. Arrays and options wrap another
/// inline type, and named references point to non-response types.
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
            InlineTypeData::Array(arr) => Self::Array(ReferenceView::new(spec, arr.inner_id)),
            InlineTypeData::Reference(r) => {
                Self::NamedReference(NamedTypeRefView::from_data(spec, r))
            }
        }
    }
}

/// Any non-response type.
///
/// The full set of possible types, including responses and root references,
/// is [`RootTypeView`].
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

    pub fn typ(&self) -> TypeView<'a> {
        TypeView::new(self.spec, self.id)
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

    pub fn typ(&self) -> RootTypeView<'a> {
        RootTypeView::new(self.spec, self.id)
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
/// Fields are exposed through [`RecordView::fields`].
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
/// Fields are exposed through [`UnionView::fields`].
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
            headers: None,
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

#[cfg(test)]
mod test {
    use super::{InlineTyView, RootTypeView, TypeView};
    use crate::spec::arena::typ::builder::{RootRef, TypeArenaBuilder, TypeRef};
    use crate::spec::arena::typ::{EnumTy, PrimitiveTy, TypeDefData};
    use crate::spec::arena::value::PrimitiveValue;
    use crate::spec::Spec;
    use crate::text::NameId;

    #[test]
    fn builds_nested_types() {
        let mut spec = Spec::new_test();
        let mut builder = TypeArenaBuilder::default();

        // Builds:
        // - record Root { id: i64, simple_array: [f32], values: [[[string?]]], metadata: [[bool]?] }
        // - union { success: bool, error: string }

        let id_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("id")) };
        let simple_array_name =
            unsafe { NameId::from_string_id(spec.strings.get_or_intern("simple_array")) };
        let values_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("values")) };
        let metadata_name =
            unsafe { NameId::from_string_id(spec.strings.get_or_intern("metadata")) };
        let unused_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("unused")) };
        let success_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("success")) };
        let error_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("error")) };

        let mut root = builder.start_record();
        root.add_primitive(id_name, PrimitiveTy::I64, None, None);

        let (simple_array, _) = root.start_array(simple_array_name, None, None);
        simple_array.finish_primitive(PrimitiveTy::F32);

        let (mut values_builder, _) = root.start_array(values_name, None, None);
        values_builder.start_array();
        values_builder.start_array();
        values_builder.start_option();
        values_builder.finish_primitive(PrimitiveTy::String);

        {
            let _ = root.start_array(unused_name, None, None);
        }

        let (mut metadata_builder, _) = root.start_array(metadata_name, None, None);
        metadata_builder.start_array();
        metadata_builder.start_option();
        metadata_builder.finish_primitive(PrimitiveTy::Bool);

        let root_id = root.finish();

        let mut union_builder = builder.start_union();
        union_builder.add_primitive(success_name, PrimitiveTy::Bool, None, None);
        union_builder.add_primitive(error_name, PrimitiveTy::String, None, None);
        let union_id = union_builder.finish();

        spec.types = builder
            .finish(&spec.symbol_table)
            .expect("type arena should build");

        let root = match TypeView::new(&spec, root_id) {
            TypeView::Record(record) => record,
            other => panic!("expected record at root_id, got {other:?}"),
        };

        let mut fields = root.fields();

        let id_field = fields.next().expect("id field");
        assert_eq!(id_field.name.as_str(), "id");
        assert!(matches!(
            id_field.typ,
            InlineTyView::Primitive(PrimitiveTy::I64)
        ));
        assert!(id_field.default.is_none());

        let simple_array = fields.next().expect("simple_array field");
        assert_eq!(simple_array.name.as_str(), "simple_array");
        match simple_array.typ {
            InlineTyView::Array(inner) => {
                assert!(matches!(
                    inner.typ(),
                    InlineTyView::Primitive(PrimitiveTy::F32)
                ));
            }
            other => panic!("expected array type, got {other:?}"),
        }

        let values = fields.next().expect("values field");
        assert_eq!(values.name.as_str(), "values");
        let level_1 = match values.typ {
            InlineTyView::Array(inner) => inner.typ(),
            other => panic!("expected array at level 1, got {other:?}"),
        };
        let level_2 = match level_1 {
            InlineTyView::Array(inner) => inner.typ(),
            other => panic!("expected array at level 2, got {other:?}"),
        };
        let level_3 = match level_2 {
            InlineTyView::Array(inner) => inner.typ(),
            other => panic!("expected array at level 3, got {other:?}"),
        };
        let option_inner = match level_3 {
            InlineTyView::Option(inner) => inner.typ(),
            other => panic!("expected option, got {other:?}"),
        };
        assert!(matches!(
            option_inner,
            InlineTyView::Primitive(PrimitiveTy::String)
        ));

        let metadata = fields.next().expect("metadata field");
        assert_eq!(metadata.name.as_str(), "metadata");
        let metadata_level_1 = match metadata.typ {
            InlineTyView::Array(inner) => inner.typ(),
            other => panic!("expected array at metadata level 1, got {other:?}"),
        };
        let metadata_level_2 = match metadata_level_1 {
            InlineTyView::Array(inner) => inner.typ(),
            other => panic!("expected array at metadata level 2, got {other:?}"),
        };
        let metadata_inner = match metadata_level_2 {
            InlineTyView::Option(inner) => inner.typ(),
            other => panic!("expected option in metadata field, got {other:?}"),
        };
        assert!(matches!(
            metadata_inner,
            InlineTyView::Primitive(PrimitiveTy::Bool)
        ));

        assert!(fields.next().is_none());

        let union = match TypeView::new(&spec, union_id) {
            TypeView::Union(union) => union,
            other => panic!("expected union at union_id, got {other:?}"),
        };

        let mut fields = union.fields();
        let success = fields.next().expect("success field");
        assert_eq!(success.name.as_str(), "success");
        assert!(matches!(
            success.typ,
            InlineTyView::Primitive(PrimitiveTy::Bool)
        ));

        let error = fields.next().expect("error field");
        assert_eq!(error.name.as_str(), "error");
        assert!(matches!(
            error.typ,
            InlineTyView::Primitive(PrimitiveTy::String)
        ));
        assert!(fields.next().is_none());
    }

    #[test]
    fn named_refs() {
        let mut spec = Spec::new_test();
        let mut builder = TypeArenaBuilder::default();

        // Builds:
        // - type Foo = string
        // - type Bar = record { name: Foo }

        let foo_name = spec.strings.get_or_intern("Foo");
        let name_field = unsafe { NameId::from_string_id(spec.strings.get_or_intern("name")) };

        let foo_id = builder.add_primitive(PrimitiveTy::String);
        spec.symbol_table.insert(
            foo_name,
            TypeDefData {
                docs: None,
                typ: foo_id.into(),
                name: foo_name,
            },
        );

        let mut bar_builder = builder.start_record();
        bar_builder.add_reference(name_field, TypeRef(foo_name), None, None);
        let bar_id = bar_builder.finish();

        spec.types = builder
            .finish(&spec.symbol_table)
            .expect("type arena should resolve references");

        let record = match TypeView::new(&spec, bar_id) {
            TypeView::Record(record) => record,
            other => panic!("expected record type for Bar, got {other:?}"),
        };

        let mut fields = record.fields();
        let name = fields.next().expect("name field");
        assert!(fields.next().is_none());

        match name.typ {
            InlineTyView::NamedReference(reference) => {
                assert_eq!(reference.name, "Foo");
                assert!(matches!(
                    reference.typ(),
                    TypeView::Inline(InlineTyView::Primitive(PrimitiveTy::String))
                ));
            }
            other => panic!("expected named reference, got {other:?}"),
        }
    }

    #[test]
    fn simple_record_with_enum_option() {
        let mut spec = Spec::new_test();
        let mut builder = TypeArenaBuilder::default();

        // Builds:
        // - type Status = enum string { "open", "closed" }
        // - record { id: i64, title: string, status: Status? }

        let status_name = spec.strings.get_or_intern("Status");
        let open_str = spec.strings.get_or_intern("open");
        let closed_str = spec.strings.get_or_intern("closed");
        let id_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("id")) };
        let title_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("title")) };
        let status_field = unsafe { NameId::from_string_id(spec.strings.get_or_intern("status")) };

        let open_value = spec.values.add_primitive(PrimitiveValue::String(open_str));
        let closed_value = spec
            .values
            .add_primitive(PrimitiveValue::String(closed_str));

        let mut enum_builder = builder.start_enum(EnumTy::String);
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

        let mut record_builder = builder.start_record();
        record_builder.add_primitive(id_name, PrimitiveTy::I64, None, None);
        record_builder.add_primitive(title_name, PrimitiveTy::String, None, None);
        let (status_builder, _) = record_builder.start_option(status_field, None, None);
        status_builder.finish_reference(TypeRef(status_name));
        let record_id = record_builder.finish();

        spec.types = builder
            .finish(&spec.symbol_table)
            .expect("type arena should resolve references");

        let record = match TypeView::new(&spec, record_id) {
            TypeView::Record(record) => record,
            other => panic!("expected record, got {other:?}"),
        };

        let mut fields = record.fields();

        let id = fields.next().expect("id field");
        assert_eq!(id.name.as_str(), "id");
        assert!(matches!(id.typ, InlineTyView::Primitive(PrimitiveTy::I64)));

        let title = fields.next().expect("title field");
        assert_eq!(title.name.as_str(), "title");
        assert!(matches!(
            title.typ,
            InlineTyView::Primitive(PrimitiveTy::String)
        ));

        let status = fields.next().expect("status field");
        assert_eq!(status.name.as_str(), "status");
        let option_inner = match status.typ {
            InlineTyView::Option(inner) => inner.typ(),
            other => panic!("expected option for status field, got {other:?}"),
        };

        let status_enum = match option_inner {
            InlineTyView::NamedReference(reference) => {
                assert_eq!(reference.name, "Status");
                match reference.typ() {
                    TypeView::Enum(enm) => enm,
                    other => panic!("expected enum for status reference, got {other:?}"),
                }
            }
            other => panic!("expected named reference for status field, got {other:?}"),
        };

        let mut members = status_enum.members();
        let open = members.next().expect("open enum member");
        assert!(matches!(
            open.value,
            crate::spec::view::value::ValueView::String("open")
        ));
        let closed = members.next().expect("closed enum member");
        assert!(matches!(
            closed.value,
            crate::spec::view::value::ValueView::String("closed")
        ));
        assert!(members.next().is_none());
        assert!(fields.next().is_none());
    }

    #[test]
    fn root_reference_response_enum_body() {
        let mut spec = Spec::new_test();
        let mut builder = TypeArenaBuilder::default();

        // Builds:
        // - type Enum = enum string { "ok", "error" }
        // - type StatusResponse = response { body: Enum }
        // - root reference to StatusResponse

        let enum_name = spec.strings.get_or_intern("Enum");
        let response_name = spec.strings.get_or_intern("StatusResponse");
        let ok_str = spec.strings.get_or_intern("ok");
        let error_str = spec.strings.get_or_intern("error");

        let ok_value = spec.values.add_primitive(PrimitiveValue::String(ok_str));
        let error_value = spec.values.add_primitive(PrimitiveValue::String(error_str));

        let mut enum_builder = builder.start_enum(EnumTy::String);
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

        let enum_ref_id = builder.add_reference(RootRef(enum_name));
        let response_id = builder.add_response(
            crate::spec::arena::typ::id::InlineTypeId::from_root_type_id(enum_ref_id),
            None,
            None,
        );
        spec.symbol_table.insert(
            response_name,
            TypeDefData {
                docs: None,
                typ: response_id.into(),
                name: response_name,
            },
        );

        let response_ref_id = builder.add_reference(RootRef(response_name));
        spec.types = builder
            .finish(&spec.symbol_table)
            .expect("type arena should resolve references");

        let response = match RootTypeView::new(&spec, response_ref_id) {
            RootTypeView::NamedReference(reference) => match reference.typ() {
                RootTypeView::Response(response) => response,
                other => panic!("expected response type, got {other:?}"),
            },
            other => panic!("expected named root reference, got {other:?}"),
        };

        assert!(matches!(response.body, InlineTyView::NamedReference(_)));
        let status_enum = match response.body {
            InlineTyView::NamedReference(reference) => match reference.typ() {
                TypeView::Enum(enm) => enm,
                other => panic!("expected enum response body, got {other:?}"),
            },
            other => panic!("expected named reference response body, got {other:?}"),
        };
        assert_eq!(status_enum.typ, EnumTy::String);

        let mut members = status_enum.members();
        let ok = members.next().expect("ok enum member");
        assert!(matches!(
            ok.value,
            crate::spec::view::value::ValueView::String("ok")
        ));
        let error = members.next().expect("error enum member");
        assert!(matches!(
            error.value,
            crate::spec::view::value::ValueView::String("error")
        ));
        assert!(members.next().is_none());
        assert!(response.headers.is_none());
        assert!(response.content_type.is_none());
    }
}
