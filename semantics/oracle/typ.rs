use better_api_diagnostic::{Label, Report};
use better_api_syntax::ast::AstNode;
use better_api_syntax::{TextRange, ast};

use crate::oracle::SymbolMap;
use crate::oracle::symbols::{ResolvedSymbol, deref, report_missing, resolve};
use crate::oracle::value::{lower_mime_types, lower_value};
use crate::spec::typ::{
    EnumTy, FieldBuilder, InlineTyId, OptionArrayBuilder, PrimitiveTy, ResponseTyId, RootRef,
    RootTypeId, SimpleRecordReferenceId, TypeDefData, TypeId, TypeRef,
};
use crate::spec::value::ValueId;
use crate::string::{StringId, StringInterner};
use crate::text::lower_name;

use super::Oracle;

/// Represents type field with interned name.
#[derive(Clone)]
struct InternedField {
    name: StringId,
    field: ast::TypeField,

    // Prologue
    default: Option<ValueId>,
    docs: Option<StringId>,
}

/// Semantic wrapper for fields parsed by [`Oracle::parse_type_fields`].
#[derive(Clone)]
#[repr(transparent)]
struct ParsedFields(Vec<InternedField>);

/// Helper type for handling primitive types separately from composite ones.
enum ParsedType<'a> {
    Primitive(PrimitiveTy),
    Reference { name: StringId, range: TextRange },
    Enum(&'a ast::Enum),
    Response,
    Record(&'a ast::Record),
    Union(&'a ast::Union),
    Array(&'a ast::TypeArray),
    Option(&'a ast::TypeOption),
}

impl<'a> ParsedType<'a> {
    fn new(typ: &'a ast::Type, strings: &mut StringInterner) -> Self {
        match typ {
            ast::Type::TypeOption(opt) => Self::Option(opt),
            ast::Type::TypeArray(arr) => Self::Array(arr),
            ast::Type::Record(rec) => Self::Record(rec),
            ast::Type::Enum(en) => Self::Enum(en),
            ast::Type::Union(union) => Self::Union(union),
            ast::Type::TypeResponse(_) => Self::Response,
            ast::Type::TypeRef(typ_ref) => {
                let token = typ_ref.name();
                let str_id = strings.get_or_intern(token.text());

                Self::Reference {
                    name: str_id,
                    range: typ.syntax().text_range(),
                }
            }
            ast::Type::TypeI32(_) => Self::Primitive(PrimitiveTy::I32),
            ast::Type::TypeI64(_) => Self::Primitive(PrimitiveTy::I64),
            ast::Type::TypeU32(_) => Self::Primitive(PrimitiveTy::U32),
            ast::Type::TypeU64(_) => Self::Primitive(PrimitiveTy::U64),
            ast::Type::TypeF32(_) => Self::Primitive(PrimitiveTy::F32),
            ast::Type::TypeF64(_) => Self::Primitive(PrimitiveTy::F64),
            ast::Type::TypeDate(_) => Self::Primitive(PrimitiveTy::Date),
            ast::Type::TypeTimestamp(_) => Self::Primitive(PrimitiveTy::Timestamp),
            ast::Type::TypeBool(_) => Self::Primitive(PrimitiveTy::Bool),
            ast::Type::TypeString(_) => Self::Primitive(PrimitiveTy::String),
            ast::Type::TypeFile(_) => Self::Primitive(PrimitiveTy::File),
        }
    }
}

impl<'a> Oracle<'a> {
    /// Lowers type definitions.
    pub(crate) fn lower_type_definitions(&mut self) {
        for def in self.root.type_definitions() {
            self.lower_type_def(&def);
        }
    }

    /// Lowers type definition node.
    fn lower_type_def(&mut self, def: &ast::TypeDefinition) {
        // Missing name error is reported by parser.
        let Some(name_token) = def.name() else {
            return;
        };
        let name = name_token.text();
        let name_id = self.strings.get_or_intern(name);

        // Missing type report is handled by parser. Other errors are handled in lower_type.
        let Some(type_id) = def.typ().and_then(|t| match t {
            ast::Type::TypeResponse(resp) => self.lower_response(&resp, name).map(|id| id.into()),
            _ => self.lower_type(&t),
        }) else {
            return;
        };

        // Insert type to symbol table if not already present.
        // The duplicate type definition error is reported by [`Oracle::validate_symbols`].
        self.spec_symbol_table
            .entry(name_id)
            .or_insert(TypeDefData {
                typ: type_id,
                name: name_id,

                // TODO: Extract docs from type definition prologue
                docs: None,
            });
    }

    /// Lower a type that isn't a response.
    ///
    /// Returns [`RootTypeId`] if type could be parsed, and `None` otherwise.
    ///
    /// ## Panics
    ///
    /// This method mustn't be called on [`ast::TypeResponse`]. If it's called
    /// on `ast::TypeResponse`, it will panic.
    ///
    /// Use [`Oracle::lower_response`] for lowering response.
    ///
    /// **Note:** You can should use this method to lower a _reference_ to a response.
    pub(crate) fn lower_type(&mut self, typ: &ast::Type) -> Option<RootTypeId> {
        match ParsedType::new(typ, &mut self.strings) {
            ParsedType::Primitive(primitive) => Some(self.types.add_primitive(primitive).into()),
            ParsedType::Reference { name, range } => {
                match validate_reference(
                    &self.strings,
                    &self.symbol_map,
                    self.root,
                    &mut self.reports,
                    name,
                    range,
                ) {
                    None => None,
                    Some(typ) => Some(self.types.add_reference(typ.into())),
                }
            }
            ParsedType::Enum(en) => self.lower_enum(en).map(|id| id.into()),
            ParsedType::Record(record) => Some(self.lower_record(record).into()),
            ParsedType::Union(union) => self.lower_union(union).map(|id| id.into()),
            ParsedType::Array(arr) => {
                let inner = arr.typ()?;
                let builder = self.types.start_array();
                lower_array_option(
                    &inner,
                    builder,
                    &mut self.reports,
                    &mut self.strings,
                    &self.symbol_map,
                    self.root,
                    InvalidOuterContext::Array,
                )
                .map(|id| id.into())
            }
            ParsedType::Option(opt) => {
                let inner = opt.typ()?;
                let builder = self.types.start_option();
                lower_array_option(
                    &inner,
                    builder,
                    &mut self.reports,
                    &mut self.strings,
                    &self.symbol_map,
                    self.root,
                    InvalidOuterContext::Option,
                )
                .map(|id| id.into())
            }

            ParsedType::Response => {
                panic!("trying to lower response with `lower_type`. Use lower_response instead")
            }
        }
    }

    /// Check if type is simple by resolving the references.
    ///
    /// For instance:
    ///
    /// ```text
    /// type Foo: string
    /// type Bar: Foo
    /// ```
    ///
    /// are both simple types
    pub(crate) fn is_simple_type(
        &self,
        typ: &ast::Type,
        allow_option: bool,
        allow_array: bool,
    ) -> Result<bool, Report> {
        match typ {
            ast::Type::TypeI32(_)
            | ast::Type::TypeI64(_)
            | ast::Type::TypeU32(_)
            | ast::Type::TypeU64(_)
            | ast::Type::TypeF32(_)
            | ast::Type::TypeF64(_)
            | ast::Type::TypeDate(_)
            | ast::Type::TypeTimestamp(_)
            | ast::Type::TypeBool(_)
            | ast::Type::TypeString(_)
            | ast::Type::TypeFile(_) => Ok(true),

            // Enum is a simple type
            ast::Type::Enum(_) => Ok(true),

            ast::Type::Record(_) | ast::Type::Union(_) | ast::Type::TypeResponse(_) => Ok(false),

            // If option is invalid (has no type) parser already reports an error.
            // It would be bad UX if you also got an error for type not being simple, just because
            // you didn't finish writing an option type. Same goes for array.
            ast::Type::TypeOption(opt) => {
                if allow_option {
                    match opt.typ() {
                        None => Ok(true),
                        Some(typ) => self.is_simple_type(&typ, allow_option, allow_array),
                    }
                } else {
                    Ok(false)
                }
            }
            ast::Type::TypeArray(arr) => {
                if allow_array {
                    match arr.typ() {
                        None => Ok(true),
                        Some(typ) => self.is_simple_type(&typ, allow_option, allow_array),
                    }
                } else {
                    Ok(false)
                }
            }
            // If there is an error during type resolution, we don't want to report it
            // for the same reasons as option and array.
            ast::Type::TypeRef(reference) => match self.deref(reference) {
                None => Ok(true),
                Some(typ) => self.is_simple_type(&typ, allow_option, allow_array),
            },
        }
    }

    /// Lowers enum type and inserts it into arena
    fn lower_enum(&mut self, typ: &ast::Enum) -> Option<TypeId> {
        // Validate enum type
        let enum_type_ast = typ.typ()?;
        let enum_type = self.lower_enum_type(&enum_type_ast)?;

        // Parse enum members

        // Deref is used by a more generic value matches type function. We already checked that
        // enum_type (which we are matching) is one of the valid types. Since reference is not a
        // valid type, `deref` closure should never be called.
        let deref = |_: &ast::TypeRef| unreachable!("enum type should not be a reference");

        let mut builder = self.types.start_enum(enum_type);
        let mut is_valid = true;
        for member in typ.members() {
            let Some(value) = member.value() else {
                // Missing enum member value is reported by parser.
                is_valid = false;
                continue;
            };

            if !ast::value_matches_type(&value, &enum_type_ast, &mut self.reports, &deref) {
                is_valid = false;
                continue;
            }

            let value_id = lower_value(
                &mut self.values,
                &mut self.strings,
                &mut self.reports,
                &value,
            );

            // TODO: Get docs from prologue
            builder.add_member(value_id, None);
        }

        if is_valid {
            Some(builder.finish())
        } else {
            None
        }
    }

    /// Lowers response type.
    ///
    /// Returns [`RootTypeId`] if type could be parsed, and `None` otherwise.
    ///
    /// Parameter `name` is the name of the response type being lowered. It's used to generate
    /// header and body names.
    pub(crate) fn lower_response(
        &mut self,
        resp: &ast::TypeResponse,
        name: &str,
    ) -> Option<ResponseTyId> {
        // Is response valid. We don't want to early return, because
        // we want to validate as many things as possible and capture as many
        // errors as possible.
        //
        // In some checks for body we do want early return, since there is nothing more we can
        // do. Specifically, if body is not specified, or is a response, we have nothing else to
        // validate.
        let mut is_valid = true;

        // Parse and validate content type
        let content_type_id = resp.content_type().and_then(|v| v.value()).and_then(|v| {
            lower_mime_types(&mut self.values, &mut self.strings, &mut self.reports, &v)
        });

        // Parse and validate header type
        let mut headers_id = None;
        if let Some(headers) = resp.headers().and_then(|h| h.typ()) {
            match self.lower_headers(&headers, &format!("{name}Headers")) {
                res @ Some(_) => headers_id = res,
                None => is_valid = false,
            }
        }

        // Parse and validate response body
        let Some(body) = resp.body().and_then(|b| b.typ()) else {
            self.reports.push(
                Report::error("missing response body".to_string())
                    .add_label(Label::primary(
                        "missing response body".to_string(),
                        resp.syntax().text_range().into(),
                    ))
                    .with_note("help: `body` is a required response field".to_string()),
            );

            return None;
        };

        // Response body can't be response
        if self.require_not_response(&body).is_err() {
            return None;
        }

        // Does content type require the response body to be `file`
        let requires_file = content_type_id.is_some_and(|id| {
            self.spec_ctx()
                .get_mime_types(id)
                .any(|str| str != "application/json")
        });

        // Check that response body is `file` by also resolving references
        if requires_file {
            match self.require_resp_body_is_file(&body, resp.content_type().as_ref()) {
                Ok(_) => (),
                Err(_) => is_valid = false,
            }
        } else {
            // Check that response body is not file, and it does not contain a file (in case of
            // ie a record). This is done by resolving named references.
            //
            // Reports are generated by the helper function, so we can return early.
            let report_builder = |range: TextRange| {
                let mut report = Report::error("invalid response body type".to_string())
                    .add_label(Label::primary(
                        "content type requires that no `file` is present in body".to_string(),
                        body.syntax().text_range().into(),
                    ))
                    .add_label(Label::secondary(
                        "file introduced here".to_string(),
                        range.into(),
                    ))
                    .with_note(
                        "help: `application/json` response must not use `file` in body".to_string(),
                    );

                if let Some(content_type) = resp.content_type().as_ref() {
                    report = report.add_label(Label::secondary(
                        "content type defined here".to_string(),
                        content_type.syntax().text_range().into(),
                    ));
                }

                report
            };

            match self.require_no_file(&body, &report_builder) {
                Ok(_) => (),
                Err(_) => is_valid = false,
            }
        }

        // Finally we can lower the body. This also does basic type validation (ie record fields
        // are valid with correct defaults, ...). Since there are no more validation steps after
        // this, we can early return in case of errors.
        let body_id = self.lower_type(&body)?;

        // If there were any errors during validation, don't build the final type.
        if !is_valid {
            return None;
        }

        let body_id = self.ensure_inline(&body, body_id, &format!("{name}Body"))?;

        // Safety: We checked that body is not a response, and that it's inlined.
        let body_id = unsafe { InlineTyId::new_unchecked(body_id) };

        let id = self
            .types
            .add_response(body_id, headers_id, content_type_id);
        Some(id)
    }

    /// Checks that given type represents valid headers and lowers it.
    ///
    /// If necessary headers are put behind a new type definition, with provided name.
    ///
    /// Valid headers are simple record without files. If type is valid,
    /// Some(_) is returned. If any validation fails, reports are generated and None is returned.
    fn lower_headers(
        &mut self,
        headers: &ast::Type,
        name: &str,
    ) -> Option<SimpleRecordReferenceId> {
        // Are headers valid. We don't want to early return, because we want
        // to validate as many things as possible.
        let mut is_valid = true;

        let file_report_builder = |range: TextRange| {
            Report::error("invalid headers type".to_string())
                .add_label(Label::primary(
                    "headers type must not contain `file`".to_string(),
                    headers.syntax().text_range().into(),
                ))
                .add_label(Label::secondary(
                    "`file` introduced here".to_string(),
                    range.into(),
                ))
        };

        match self.require_no_file(headers, &file_report_builder) {
            Ok(_) => (),
            Err(_) => is_valid = false,
        }

        let simple_report_builder = |typ: SimpleRecordReportType| match typ {
            SimpleRecordReportType::NotStruct(resolved) => {
                Report::error("invalid header type".to_string())
                    .add_label(Label::primary(
                        format!("expected struct, got {resolved}"),
                        headers.syntax().text_range().into(),
                    ))
                    .with_note("help: headers must be a simple record".to_string())
            }
            SimpleRecordReportType::CompositeField(field) => {
                let range = field
                    .typ()
                    .map_or_else(|| field.syntax().text_range(), |t| t.syntax().text_range());

                Report::error("invalid header type".to_string())
                    .add_label(Label::primary(
                        "header fields can only be simple types or option".to_string(),
                        headers.syntax().text_range().into(),
                    ))
                    .add_label(Label::secondary(
                        "non-simple field type here".to_string(),
                        range.into(),
                    ))
            }
        };

        match self.require_simple_record(headers, true, false, simple_report_builder) {
            Ok(_) => (),
            Err(_) => is_valid = false,
        }

        // We don't lower type if it isn't a simple record. We don't want to report things
        // like union errors _and_ "it's not a simple record". This would be confusing.
        if !is_valid {
            return None;
        }

        let id = self.lower_type(headers)?;
        let id = self.ensure_inline(headers, id, name)?;

        // Safety: We have checked that it's a simple record, and that it's behind a reference.
        let id = unsafe { SimpleRecordReferenceId::new_unchecked(id) };
        Some(id)
    }

    /// Lowers record type and inserts it into arena
    fn lower_record(&mut self, record: &ast::Record) -> TypeId {
        let fields = self.parse_type_fields(record.fields(), true);
        let builder = self.types.start_record();
        insert_type_fields(
            fields,
            builder,
            &mut self.reports,
            &mut self.strings,
            &self.symbol_map,
            self.root,
            InvalidOuterContext::RecordField,
        )
    }

    /// Lowers union type and inserts it into arena
    fn lower_union(&mut self, union: &ast::Union) -> Option<TypeId> {
        let mut is_valid = true;
        let fields = self.parse_type_fields(union.fields(), false);

        // Validate the fields - they must not contain a `file`
        for field in &fields.0 {
            let typ = field.field.typ().expect("parsed field should have a type");
            let typ_range = typ.syntax().text_range();

            let report_builder = |range: TextRange| {
                let mut report = Report::error("invalid type of union field".to_string())
                    .add_label(Label::primary(
                        "union must not contain a `file`".to_string(),
                        typ_range.into(),
                    ));

                if typ_range != range {
                    report = report.add_label(Label::secondary(
                        "`file` introduced here".to_string(),
                        range.into(),
                    ))
                }

                report
            };

            let res = self.require_no_file(&typ, &report_builder);
            if res.is_err() {
                is_valid = false;
            }
        }

        // Build the union
        let builder = self.types.start_union();
        let id = insert_type_fields(
            fields,
            builder,
            &mut self.reports,
            &mut self.strings,
            &self.symbol_map,
            self.root,
            InvalidOuterContext::UnionField,
        );

        if is_valid { Some(id) } else { None }
    }

    /// Collects type fields into a vector.
    ///
    /// Only valid fields are collected. Field is valid if it has a valid name and a type that
    /// isn't a response. Field names are interned.
    ///
    /// Returned vector is sorted by name, which stabilizes the fields.
    fn parse_type_fields(
        &mut self,
        fields: impl Iterator<Item = ast::TypeField>,
        parse_default: bool,
    ) -> ParsedFields {
        let mut fields: Vec<_> = fields
            .filter_map(|f| {
                let typ = f.typ()?;

                // Lower name
                let name_id = f
                    .name()
                    .and_then(|n| lower_name(&n, &mut self.strings, &mut self.reports))?;

                // Get default from prologue
                let default = if parse_default {
                    f.prologue()
                        .and_then(|p| p.default())
                        .and_then(|d| d.value())
                } else {
                    // No need to report an error if default is present in field.
                    // It's already reported by the parser.
                    None
                };

                // Check type is valid
                let typ_valid = self.require_not_response(&typ).is_ok();

                // We only check that default value is valid when type is valid.
                let mut default_id = None;
                if typ_valid {
                    if let Some(val) = &default {
                        let deref = |node: &ast::TypeRef| {
                            deref(&self.strings, &self.symbol_map, self.root, node)
                        };
                        ast::value_matches_type(val, &typ, &mut self.reports, &deref);
                    }

                    // Lower default value
                    default_id = default.map(|val| self.lower_value(&val));
                }

                Some(InternedField {
                    name: name_id,
                    field: f,
                    default: default_id,

                    // TODO: Extract docs from field prologue
                    docs: None,
                })
            })
            .collect();

        fields.sort_by_key(|f| f.name);
        check_type_fields_unique(&fields, &mut self.reports, &self.strings);

        ParsedFields(fields)
    }

    /// Validates type of the enum (not enum itself).
    ///
    /// When declaring an enum, you do `enum (T) {...}`. This function validates
    /// that `T` is one of the allowed types. If it isn't a report is generated.
    fn lower_enum_type(&mut self, enum_type: &ast::Type) -> Option<EnumTy> {
        match enum_type {
            ast::Type::TypeI32(_) => Some(EnumTy::I32),
            ast::Type::TypeI64(_) => Some(EnumTy::I64),
            ast::Type::TypeU32(_) => Some(EnumTy::U32),
            ast::Type::TypeU64(_) => Some(EnumTy::U64),
            ast::Type::TypeString(_) => Some(EnumTy::String),

            typ => {
                let range = enum_type.syntax().text_range();
                self.reports.push(
                    Report::error(format!("invalid enum type `{typ}`"))
                        .add_label(Label::primary(
                            format!("invalid enum type `{typ}`"),
                            range.into(),
                        ))
                        .with_note(
                            "help: enum must have a type `i32`, `i64`, `u32`, `u64`, or `string`"
                                .to_string(),
                        ),
                );

                None
            }
        }
    }

    /// Requires that response body is a file or reference to a file.
    ///
    /// Reports are added to self.reports on the fly.
    ///
    /// If no reports are generated (node is valid), Ok is returned, otherwise Err.
    fn require_resp_body_is_file(
        &mut self,
        node: &ast::Type,
        content_type: Option<&ast::ResponseContentType>,
    ) -> Result<(), ()> {
        let check = |derefed: &ast::Type| {
            if matches!(derefed, ast::Type::TypeFile(_)) {
                Ok(())
            } else {
                let mut report = Report::error("invalid response body type".to_string())
                    .add_label(Label::primary(
                        "content type requires `file` body".to_string(),
                        node.syntax().text_range().into(),
                    ))
                    .with_note(
                        "help: non `application/json` responses must use `file` as body"
                            .to_string(),
                    );

                if let Some(content_type) = content_type {
                    report = report.add_label(Label::secondary(
                        "content type defined here".to_string(),
                        content_type.syntax().text_range().into(),
                    ));
                }

                Err(report)
            }
        };

        match self.require_with_deref(node, check) {
            Ok(_) => Ok(()),
            Err(None) => Err(()),
            Err(Some(report)) => {
                self.reports.push(report);
                Err(())
            }
        }
    }

    /// Requires that node is not a file and it does not contain any children that are `file`.
    ///
    /// The check is recursive. If a node is a composite type (like record) and one of the fields
    /// is a `file`, the method does report errors. It also resolves references.
    ///
    /// Union is a special case. Union can _never_ contain a file. Each field is verified
    /// `lower_union`. To not report errors multiple times per union, this check stops with
    /// `Ok(())` on `ast::TypeUnion`. This also means that `lower_union` needs to call the
    /// check per field, instead of the whole union node.
    ///
    /// Reports are added to self.reports on the fly.
    ///
    /// If no reports are generated (node is valid), Ok is returned, otherwise Err.
    fn require_no_file<R>(&mut self, node: &ast::Type, build_report: &R) -> Result<(), ()>
    where
        R: Fn(TextRange) -> Report,
    {
        match node {
            ast::Type::TypeI32(_)
            | ast::Type::TypeI64(_)
            | ast::Type::TypeU32(_)
            | ast::Type::TypeU64(_)
            | ast::Type::TypeF32(_)
            | ast::Type::TypeF64(_)
            | ast::Type::TypeDate(_)
            | ast::Type::TypeTimestamp(_)
            | ast::Type::TypeBool(_)
            | ast::Type::TypeString(_) => Ok(()),

            // File is not a valid enum type
            ast::Type::Enum(_) => Ok(()),

            ast::Type::TypeOption(opt) => match opt.typ() {
                Some(typ) => self.require_no_file(&typ, build_report),
                None => Err(()),
            },
            ast::Type::TypeArray(arr) => match arr.typ() {
                Some(typ) => self.require_no_file(&typ, build_report),
                None => Err(()),
            },

            ast::Type::Record(record) => {
                let valid = record.fields().all(|field| {
                    field
                        .typ()
                        .is_none_or(|typ| self.require_no_file(&typ, build_report).is_ok())
                });

                if valid { Ok(()) } else { Err(()) }
            }

            // Union can _never_ contain a file. This is checked in `lower_union`. Because
            // we don't want to report an error multiple times, we do an early return here.
            ast::Type::Union(_) => Ok(()),

            // There are only two ways to get here:
            // - call this method directly on a response: we have a bug in `lower_response`
            // - one of the composite types contains a response: during lowering of that type the
            //   error is reported
            //
            // In any case, we don't have to report an error here
            ast::Type::TypeResponse(_) => Err(()),

            ast::Type::TypeRef(reference) => match self.deref(reference) {
                None => Err(()),
                Some(typ) => self.require_no_file(&typ, build_report),
            },

            ast::Type::TypeFile(_) => {
                self.reports.push(build_report(node.syntax().text_range()));
                Err(())
            }
        }
    }

    /// Requires that node is a record and all fields are primitive type.
    ///
    /// Reports are added to self.reports on the fly.
    ///
    /// If node is valid, Ok is returned, otherwise Err
    fn require_simple_record<R>(
        &mut self,
        node: &ast::Type,
        allow_option: bool,
        allow_array: bool,
        build_report: R,
    ) -> Result<(), ()>
    where
        R: Fn(SimpleRecordReportType) -> Report,
    {
        let fields = match node {
            ast::Type::Record(rec) => rec.fields(),
            ast::Type::TypeRef(reference) => match self.deref(reference) {
                None => return Err(()),
                Some(ast::Type::Record(rec)) => rec.fields(),
                Some(typ) => {
                    let report = build_report(SimpleRecordReportType::NotStruct(&typ));
                    self.reports.push(report);
                    return Err(());
                }
            },

            typ => {
                let report = build_report(SimpleRecordReportType::NotStruct(typ));
                self.reports.push(report);
                return Err(());
            }
        };

        let mut is_valid = true;
        for field in fields {
            let Some(typ) = field.typ() else {
                continue;
            };

            match self.is_simple_type(&typ, allow_option, allow_array) {
                Ok(true) => (),
                Ok(false) => {
                    let report = build_report(SimpleRecordReportType::CompositeField(&field));
                    self.reports.push(report);
                    is_valid = false;
                }
                Err(report) => {
                    self.reports.push(report);
                    is_valid = false;
                }
            }
        }

        if is_valid { Ok(()) } else { Err(()) }
    }

    /// Requires that node is not a response type by resolving references.
    ///
    /// The check is **not** recursive. If a node is composite type (like record) and one of the
    /// fields is a response, this method will not report an error. The reason why the check does
    /// not need to be recursive is, that this is an invariant. No type can contain response as a
    /// child. When lowering type A, we check it doesn't have a response. When lowering type B
    /// depending on A, we have now already checked A so we don't have to perform the same check on
    /// B recursively. This is different to [`Self::require_no_file`].
    ///
    /// Reports are added to self.reports on the fly.
    ///
    /// If no reports are generated (node is valid), Ok is returned, otherwise Err.
    fn require_not_response(&mut self, node: &ast::Type) -> Result<(), ()> {
        let check = |derefed: &ast::Type| {
            if !matches!(derefed, ast::Type::TypeResponse(_)) {
                Ok(())
            } else {
                let range = node.syntax().text_range();
                let deref_range = derefed.syntax().text_range();

                let report = new_invalid_response(range, Some(deref_range));
                Err(report)
            }
        };

        match self.require_with_deref(node, check) {
            Ok(_) => Ok(()),
            Err(None) => Err(()),
            Err(Some(report)) => {
                self.reports.push(report);
                Err(())
            }
        }
    }

    /// Requires that check C succeeds for given node. If node is a reference,
    /// it dereferences it first.
    fn require_with_deref<C>(&self, node: &ast::Type, check: C) -> Result<(), Option<Report>>
    where
        C: Fn(&ast::Type) -> Result<(), Report>,
    {
        match &node {
            ast::Type::TypeRef(reference) => match self.deref(reference) {
                None => Err(None),
                Some(typ) => check(&typ).map_err(Some),
            },

            _ => check(node).map_err(Some),
        }
    }

    /// Ensures the type is an inline type.
    ///
    /// If type behind `node` is not already an inline type, its assigned a new type definition
    /// with name `name`. Parameter `id` should be the id of the lowered type behind `node`.
    ///
    /// If type can't be inlined (usually because `name` already exists), None is returned.
    fn ensure_inline(
        &mut self,
        node: &ast::Type,
        id: RootTypeId,
        name: &str,
    ) -> Option<RootTypeId> {
        // If type is already inline, there's nothing to do.
        match node {
            ast::Type::TypeOption(_)
            | ast::Type::TypeArray(_)
            | ast::Type::TypeRef(_)
            | ast::Type::TypeI32(_)
            | ast::Type::TypeI64(_)
            | ast::Type::TypeU32(_)
            | ast::Type::TypeU64(_)
            | ast::Type::TypeF32(_)
            | ast::Type::TypeF64(_)
            | ast::Type::TypeDate(_)
            | ast::Type::TypeTimestamp(_)
            | ast::Type::TypeBool(_)
            | ast::Type::TypeString(_)
            | ast::Type::TypeFile(_) => return Some(id),

            ast::Type::Record(_)
            | ast::Type::Enum(_)
            | ast::Type::Union(_)
            | ast::Type::TypeResponse(_) => (),
        }

        // Check if name already exists
        let name_id = self.strings.get_or_intern(name);
        if let Some(original_def_ptr) = self.symbol_map.get(&name_id) {
            let original_def = original_def_ptr.to_node(self.root.syntax());

            // If symbol table contains the name already, we should also have a valid type def
            // node, so expects should be fine.
            let original_range = original_def
                .name()
                .expect("name of original type def should exist")
                .text_range();

            self.reports.push(
                Report::error(format!("name `{name}` collides with auto generated name"))
                    .add_label(Label::primary(
                        format!("name `{name}` collides with auto generated name"),
                        original_range.into(),
                    ))
                    .add_label(Label::secondary(
                        "autogenerated name for this type".to_string(),
                        node.syntax().text_range().into(),
                    )),
            );

            return None;
        }

        // Insert into symbol tables. We don't have to update `symbol_map`,
        // because we are not modifying the AST. We are only modifying semantic representation.
        // Also, symbol_map is used primarily for tracking/resolving AST references. This is not
        // an AST reference, only a semantic one and it will never tried to be resolved on the
        // AST level.
        self.spec_symbol_table.insert(
            name_id,
            TypeDefData {
                typ: id,
                name: name_id,
                docs: None,
            },
        );

        // Create a new reference and return it.
        let ref_id = self.types.add_reference(RootRef(name_id));
        Some(ref_id)
    }
}

/// Helper type to determine what report should be build when type is not simple record.
///
/// Used by [`Oracle::require_simple_record`].
enum SimpleRecordReportType<'a> {
    /// Given type is not a record
    NotStruct(&'a ast::Type),

    /// Given record has a composite field (not simple)
    CompositeField(&'a ast::TypeField),
}

/// Helper type representing a valid reference.
#[derive(Debug, Clone, Copy)]
enum ValidRef {
    Type(TypeRef),
    Response {
        reference: RootRef,
        /// Range of the dereferenced response
        range: TextRange,
    },
}

impl From<ValidRef> for RootRef {
    fn from(value: ValidRef) -> Self {
        match value {
            ValidRef::Type(reference) => reference.into(),
            ValidRef::Response { reference, .. } => reference,
        }
    }
}

/// Validates reference is valid.
///
/// Reports are pushed to `reports` on the fly. If reference is valid, [`PrimitiveType`] that can
/// be inserted into types arena is returned, otherwise `None` is returned.
fn validate_reference(
    strings: &StringInterner,
    symbol_map: &SymbolMap,
    root: &ast::Root,
    reports: &mut Vec<Report>,
    reference: StringId,
    range: TextRange,
) -> Option<ValidRef> {
    match resolve(strings, symbol_map, root, reference, range) {
        ResolvedSymbol::Missing { name, range } => {
            let name_str = strings.resolve(name);
            reports.push(report_missing(name_str, range));
            None
        }

        // Report created during cycle detection
        ResolvedSymbol::Cycle => None,

        ResolvedSymbol::Type(ast::Type::TypeResponse(resp)) => Some(ValidRef::Response {
            reference: RootRef(reference),
            range: resp.syntax().text_range(),
        }),
        ResolvedSymbol::Type(_) => Some(ValidRef::Type(TypeRef(reference))),
    }
}

/// Lowers array or option by using the [`OptionArrayBuilder`].
///
/// It inserts the `inner` type to the builder and returns the id of the constructed
/// Array or Option type. All intermediate types are inserted into the source map.
fn lower_array_option(
    inner: &ast::Type,
    mut builder: OptionArrayBuilder,
    reports: &mut Vec<Report>,
    strings: &mut StringInterner,
    symbol_map: &SymbolMap,
    root: &ast::Root,
    outer_type: InvalidOuterContext,
) -> Option<TypeId> {
    match ParsedType::new(inner, strings) {
        ParsedType::Primitive(primitive) => {
            let res = builder.finish_primitive(primitive);
            Some(res.container_id)
        }
        ParsedType::Reference { name, range } => {
            match validate_reference(strings, symbol_map, root, reports, name, range) {
                None => None,
                Some(ValidRef::Response {
                    range: deref_range, ..
                }) => {
                    let report = new_invalid_response(range, Some(deref_range));
                    reports.push(report);

                    None
                }
                Some(ValidRef::Type(reference)) => {
                    let res = builder.finish_reference(reference);
                    Some(res.container_id)
                }
            }
        }
        ParsedType::Array(arr) => {
            // Error for empty inner type is reported by parser.
            let inner = arr.typ()?;
            builder.start_array();
            lower_array_option(
                &inner,
                builder,
                reports,
                strings,
                symbol_map,
                root,
                InvalidOuterContext::Array,
            )
        }
        ParsedType::Option(opt) => {
            // Error for empty inner type is reported by parser.
            let inner = opt.typ()?;
            builder.start_option();
            lower_array_option(
                &inner,
                builder,
                reports,
                strings,
                symbol_map,
                root,
                InvalidOuterContext::Option,
            )
        }

        ParsedType::Enum(_) => {
            reports.push(new_invalid_inner_type(
                InvalidInnerContext::Enum,
                outer_type,
                inner,
            ));
            None
        }
        ParsedType::Response => {
            reports.push(new_invalid_response(inner.syntax().text_range(), None));
            None
        }
        ParsedType::Record(_) => {
            reports.push(new_invalid_inner_type(
                InvalidInnerContext::Record,
                outer_type,
                inner,
            ));
            None
        }
        ParsedType::Union(_) => {
            reports.push(new_invalid_inner_type(
                InvalidInnerContext::Union,
                outer_type,
                inner,
            ));
            None
        }
    }
}

/// Helper type for passing around the context in which an invalid inner
/// type has been found.
#[derive(Debug, Clone, Copy, PartialEq, Eq, derive_more::Display)]
enum InvalidInnerContext {
    #[display("enum")]
    Enum,

    #[display("union")]
    Union,

    #[display("record")]
    Record,
}

/// Helper type for passing around the context in which an invalid inner
/// type has been found
#[derive(Debug, Clone, Copy, PartialEq, Eq, derive_more::Display)]
enum InvalidOuterContext {
    #[display("option")]
    Option,

    #[display("array")]
    Array,

    #[display("record field")]
    RecordField,

    #[display("union field")]
    UnionField,
}

/// Constructs a [`Report`] for reporting that a type contains invalid inner(inline) type.
///
/// For instance, when parsing a record, not all types are valid inside a record field.
/// This function can be used to construct a [`Report`] that tells the user about invalid type.
fn new_invalid_inner_type(
    inner: InvalidInnerContext,
    outer: InvalidOuterContext,
    node: &impl AstNode,
) -> Report {
    let range = node.syntax().text_range();

    let syntax_example = match inner {
        InvalidInnerContext::Enum => "type MyEnum: enum (T) { ... }",
        InvalidInnerContext::Union => "type MyUnion: union { ... }",
        InvalidInnerContext::Record => "type MyRecord: rec { ... }",
    };

    Report::error(format!("inline {inner} not allowed in {outer}"))
        .add_label(Label::primary(
            format!("inline {inner} type not allowed"),
            range.into(),
        ))
        .with_note(format!(
            "help: define a named type first, then reference it\n      example: `{syntax_example}`"
        ))
}

/// Constructs a [`Report`] for reporting that a response type is used in invalid context.
///
/// For instance, response type can't be used as a child of any type.
/// The function takes:
/// - `range`: Range of the node that is invalid. If the node is a reference, this is the range of the reference,
///   not the range of response the reference points to.
/// - `deref_range`: If the invalid node is a reference, this is the range of the dereferenced response type.
///   If the invalid node is not a reference, this is `None`.
fn new_invalid_response(range: TextRange, deref_range: Option<TextRange>) -> Report {
    let mut report = Report::error("invalid usage of `response` type".to_string())
        .add_label(Label::primary(
            "invalid usage of `response` type".to_string(),
            range.into(),
        ))
        .with_note("help: `response` type can't be a child of a type".to_string());

    if let Some(deref_range) = deref_range
        && range != deref_range
    {
        report = report.add_label(Label::secondary(
            "`response` type introduced here".to_string(),
            deref_range.into(),
        ))
    }

    report
}

/// Inserts type fields into the type arena.
///
/// This must be called on fields returned by [`Oracle::parse_type_fields`].
fn insert_type_fields(
    fields: ParsedFields,
    mut builder: FieldBuilder,
    reports: &mut Vec<Report>,
    strings: &mut StringInterner,
    symbol_map: &SymbolMap,
    root: &ast::Root,
    outer_type: InvalidOuterContext,
) -> TypeId {
    for field in fields.0 {
        let typ = field
            .field
            .typ()
            .expect("inserted field should have a type");

        match ParsedType::new(&typ, strings) {
            ParsedType::Primitive(primitive) => {
                builder.add_primitive(field.name, primitive, field.default, field.docs);
            }
            ParsedType::Reference { name, range } => {
                match validate_reference(strings, symbol_map, root, reports, name, range) {
                    None => (),
                    // On response we do nothing. Report was generated in `parse_type_fields`.
                    Some(ValidRef::Response { .. }) => (),
                    Some(ValidRef::Type(reference)) => {
                        builder.add_reference(field.name, reference, field.default, field.docs);
                    }
                }
            }
            ParsedType::Array(arr) => {
                let Some(inner) = arr.typ() else {
                    continue;
                };

                let (child_builder, _) = builder.start_array(field.name, field.default, None);
                lower_array_option(
                    &inner,
                    child_builder,
                    reports,
                    strings,
                    symbol_map,
                    root,
                    InvalidOuterContext::Array,
                );
            }
            ParsedType::Option(opt) => {
                let Some(inner) = opt.typ() else {
                    continue;
                };

                let (child_builder, _) =
                    builder.start_option(field.name, field.default, field.docs);
                lower_array_option(
                    &inner,
                    child_builder,
                    reports,
                    strings,
                    symbol_map,
                    root,
                    InvalidOuterContext::Option,
                );
            }
            ParsedType::Enum(_) => {
                reports.push(new_invalid_inner_type(
                    InvalidInnerContext::Enum,
                    outer_type,
                    &typ,
                ));
            }
            // On response we do nothing. Report was generated in `parse_type_fields`.
            ParsedType::Response => (),
            ParsedType::Record(_) => {
                reports.push(new_invalid_inner_type(
                    InvalidInnerContext::Record,
                    outer_type,
                    &typ,
                ));
            }
            ParsedType::Union(_) => {
                reports.push(new_invalid_inner_type(
                    InvalidInnerContext::Union,
                    outer_type,
                    &typ,
                ));
            }
        };
    }

    builder.finish()
}

/// Check that names of type fields are unique.
/// Fields are expected to be sorted by name.
fn check_type_fields_unique(
    fields: &[InternedField],
    reports: &mut Vec<Report>,
    strings: &StringInterner,
) {
    if fields.is_empty() {
        return;
    }

    for idx in 0..(fields.len() - 1) {
        if fields[idx].name != fields[idx + 1].name {
            continue;
        }

        let name = strings.resolve(fields[idx].name);

        let range = fields[idx + 1]
            .field
            .name()
            .expect("collected type field should have a name")
            .syntax()
            .text_range();

        reports.push(
            Report::error(format!("repeated field name `{name}`")).add_label(Label::primary(
                "repeated field name".to_string(),
                range.into(),
            )),
        );
    }
}
