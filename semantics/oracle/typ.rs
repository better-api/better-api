use better_api_diagnostic::{Label, Report};
use better_api_syntax::ast::AstNode;
use better_api_syntax::{TextRange, ast};

use crate::oracle::value::{lower_mime_types, lower_value};
use crate::spec::typ::{
    EnumMember, FieldBuilder, OptionArrayBuilder, PrimitiveType, TypeDef, TypeId,
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

/// Helper type for handling primitive types separately from composite ones.
enum ParsedType<'a> {
    Primitive(PrimitiveType),
    Enum(&'a ast::Enum),
    Response(&'a ast::TypeResponse),
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
            ast::Type::TypeResponse(resp) => Self::Response(resp),
            ast::Type::TypeRef(typ_ref) => {
                let token = typ_ref.name();
                let str_id = strings.get_or_intern(token.text());

                Self::Primitive(PrimitiveType::Reference(str_id))
            }
            ast::Type::TypeI32(_) => Self::Primitive(PrimitiveType::I32),
            ast::Type::TypeI64(_) => Self::Primitive(PrimitiveType::I64),
            ast::Type::TypeU32(_) => Self::Primitive(PrimitiveType::U32),
            ast::Type::TypeU64(_) => Self::Primitive(PrimitiveType::U64),
            ast::Type::TypeF32(_) => Self::Primitive(PrimitiveType::F32),
            ast::Type::TypeF64(_) => Self::Primitive(PrimitiveType::F64),
            ast::Type::TypeDate(_) => Self::Primitive(PrimitiveType::Date),
            ast::Type::TypeTimestamp(_) => Self::Primitive(PrimitiveType::Timestamp),
            ast::Type::TypeBool(_) => Self::Primitive(PrimitiveType::Bool),
            ast::Type::TypeString(_) => Self::Primitive(PrimitiveType::String),
            ast::Type::TypeFile(_) => Self::Primitive(PrimitiveType::File),
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
        let name_id = self.strings.get_or_intern(name_token.text());

        // Missing type report is handled by parser. Other errors are handled in lower_type.
        let Some(type_id) = def.typ().and_then(|t| self.lower_type(&t)) else {
            return;
        };

        // Insert type to symbol table if not already present.
        // The duplicate type definition error is reported by [`Oracle::validate_symbols`].
        self.spec_symbol_table.entry(name_id).or_insert(TypeDef {
            type_id,
            // TODO: Extract docs from type definition prologue
            docs: None,
        });
    }

    /// Lower a syntactical type and store it into the type arena and source map.
    ///
    /// Returns [`TypeId`] if type could be parsed, and `None` otherwise.
    ///
    /// If type was lowered successfully it's not yet completely validated.
    /// This has to be done after _all_ the types have been lowered.
    pub(crate) fn lower_type(&mut self, typ: &ast::Type) -> Option<TypeId> {
        match ParsedType::new(typ, &mut self.strings) {
            ParsedType::Primitive(primitive) => Some(self.types.add_primitive(primitive)),
            ParsedType::Enum(en) => self.lower_enum(en),
            ParsedType::Response(resp) => self.lower_response(resp),
            ParsedType::Record(record) => self.lower_record(record),
            ParsedType::Union(union) => self.lower_union(union),
            ParsedType::Array(arr) => {
                let inner = arr.typ()?;
                let builder = self.types.start_array();
                lower_array_option(
                    &inner,
                    builder,
                    &mut self.reports,
                    &mut self.strings,
                    "array",
                )
            }
            ParsedType::Option(opt) => {
                let inner = opt.typ()?;
                let builder = self.types.start_option();
                lower_array_option(
                    &inner,
                    builder,
                    &mut self.reports,
                    &mut self.strings,
                    "option",
                )
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

            ast::Type::Record(_)
            | ast::Type::Enum(_)
            | ast::Type::Union(_)
            | ast::Type::TypeResponse(_) => Ok(false),

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
            // If there is a missing type (name doesn't exist) we want to report an error.
            // If there are cycles, we don't want to report any error for the same reasons
            // as option and array.
            ast::Type::TypeRef(reference) => match self.deref(reference) {
                Err(None) => Ok(true),
                Err(Some(report)) => Err(report),
                Ok(typ) => self.is_simple_type(&typ, allow_option, allow_array),
            },
        }
    }

    /// Lowers enum type and inserts it into arena
    fn lower_enum(&mut self, typ: &ast::Enum) -> Option<TypeId> {
        // Validate enum type
        let enum_type = typ.typ()?;
        if !self.is_enum_type_valid(&enum_type) {
            return None;
        }

        let type_id = self
            .lower_type(&enum_type)
            .expect("valid enum type should lowered");

        // Parse enum members
        let mut builder = self.types.start_enum(type_id);
        let mut is_valid = true;

        for member in typ.members() {
            let Some(value) = member.value() else {
                // Missing enum member value is reported by parser.
                is_valid = false;
                continue;
            };

            if !ast::value_matches_type(&value, &enum_type, &mut self.reports) {
                is_valid = false;
                continue;
            }

            let value_id = lower_value(
                &mut self.values,
                &mut self.strings,
                &mut self.reports,
                &value,
            );
            builder.add_member(EnumMember {
                value: value_id,
                // TODO: Extract docs from enum member prologue
                docs: None,
            });
        }

        if is_valid {
            Some(builder.finish())
        } else {
            None
        }
    }

    /// Lowers response type and inserts it into arena
    fn lower_response(&mut self, resp: &ast::TypeResponse) -> Option<TypeId> {
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
            match self.lower_headers(&headers) {
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

        if matches!(body, ast::Type::TypeResponse(_)) {
            self.reports.push(
                Report::error("invalid response body type".to_string()).add_label(Label::primary(
                    "response body cannot be a response".to_string(),
                    body.syntax().text_range().into(),
                )),
            );

            return None;
        }

        // Does content type require the response body to be `file`
        let requires_file = content_type_id.is_some_and(|id| {
            self.values.get_mime_types(id).any(|str_id| {
                let str = self.strings.resolve(str_id);
                str != "application/json"
            })
        });

        // Check that response body is `file` by also resolving references
        if requires_file {
            match self.require_file(&body, resp.content_type().as_ref()) {
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

            match self.require_no_file(&body, report_builder) {
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

        let id = self
            .types
            .add_response(body_id, headers_id, content_type_id);
        Some(id)
    }

    /// Checks that given type represents valid headers and lowers it.
    ///
    /// Valid headers are simple record without files. If type is valid,
    /// Some(_) is returned. If any validation fails, reports are generated and None is returned.
    fn lower_headers(&mut self, headers: &ast::Type) -> Option<TypeId> {
        // Are headers valid. We don't want to early return, because we want
        // to validate as many things as possible.
        let mut is_valid = true;

        let file_report_builder = |range: TextRange| {
            Report::error("invalid headers type".to_string())
                .add_label(Label::primary(
                    "headers type must not contain `file`".to_string(),
                    range.into(),
                ))
                .add_label(Label::secondary(
                    "headers used here".to_string(),
                    headers.syntax().text_range().into(),
                ))
        };

        match self.require_no_file(headers, file_report_builder) {
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
                        range.into(),
                    ))
                    .add_label(Label::secondary(
                        "headers used here".to_string(),
                        headers.syntax().text_range().into(),
                    ))
            }
        };

        match self.require_simple_record(headers, true, false, simple_report_builder) {
            Ok(_) => (),
            Err(_) => is_valid = false,
        }

        let id = self.lower_type(headers);

        if is_valid { id } else { None }
    }

    /// Lowers record type and inserts it into arena
    fn lower_record(&mut self, record: &ast::Record) -> Option<TypeId> {
        todo!()
        // let fields = self.parse_type_fields(record.fields(), true);
        // let builder = self.types.start_record();
        // insert_type_fields(
        //     fields,
        //     builder,
        //     &mut self.source_map,
        //     &mut self.reports,
        //     &mut self.strings,
        //     "record field",
        // )
    }

    /// Lowers union type and inserts it into arena
    fn lower_union(&mut self, union: &ast::Union) -> Option<TypeId> {
        todo!()
        // let discriminator = union
        //     .discriminator()
        //     .map(|v| (self.lower_value(&v), v.syntax().text_range()))
        //     .and_then(|(id, range)| match self.values.get(id) {
        //         Value::String(id) => Some(id),
        //         val => {
        //             self.reports.push(
        //                 Report::error(format!("union discriminator must be a string, got {val}"))
        //                     .add_label(Label::primary(
        //                         "invalid union discriminator".to_string(),
        //                         range.into(),
        //                     ))
        //                     .with_note("help: union discrminator must be a string".to_string()),
        //             );
        //
        //             None
        //         }
        //     });
        //
        // let fields = self.parse_type_fields(union.fields(), false);
        // let builder = self.types.start_union(discriminator);
        // insert_type_fields(
        //     fields,
        //     builder,
        //     &mut self.source_map,
        //     &mut self.reports,
        //     &mut self.strings,
        //     "union field",
        // )
    }

    /// Collects type fields into a vector.
    ///
    /// Only valid fields are collected. Field is valid if it has a valid name and a type.
    /// Names are interned.
    ///
    /// Returned vector is sorted by name, which stabilizes the fields. This is useful for
    /// type checking.
    fn parse_type_fields(
        &mut self,
        fields: impl Iterator<Item = ast::TypeField>,
        parse_default: bool,
    ) -> Vec<InternedField> {
        let mut fields: Vec<_> = fields
            .filter_map(|f| {
                f.typ()?;

                let name_id = f
                    .name()
                    .and_then(|n| lower_name(&n, &mut self.strings, &mut self.reports))?;

                let default = if parse_default {
                    f.prologue()
                        .and_then(|p| p.default())
                        .and_then(|d| d.value())
                        .map(|val| self.lower_value(&val))
                } else {
                    // No need to report an error if default is present in field.
                    // It's already reported by the parser.
                    None
                };

                Some(InternedField {
                    name: name_id,
                    field: f,
                    default,
                    // TODO: Extract docs from field prologue
                    docs: None,
                })
            })
            .collect();

        fields.sort_by_key(|f| f.name);
        // TODO: Check fields are unique in the place where type validation happens

        fields
    }

    /// Validates type of the enum (not enum itself).
    ///
    /// When declaring an enum, you do `enum (T) {...}`. This function validates
    /// that `T` is one of the allowed types. If it isn't a report is generated.
    fn is_enum_type_valid(&mut self, enum_type: &ast::Type) -> bool {
        match enum_type {
            ast::Type::TypeI32(_)
            | ast::Type::TypeI64(_)
            | ast::Type::TypeU32(_)
            | ast::Type::TypeU64(_)
            | ast::Type::TypeString(_) => true,

            typ => {
                let range = enum_type.syntax().text_range();
                self.reports.push(
                    Report::error(format!("invalid enum type {typ}"))
                        .add_label(Label::primary(
                            format!("invalid enum type {typ}"),
                            range.into(),
                        ))
                        .with_note(
                            "help: enum must have a type `i32`, `i64`, `u32`, `u64`, or `string`"
                                .to_string(),
                        ),
                );

                false
            }
        }
    }

    /// Requires that node is a file or reference to a file.
    ///
    /// Reports are added to self.reports on the fly.
    ///
    /// If no reports are generated (node is valid), Ok is returned, otherwise Err.
    fn require_file(
        &mut self,
        node: &ast::Type,
        content_type: Option<&ast::ResponseContentType>,
    ) -> Result<(), ()> {
        // Helper function for building invalid body report.
        fn require_file_aux(
            node: &ast::Type,
            content_type: Option<&ast::ResponseContentType>,
        ) -> Result<(), Report> {
            if matches!(node, ast::Type::TypeFile(_)) {
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
        }

        match &node {
            ast::Type::TypeRef(reference) => match self.deref(reference) {
                Err(None) => Err(()),
                Err(Some(report)) => {
                    self.reports.push(report);
                    Err(())
                }
                Ok(typ) => match require_file_aux(&typ, content_type) {
                    Ok(_) => Ok(()),
                    Err(report) => {
                        self.reports.push(report);
                        Err(())
                    }
                },
            },

            _ => match require_file_aux(node, content_type) {
                Ok(_) => Ok(()),
                Err(report) => {
                    self.reports.push(report);
                    Err(())
                }
            },
        }
    }

    /// Requires that node is not a file, and if it's a composite type (like record) does
    /// not contain a file. It also resolves references.
    ///
    /// Reports are added to self.reports on the fly.
    ///
    /// If no reports are generated (node is valid), Ok is returned, otherwise Err.
    fn require_no_file<R>(&mut self, node: &ast::Type, build_report: R) -> Result<(), ()>
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
                        .is_none_or(|typ| self.require_no_file(&typ, &build_report).is_ok())
                });

                if valid { Ok(()) } else { Err(()) }
            }
            ast::Type::Union(union) => {
                let valid = union.fields().all(|field| {
                    field
                        .typ()
                        .is_none_or(|typ| self.require_no_file(&typ, &build_report).is_ok())
                });

                if valid { Ok(()) } else { Err(()) }
            }

            // There are only two ways to get here:
            // - call this method directly on a response: we have a bug in `lower_response`
            // - one of the composite types contains a response: during lowering of that type the
            //   error is reported
            //
            // In any case, we don't have to report an error here
            ast::Type::TypeResponse(_) => Err(()),

            ast::Type::TypeRef(reference) => match self.deref(reference) {
                Err(None) => Err(()),
                Err(Some(report)) => {
                    self.reports.push(report);
                    Err(())
                }
                Ok(typ) => self.require_no_file(&typ, build_report),
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
                Err(None) => return Err(()),
                Err(Some(report)) => {
                    self.reports.push(report);
                    return Err(());
                }
                Ok(ast::Type::Record(rec)) => rec.fields(),
                Ok(typ) => {
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

/// Lowers array or option by using the [`OptionArrayBuilder`].
///
/// It inserts the `inner` type to the builder and returns the id of the constructed
/// Array or Option type. All intermediate types are inserted into the source map.
fn lower_array_option(
    inner: &ast::Type,
    mut builder: OptionArrayBuilder,
    reports: &mut Vec<Report>,
    strings: &mut StringInterner,
    outer_type_name: &str,
) -> Option<TypeId> {
    let container_id = match ParsedType::new(inner, strings) {
        ParsedType::Primitive(primitive) => {
            let res = builder.finish(primitive);
            res.container_id
        }
        ParsedType::Array(arr) => {
            // Error for empty inner type is reported by parser.
            let inner = arr.typ()?;
            builder.start_array();
            let container_id =
                lower_array_option(&inner, builder, reports, strings, outer_type_name)?;

            container_id
        }
        ParsedType::Option(opt) => {
            // Error for empty inner type is reported by parser.
            let inner = opt.typ()?;
            builder.start_option();
            let container_id =
                lower_array_option(&inner, builder, reports, strings, outer_type_name)?;

            container_id
        }

        ParsedType::Enum(_) => {
            reports.push(new_invalid_inner_type(
                InvalidInnerContext::Enum,
                outer_type_name,
                inner,
            ));
            return None;
        }
        ParsedType::Response(_) => {
            reports.push(new_invalid_inner_type(
                InvalidInnerContext::Response,
                outer_type_name,
                inner,
            ));
            return None;
        }
        ParsedType::Record(_) => {
            reports.push(new_invalid_inner_type(
                InvalidInnerContext::Record,
                outer_type_name,
                inner,
            ));
            return None;
        }
        ParsedType::Union(_) => {
            reports.push(new_invalid_inner_type(
                InvalidInnerContext::Union,
                outer_type_name,
                inner,
            ));
            return None;
        }
    };

    Some(container_id)
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

    #[display("response")]
    Response,
}

/// Constructs a [`Report`] for reporting that a type contains invalid inner(inline) type.
///
/// For instance, when parsing a record, not all types are valid inside a record field.
/// This function can be used to construct a [`Report`] that tells the user about invalid type.
fn new_invalid_inner_type(inner: InvalidInnerContext, outer: &str, node: &impl AstNode) -> Report {
    let range = node.syntax().text_range();

    let syntax_example = match inner {
        InvalidInnerContext::Enum => "type MyEnum: enum (T) { ... }",
        InvalidInnerContext::Union => "type MyUnion: union (\"discriminator\") { ... }",
        InvalidInnerContext::Record => "type MyRecord: rec { ... }",
        InvalidInnerContext::Response => "type MyResponse: resp { ... }",
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

/// Inserts type fields into the type arena.
fn insert_type_fields(
    fields: Vec<InternedField>,
    mut builder: FieldBuilder,
    reports: &mut Vec<Report>,
    strings: &mut StringInterner,
    type_field_name: &str,
) -> TypeId {
    todo!()
    // for field in fields {
    //     let typ = field
    //         .field
    //         .typ()
    //         .expect("inserted field should have a type");
    //
    //     let field_id = match ParsedType::new(&typ, strings) {
    //         ParsedType::Primitive(primitive) => {
    //             Some(builder.add_primitive(field.name, primitive, field.default))
    //         }
    //         ParsedType::Array(arr) => {
    //             let Some(inner) = arr.typ() else {
    //                 continue;
    //             };
    //
    //             let (child_builder, field_id) = builder.start_array(field.name, field.default);
    //             let type_id = lower_array_option(
    //                 &inner,
    //                 child_builder,
    //                 reports,
    //                 strings,
    //                 source_map,
    //                 "array",
    //             );
    //
    //             if type_id.is_some() {
    //                 Some(field_id)
    //             } else {
    //                 None
    //             }
    //         }
    //         ParsedType::Option(opt) => {
    //             let Some(inner) = opt.typ() else {
    //                 continue;
    //             };
    //
    //             let (child_builder, field_id) = builder.start_option(field.name, field.default);
    //             let type_id = lower_array_option(
    //                 &inner,
    //                 child_builder,
    //                 reports,
    //                 strings,
    //                 source_map,
    //                 "option",
    //             );
    //
    //             if type_id.is_some() {
    //                 Some(field_id)
    //             } else {
    //                 None
    //             }
    //         }
    //         ParsedType::Enum(_) => {
    //             reports.push(new_invalid_inner_type(
    //                 InvalidInnerContext::Enum,
    //                 type_field_name,
    //                 &typ,
    //             ));
    //             continue;
    //         }
    //         ParsedType::Response(_) => {
    //             reports.push(new_invalid_inner_type(
    //                 InvalidInnerContext::Response,
    //                 type_field_name,
    //                 &typ,
    //             ));
    //             continue;
    //         }
    //         ParsedType::Record(_) => {
    //             reports.push(new_invalid_inner_type(
    //                 InvalidInnerContext::Record,
    //                 type_field_name,
    //                 &typ,
    //             ));
    //             continue;
    //         }
    //         ParsedType::Union(_) => {
    //             reports.push(new_invalid_inner_type(
    //                 InvalidInnerContext::Union,
    //                 type_field_name,
    //                 &typ,
    //             ));
    //             continue;
    //         }
    //     };
    //
    //     let Some(field_id) = field_id else {
    //         continue;
    //     };
    //
    //     source_map.insert_type(field_id.type_id(), &typ);
    // }
    //
    // builder.finish()
}
