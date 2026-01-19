use better_api_diagnostic::{Label, Report};
use better_api_syntax::ast;
use better_api_syntax::ast::AstNode;

use crate::oracle::value::lower_value;
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
        todo!()
        // // Parse and validate content type
        // let content_type_id = resp
        //     .content_type()
        //     .and_then(|v| v.value())
        //     .map(|v| self.lower_value(&v));
        // // TODO: Move validation to shared validation logic.
        // if let Some(id) = content_type_id {
        //     self.validate_response_content_type(id);
        // }
        //
        // // Parse and validate header type
        // let headers_id = resp
        //     .headers()
        //     .and_then(|h| h.typ())
        //     .and_then(|t| self.lower_type(&t));
        // // TODO: Check that headers type is a record. Do not forget to resolve named references.
        //
        // // Parse and validate response body
        // let body_id = resp
        //     .body()
        //     .and_then(|b| b.typ())
        //     .and_then(|t| self.lower_type(&t));
        // // TODO: Check that body type is valid (not a response). Again, resolve named references
        //
        // // TODO: Check response body type and header mime type match.
        //
        // self.types
        //     .add_response(body_id, headers_id, content_type_id)
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

    /// Validates if content type value of response is valid.
    ///
    /// If the content type isn't valid, report is generated.
    fn validate_response_content_type(&mut self, content_type_id: ValueId) {
        todo!()
        // match self.values.get(content_type_id) {
        //     Value::String(str_id) => {
        //         let _content_type = self.strings.get(str_id);
        //
        //         // TODO: Validate that header has a valid mime type and is not a random string
        //     }
        //     val => {
        //         let node = self.source_map.get_value(content_type_id);
        //         let range = node.syntax().text_range();
        //
        //         self.reports.push(
        //             Report::error(format!("invalid response content type {val}"))
        //                 .add_label(Label::primary(
        //                     format!("invalid response content type {val}"),
        //                     range.into(),
        //                 ))
        //                 .with_note("help: response content type must be a string".to_string()),
        //         );
        //     }
        // }
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
