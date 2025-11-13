use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::ast;
use better_api_syntax::ast::AstNode;
use string_interner::DefaultStringInterner;

use crate::oracle::value::insert_array_values;
use crate::typ::{OptionArrayBuilder, PrimitiveType, Type, TypeId};
use crate::value::{Value, ValueId};
use crate::{Element, SourceMap};

use super::Oracle;

impl<'a> Oracle<'a> {
    pub(crate) fn parse_type(&mut self, typ: &ast::Type) -> Option<TypeId> {
        let id = match ParsedType::new(typ, &mut self.strings) {
            ParsedType::Primitive(primitive) => self.types.add_primitive(primitive),
            ParsedType::Enum(en) => self.parse_enum(en),
            ParsedType::Response(resp) => self.parse_response(resp),
            ParsedType::Record(record) => todo!(),
            ParsedType::Union(union) => todo!(),
            ParsedType::Array(arr) => {
                let inner = arr.typ()?;
                let builder = self.types.start_array();
                parse_array_option(
                    &inner,
                    builder,
                    &mut self.source_map,
                    &mut self.reports,
                    &mut self.strings,
                    "array",
                )?
            }
            ParsedType::Option(opt) => {
                let inner = opt.typ()?;
                let builder = self.types.start_option();
                parse_array_option(
                    &inner,
                    builder,
                    &mut self.source_map,
                    &mut self.reports,
                    &mut self.strings,
                    "option",
                )?
            }
        };

        self.source_map.insert(typ, Element::Type(id));
        Some(id)
    }

    /// Parses enum type and inserts it into arena
    fn parse_enum(&mut self, typ: &ast::Enum) -> TypeId {
        // TODO: Validate members types. For this value - type comparison function is needed.

        // Parse type of the enum and validate it
        // Reports are handled by parser and self.parse_type methods already
        let enum_type_id = typ.typ().and_then(|t| self.parse_type(&t));
        if let Some(id) = enum_type_id {
            self.validate_enum_type(id);
        }

        // Parse enum members
        let builder = self.values.start_array();
        let members_id = insert_array_values(
            typ.members().filter_map(|m| m.value()),
            builder,
            &mut self.source_map,
            &mut self.reports,
            &mut self.strings,
        );

        // Insert members into source map
        let arr = match self.values.get(members_id) {
            Value::Array(arr) => arr,
            _ => unreachable!("inserted array should be an array"),
        };
        for (member, arr_item) in typ.members().filter(|m| m.value().is_some()).zip(arr) {
            self.source_map
                .insert(&member, Element::EnumMember(arr_item.id));
        }

        self.types.add_enum(enum_type_id, members_id)
    }

    fn parse_response(&mut self, resp: &ast::TypeResponse) -> TypeId {
        // Parse and validate content type
        let content_type_id = resp
            .content_type()
            .and_then(|v| v.value())
            .map(|v| self.parse_value(&v));
        if let Some(id) = content_type_id {
            self.validate_response_content_type(id);
        }

        // Parse and validate header type
        let headers_id = resp
            .headers()
            .and_then(|h| h.typ())
            .and_then(|t| self.parse_type(&t));
        // TODO: Check that headers type is a record. Do not forget to resolve named references

        // Parse and validate response body
        let body_id = resp
            .body()
            .and_then(|b| b.typ())
            .and_then(|t| self.parse_type(&t));
        // TODO: Check that body type is valid (not a response)

        // TODO: Check response body type and header mime type match.

        self.types
            .add_response(body_id, headers_id, content_type_id)
    }

    /// Validates type of the enum (not enum itself).
    ///
    /// When declaring an enum, you do `enum (T) {...}`. This function validates
    /// that `T` is one of the allowed types. If it isn't a report is generated.
    fn validate_enum_type(&mut self, enum_type_id: TypeId) {
        match self.types.get(enum_type_id) {
            Type::I32 | Type::I64 | Type::U32 | Type::U64 | Type::String => (),
            typ => {
                let node_ptr = self.source_map.get_bck(&Element::Type(enum_type_id));
                let node = self.node(node_ptr);
                let range = node.text_range();

                self.reports.push(
                    Report::error(format!("invalid enum type {typ}"))
                        .with_label(Label::new(
                            format!("invalid enum type {typ}"),
                            Span::new(range.start().into(), range.end().into()),
                        ))
                        .with_note(
                            "help: enum must have a type `i32`, `i64`, `u32`, `u64`, or `string`"
                                .to_string(),
                        ),
                );
            }
        }
    }

    /// Validates if content type value of response is valid.
    ///
    /// If the content type isn't valid, report is generated.
    fn validate_response_content_type(&mut self, content_type_id: ValueId) {
        match self.values.get(content_type_id) {
            Value::String(str_id) => {
                let content_type = self
                    .strings
                    .resolve(str_id)
                    .expect("stored string should be interned");

                // TODO: Validate that header has a valid mime type and is not a random string
            }
            val => {
                let node_ptr = self.source_map.get_bck(&Element::Value(content_type_id));
                let node = self.node(node_ptr);
                let range = node.text_range();

                self.reports.push(
                    Report::error(format!("invalid response content type {val}"))
                        .with_label(Label::new(
                            format!("invalid response content type {val}"),
                            Span::new(range.start().into(), range.end().into()),
                        ))
                        .with_note("help: response content type must be a string".to_string()),
                );
            }
        }
    }
}

fn parse_array_option(
    inner: &ast::Type,
    mut builder: OptionArrayBuilder,
    source_map: &mut SourceMap,
    reports: &mut Vec<Report>,
    strings: &mut DefaultStringInterner,
    outer_type_name: &str,
) -> Option<TypeId> {
    let id = match ParsedType::new(inner, strings) {
        ParsedType::Primitive(primitive) => builder.finish(primitive),
        ParsedType::Array(arr) => {
            // Error for empty inner type is reported by parser.
            let inner = arr.typ()?;
            builder.start_array();
            parse_array_option(
                &inner,
                builder,
                source_map,
                reports,
                strings,
                outer_type_name,
            )?
        }
        ParsedType::Option(opt) => {
            // Error for empty inner type is reported by parser.
            let inner = opt.typ()?;
            builder.start_option();
            parse_array_option(
                &inner,
                builder,
                source_map,
                reports,
                strings,
                outer_type_name,
            )?
        }

        ParsedType::Enum(_) => {
            reports.push(new_invalid_array_option_type_report(
                "enum",
                outer_type_name,
                inner,
            ));
            return None;
        }
        ParsedType::Response(_) => {
            reports.push(new_invalid_array_option_type_report(
                "response",
                outer_type_name,
                inner,
            ));
            return None;
        }
        ParsedType::Record(_) => {
            reports.push(new_invalid_array_option_type_report(
                "record",
                outer_type_name,
                inner,
            ));
            return None;
        }
        ParsedType::Union(_) => {
            reports.push(new_invalid_array_option_type_report(
                "union",
                outer_type_name,
                inner,
            ));
            return None;
        }
    };

    source_map.insert(inner, Element::Type(id));
    Some(id)
}

fn new_invalid_array_option_type_report(inner: &str, outer: &str, node: &impl AstNode) -> Report {
    let range = node.syntax().text_range();
    Report::error(format!("invalid {inner} type inside of {outer}"))
        .with_label(Label::new(
            format!("invalid {inner} type"),
            Span::new(range.start().into(), range.end().into()),
        ))
        .with_note(format!(
            "help: create a new named {inner} type, and use it's identifier inside of {outer} type"
        ))
}

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
    fn new(typ: &'a ast::Type, strings: &mut DefaultStringInterner) -> Self {
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
