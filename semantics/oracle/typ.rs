use std::borrow::Cow;

use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::ast;
use better_api_syntax::ast::AstNode;
use string_interner::DefaultStringInterner;

use crate::oracle::value::insert_array_values;
use crate::text::{parse_string, validate_name};
use crate::typ::{FieldBuilder, OptionArrayBuilder, PrimitiveType, Type, TypeId};
use crate::value::{Value, ValueId};
use crate::{Element, SourceMap, StringId};

use super::Oracle;

/// Represents type field with interned name.
#[derive(Clone)]
struct InternedField {
    name: StringId,
    field: ast::TypeField,
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

impl<'a> Oracle<'a> {
    pub(crate) fn parse_type(&mut self, typ: &ast::Type) -> Option<TypeId> {
        let id = match ParsedType::new(typ, &mut self.strings) {
            ParsedType::Primitive(primitive) => self.types.add_primitive(primitive),
            ParsedType::Enum(en) => self.parse_enum(en),
            ParsedType::Response(resp) => self.parse_response(resp),
            ParsedType::Record(record) => self.parse_record(record),
            ParsedType::Union(union) => self.parse_union(union),
            ParsedType::Array(arr) => {
                let inner = arr.typ()?;
                let builder = self.types.start_array();
                parse_array_option(
                    &inner,
                    builder,
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
        // TODO: Validate members types. For this value -> type comparison function is needed.

        // Parse type of the enum and validate it
        // Reports are handled by parser and self.parse_type methods already
        // TODO: move this in the shared type validation logic
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
        // TODO: Move validation to shared validation logic.
        if let Some(id) = content_type_id {
            self.validate_response_content_type(id);
        }

        // Parse and validate header type
        let headers_id = resp
            .headers()
            .and_then(|h| h.typ())
            .and_then(|t| self.parse_type(&t));
        // TODO: Check that headers type is a record. Do not forget to resolve named references.

        // Parse and validate response body
        let body_id = resp
            .body()
            .and_then(|b| b.typ())
            .and_then(|t| self.parse_type(&t));
        // TODO: Check that body type is valid (not a response). Again, resolve named references

        // TODO: Check response body type and header mime type match.

        self.types
            .add_response(body_id, headers_id, content_type_id)
    }

    fn parse_record(&mut self, record: &ast::Record) -> TypeId {
        let fields = self.parse_type_fields(record.fields());
        let builder = self.types.start_record();
        insert_type_fields(
            fields,
            builder,
            &mut self.source_map,
            &mut self.reports,
            &mut self.strings,
            "record field",
        )
    }

    fn parse_union(&mut self, union: &ast::Union) -> TypeId {
        let discriminator = union
            .discriminator()
            .map(|v| (self.parse_value(&v), v.syntax().text_range()))
            .and_then(|(id, range)| match self.values.get(id) {
                Value::String(id) => Some(id),
                val => {
                    self.reports.push(
                        Report::error(format!("union discriminator must be a string, got {val}"))
                            .with_label(Label::new(
                                "invalid union discrminator".to_string(),
                                Span::new(range.start().into(), range.end().into()),
                            ))
                            .with_note("help: union discrminator must be a string".to_string()),
                    );

                    None
                }
            });

        let fields = self.parse_type_fields(union.fields());
        let builder = self.types.start_union(discriminator);
        insert_type_fields(
            fields,
            builder,
            &mut self.source_map,
            &mut self.reports,
            &mut self.strings,
            "union field",
        )
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
    ) -> Vec<InternedField> {
        let mut fields: Vec<_> = fields
            .filter_map(|f| {
                f.typ()?;

                let name = f.name().map(|n| n.token())?;
                let name_str: Cow<_> = match &name {
                    ast::NameToken::Identifier(ident) => ident.text().into(),
                    ast::NameToken::String(string) => parse_string(string, &mut self.reports),
                };

                if let Err(report) = validate_name(&name_str, name.text_range()) {
                    self.reports.push(report);
                    return None;
                }

                let name_id = self.strings.get_or_intern(name_str);
                Some(InternedField {
                    name: name_id,
                    field: f,
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
                let _content_type = self
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
    reports: &mut Vec<Report>,
    strings: &mut DefaultStringInterner,
    outer_type_name: &str,
) -> Option<TypeId> {
    match ParsedType::new(inner, strings) {
        ParsedType::Primitive(primitive) => Some(builder.finish(primitive)),
        ParsedType::Array(arr) => {
            // Error for empty inner type is reported by parser.
            let inner = arr.typ()?;
            builder.start_array();
            parse_array_option(&inner, builder, reports, strings, outer_type_name)
        }
        ParsedType::Option(opt) => {
            // Error for empty inner type is reported by parser.
            let inner = opt.typ()?;
            builder.start_option();
            parse_array_option(&inner, builder, reports, strings, outer_type_name)
        }

        ParsedType::Enum(_) => {
            reports.push(new_invalid_inner_type("enum", outer_type_name, inner));
            None
        }
        ParsedType::Response(_) => {
            reports.push(new_invalid_inner_type("response", outer_type_name, inner));
            None
        }
        ParsedType::Record(_) => {
            reports.push(new_invalid_inner_type("record", outer_type_name, inner));
            None
        }
        ParsedType::Union(_) => {
            reports.push(new_invalid_inner_type("union", outer_type_name, inner));
            None
        }
    }
}

fn new_invalid_inner_type(inner: &str, outer: &str, node: &impl AstNode) -> Report {
    let range = node.syntax().text_range();
    Report::error(format!("invalid {inner} type inside of {outer}"))
        .with_label(Label::new(
            format!("invalid {inner} type"),
            Span::new(range.start().into(), range.end().into()),
        ))
        .with_note(format!(
            "help: create a new named {inner} type, and use it's identifier inside of {outer}"
        ))
}

fn insert_type_fields(
    fields: Vec<InternedField>,
    mut builder: FieldBuilder,
    source_map: &mut SourceMap,
    reports: &mut Vec<Report>,
    strings: &mut DefaultStringInterner,
    type_field_name: &str,
) -> TypeId {
    for field in fields {
        let typ = field
            .field
            .typ()
            .expect("inserted field should have a type");

        // TODO: Handle default
        let default = None;

        let field_id = match ParsedType::new(&typ, strings) {
            ParsedType::Primitive(primitive) => {
                Some(builder.add_primitive(field.name, primitive, default))
            }
            ParsedType::Array(arr) => {
                let Some(inner) = arr.typ() else {
                    continue;
                };

                let (child_builder, field_id) = builder.start_array(field.name, default);
                let type_id = parse_array_option(&inner, child_builder, reports, strings, "array");

                if type_id.is_some() {
                    Some(field_id)
                } else {
                    None
                }
            }
            ParsedType::Option(opt) => {
                let Some(inner) = opt.typ() else {
                    continue;
                };

                let (child_builder, field_id) = builder.start_option(field.name, default);
                let type_id = parse_array_option(&inner, child_builder, reports, strings, "option");

                if type_id.is_some() {
                    Some(field_id)
                } else {
                    None
                }
            }
            ParsedType::Enum(_) => {
                reports.push(new_invalid_inner_type("enum", type_field_name, &typ));
                continue;
            }
            ParsedType::Response(_) => {
                reports.push(new_invalid_inner_type("response", type_field_name, &typ));
                continue;
            }
            ParsedType::Record(_) => {
                reports.push(new_invalid_inner_type("record", type_field_name, &typ));
                continue;
            }
            ParsedType::Union(_) => {
                reports.push(new_invalid_inner_type("union", type_field_name, &typ));
                continue;
            }
        };

        let Some(field_id) = field_id else {
            continue;
        };

        source_map.insert(&typ, Element::Type(field_id.type_id()));
        source_map.insert(&field.field, Element::TypeField(field_id));
    }

    builder.finish()
}

#[cfg(test)]
mod test {
    use better_api_diagnostic::{Label, Report, Span};
    use better_api_syntax::{parse, tokenize};
    use indoc::indoc;

    use crate::{Element, Oracle, typ::Type};

    #[test]
    fn parse_primitive_i32() {
        let text = indoc! {r#"
            type Foo: i32
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::I32);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_i64() {
        let text = indoc! {r#"
            type Foo: i64
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::I64);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_u32() {
        let text = indoc! {r#"
            type Foo: u32
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::U32);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_u64() {
        let text = indoc! {r#"
            type Foo: u64
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::U64);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_f32() {
        let text = indoc! {r#"
            type Foo: f32
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::F32);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_f64() {
        let text = indoc! {r#"
            type Foo: f64
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::F64);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_date() {
        let text = indoc! {r#"
            type Foo: date
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::Date);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_timestamp() {
        let text = indoc! {r#"
            type Foo: timestamp
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::Timestamp);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_bool() {
        let text = indoc! {r#"
            type Foo: bool
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::Bool);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_string() {
        let text = indoc! {r#"
            type Foo: string
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::String);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_file() {
        let text = indoc! {r#"
            type Foo: file
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::File);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_primitive_reference() {
        let text = indoc! {r#"
            type Foo: Bar
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        let name = oracle.strings.get("Bar").unwrap();
        assert_eq!(oracle.reports(), vec![]);
        assert_eq!(oracle.types.get(id), Type::Reference(name));

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_simple_array() {
        let text = indoc! {r#"
            type Foo: [i32]
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        let arr = match oracle.types.get(id) {
            Type::Array(arr) => arr,
            _ => panic!(),
        };
        assert_eq!(arr.typ(), Type::I32);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_nested_array() {
        let text = indoc! {r#"
            type Foo: [[i32]]
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        let arr = match oracle.types.get(id) {
            Type::Array(arr) => arr,
            _ => panic!(),
        };
        let arr = match arr.typ() {
            Type::Array(arr) => arr,
            _ => panic!(),
        };
        assert_eq!(arr.typ(), Type::I32);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_empty_array() {
        let text = indoc! {r#"
            type Foo: []
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ);
        assert_eq!(id, None);

        assert_eq!(oracle.reports(), vec![]);

        assert_eq!(oracle.source_map.fwd.len(), 0);
        assert_eq!(oracle.source_map.bck.len(), 0);
    }

    #[test]
    fn parse_invalid_array() {
        let text = indoc! {r#"
            type Foo: [rec {}]
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ);
        assert_eq!(id, None);

        assert_eq!(
            oracle.reports(),
            vec![
                Report::error("invalid record type inside of array".to_string()).with_label(
                    Label::new("invalid record type".to_string(), Span::new(11, 17))
                ).with_note("help: create a new named record type, and use it's identifier inside of array".to_string())
            ]
        );

        assert_eq!(oracle.source_map.fwd.len(), 0);
        assert_eq!(oracle.source_map.bck.len(), 0);
    }

    #[test]
    fn parse_simple_option() {
        let text = indoc! {r#"
            type Foo: i32?
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        let opt = match oracle.types.get(id) {
            Type::Option(opt) => opt,
            _ => panic!(),
        };
        assert_eq!(opt.typ(), Type::I32);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_nested_option() {
        // This contains errors reported by parser. We don't care about them here.
        // We are interested that there is a semantic Option<i32> type.
        let text = indoc! {r#"
            type Foo: i32??
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        let opt = match oracle.types.get(id) {
            Type::Option(opt) => opt,
            _ => panic!(),
        };
        assert_eq!(opt.typ(), Type::I32);

        oracle.source_map.get_bck(&Element::Type(id));
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_invalid_option() {
        let text = indoc! {r#"
            type Foo: rec {}?
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        assert!(diagnostics.is_empty());
        assert!(res.reports.is_empty());

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ);
        assert_eq!(id, None);

        assert_eq!(
            oracle.reports(),
            vec![
                Report::error("invalid record type inside of option".to_string()).with_label(
                    Label::new("invalid record type".to_string(), Span::new(10, 16))
                ).with_note("help: create a new named record type, and use it's identifier inside of option".to_string())
            ]
        );

        assert_eq!(oracle.source_map.fwd.len(), 0);
        assert_eq!(oracle.source_map.bck.len(), 0);
    }
}
