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
    default: Option<ValueId>,
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
                    &mut self.source_map,
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
                    &mut self.source_map,
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
        let fields = self.parse_type_fields(record.fields(), true);
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
                                "invalid union discriminator".to_string(),
                                Span::new(range.start().into(), range.end().into()),
                            ))
                            .with_note("help: union discrminator must be a string".to_string()),
                    );

                    None
                }
            });

        let fields = self.parse_type_fields(union.fields(), false);
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
        parse_default: bool,
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

                let default = if parse_default {
                    f.prologue()
                        .and_then(|p| p.default())
                        .and_then(|d| d.value())
                        .map(|val| self.parse_value(&val))
                } else {
                    // No need to report an error if default is present in field.
                    // It's already reported by the parser.
                    None
                };

                Some(InternedField {
                    name: name_id,
                    default,
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

/// Parses array or option by using the [`OptionArrayBuilder`].
///
/// It inserts the `inner` type to the builder and returns the id of the constructed
/// Array or Option type. All intermediate types are inserted into the source map.
fn parse_array_option(
    inner: &ast::Type,
    mut builder: OptionArrayBuilder,
    reports: &mut Vec<Report>,
    strings: &mut DefaultStringInterner,
    source_map: &mut SourceMap,
    outer_type_name: &str,
) -> Option<TypeId> {
    let (container_id, inner_id) = match ParsedType::new(inner, strings) {
        ParsedType::Primitive(primitive) => {
            let res = builder.finish(primitive);
            (res.container_id, res.primitive_id)
        }
        ParsedType::Array(arr) => {
            // Error for empty inner type is reported by parser.
            let inner = arr.typ()?;
            let inner_id = builder.start_array();
            let container_id = parse_array_option(
                &inner,
                builder,
                reports,
                strings,
                source_map,
                outer_type_name,
            )?;

            (container_id, inner_id)
        }
        ParsedType::Option(opt) => {
            // Error for empty inner type is reported by parser.
            let inner = opt.typ()?;
            let inner_id = builder.start_option();
            let container_id = parse_array_option(
                &inner,
                builder,
                reports,
                strings,
                source_map,
                outer_type_name,
            )?;

            (container_id, inner_id)
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

    source_map.insert(inner, Element::Type(inner_id));

    Some(container_id)
}

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

fn new_invalid_inner_type(inner: InvalidInnerContext, outer: &str, node: &impl AstNode) -> Report {
    let range = node.syntax().text_range();

    let syntax_example = match inner {
        InvalidInnerContext::Enum => "type MyEnum: enum (T) { ... }",
        InvalidInnerContext::Union => "type MyUnion: union (\"discriminator\") { ... }",
        InvalidInnerContext::Record => "type MyRecord: rec { ... }",
        InvalidInnerContext::Response => "type MyResponse: resp { ... }",
    };

    Report::error(format!("inline {inner} not allowed in {outer}"))
        .with_label(Label::new(
            format!("inline {inner} type not allowed"),
            Span::new(range.start().into(), range.end().into()),
        ))
        .with_note(format!(
            "help: define a named type first, then reference it\n      example: `{syntax_example}`"
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

        let field_id = match ParsedType::new(&typ, strings) {
            ParsedType::Primitive(primitive) => {
                Some(builder.add_primitive(field.name, primitive, field.default))
            }
            ParsedType::Array(arr) => {
                let Some(inner) = arr.typ() else {
                    continue;
                };

                let (child_builder, field_id) = builder.start_array(field.name, field.default);
                let type_id = parse_array_option(
                    &inner,
                    child_builder,
                    reports,
                    strings,
                    source_map,
                    "array",
                );

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

                let (child_builder, field_id) = builder.start_option(field.name, field.default);
                let type_id = parse_array_option(
                    &inner,
                    child_builder,
                    reports,
                    strings,
                    source_map,
                    "option",
                );

                if type_id.is_some() {
                    Some(field_id)
                } else {
                    None
                }
            }
            ParsedType::Enum(_) => {
                reports.push(new_invalid_inner_type(
                    InvalidInnerContext::Enum,
                    type_field_name,
                    &typ,
                ));
                continue;
            }
            ParsedType::Response(_) => {
                reports.push(new_invalid_inner_type(
                    InvalidInnerContext::Response,
                    type_field_name,
                    &typ,
                ));
                continue;
            }
            ParsedType::Record(_) => {
                reports.push(new_invalid_inner_type(
                    InvalidInnerContext::Record,
                    type_field_name,
                    &typ,
                ));
                continue;
            }
            ParsedType::Union(_) => {
                reports.push(new_invalid_inner_type(
                    InvalidInnerContext::Union,
                    type_field_name,
                    &typ,
                ));
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

    use crate::{Element, Oracle, typ::Type, value::Value};

    #[test]
    fn parse_primitives() {
        let primitives = [
            ("i32", Type::I32),
            ("i64", Type::I64),
            ("u32", Type::U32),
            ("u64", Type::U64),
            ("f32", Type::F32),
            ("f64", Type::F64),
            ("date", Type::Date),
            ("timestamp", Type::Timestamp),
            ("bool", Type::Bool),
            ("string", Type::String),
            ("file", Type::File),
        ];

        for (raw, expected) in primitives {
            let text = format!("type Foo: {raw}");

            let mut diagnostics = vec![];
            let tokens = tokenize(&text, &mut diagnostics);
            let res = parse(tokens);

            let mut oracle = Oracle::new_raw(&res.root);

            let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
            let id = oracle.parse_type(&typ).unwrap();

            assert_eq!(oracle.reports(), vec![]);
            assert_eq!(oracle.types.get(id), expected);

            oracle.source_map.get_bck(&Element::Type(id));
            assert_eq!(oracle.source_map.fwd.len(), 1);
            assert_eq!(oracle.source_map.bck.len(), 1);
        }
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
        oracle.source_map.get_bck(&Element::Type(arr.id));
        assert_eq!(oracle.source_map.fwd.len(), 2);
        assert_eq!(oracle.source_map.bck.len(), 2);
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
        let arr_inner = match arr.typ() {
            Type::Array(arr) => arr,
            _ => panic!(),
        };
        assert_eq!(arr_inner.typ(), Type::I32);

        oracle.source_map.get_bck(&Element::Type(id));
        oracle.source_map.get_bck(&Element::Type(arr.id));
        oracle.source_map.get_bck(&Element::Type(arr_inner.id));
        assert_eq!(oracle.source_map.fwd.len(), 3);
        assert_eq!(oracle.source_map.bck.len(), 3);
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
            type Foo: [union (string) {}]
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
                Report::error("inline union not allowed in array".to_string()).with_label(
                    Label::new("inline union type not allowed".to_string(), Span::new(11, 28))
                ).with_note("help: define a named type first, then reference it\n      example: `type MyUnion: union (\"discriminator\") { ... }`".to_string())
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
        oracle.source_map.get_bck(&Element::Type(opt.id));
        assert_eq!(oracle.source_map.fwd.len(), 2);
        assert_eq!(oracle.source_map.bck.len(), 2);
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
        oracle.source_map.get_bck(&Element::Type(opt.id));
        assert_eq!(oracle.source_map.fwd.len(), 2);
        assert_eq!(oracle.source_map.bck.len(), 2);
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
                Report::error("inline record not allowed in option".to_string())
                    .with_label(Label::new(
                        "inline record type not allowed".to_string(),
                        Span::new(10, 16)
                    ))
                    .with_note(
                        "help: define a named type first, then reference it\n      example: `type MyRecord: rec { ... }`"
                            .to_string()
                    )
            ]
        );

        assert_eq!(oracle.source_map.fwd.len(), 0);
        assert_eq!(oracle.source_map.bck.len(), 0);
    }

    #[test]
    fn parse_simple_record() {
        // Test parsing of simple records.
        // We have two records that are the same, but with different field ordering.
        // This test also checks that ordering of parsed is the same, which means we have
        // stable ordering.
        let text = indoc! {r#"
            type Foo: rec {
                foo: string
                bar: [i32]

                @default(69420)
                baz: u32
            }

            type Foo: rec {
                @default(69420)
                baz: u32
                bar: [i32]
                foo: string
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        assert_eq!(res.root.type_definitions().count(), 2);

        for typ in res.root.type_definitions() {
            let typ = typ.typ().unwrap();
            let id = oracle.parse_type(&typ).unwrap();

            assert_eq!(oracle.reports(), vec![]);

            // Check record was inserted and is in source map
            let rec = match oracle.types.get(id) {
                Type::Record(rec) => rec,
                _ => panic!(),
            };
            oracle.source_map.get_bck(&Element::Type(id));

            let fields: Vec<_> = rec.collect();

            // Check field names are correct
            let names: Vec<_> = fields
                .iter()
                .map(|field| oracle.strings.resolve(field.name).unwrap())
                .collect();
            assert_eq!(names, vec!["foo", "bar", "baz"]);

            // Check field @default's are correct
            let defaults: Vec<_> = fields
                .iter()
                .map(|field| field.default.map(|id| oracle.values.get(id)))
                .collect();
            assert_eq!(defaults, vec![None, None, Some(Value::Integer(69420))]);

            // Check default values are in source map
            for default_id in fields.iter().filter_map(|field| field.default) {
                oracle.source_map.get_bck(&Element::Value(default_id));
            }

            // Check fields and field types are in source map
            for field in &fields {
                oracle.source_map.get_bck(&Element::TypeField(field.id));
                oracle
                    .source_map
                    .get_bck(&Element::Type(field.id.type_id()));
            }
        }

        // Check source map size
        // Times two, since we have two of the same type
        assert_eq!(oracle.source_map.fwd.len(), 9 * 2);
        assert_eq!(oracle.source_map.bck.len(), 9 * 2);
    }

    #[test]
    fn parse_empty_record() {
        let text = indoc! {r#"
            type Foo: rec {}
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        let rec = match oracle.types.get(id) {
            Type::Record(rec) => rec,
            _ => panic!(),
        };
        oracle.source_map.get_bck(&Element::Type(id));

        assert_eq!(rec.count(), 0);

        // Check source map
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_invalid_record() {
        let text = indoc! {r#"
            type Foo: rec {
                invalid: rec {}
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(
            oracle.reports(),
            vec![
                Report::error("inline record not allowed in record field".to_string()).with_label(
                    Label::new("inline record type not allowed".to_string(), Span::new(29, 35))
                ).with_note("help: define a named type first, then reference it\n      example: `type MyRecord: rec { ... }`".to_string())
            ]
        );

        let rec = match oracle.types.get(id) {
            Type::Record(rec) => rec,
            _ => panic!(),
        };
        oracle.source_map.get_bck(&Element::Type(id));

        assert_eq!(rec.count(), 0);

        // Check source map
        assert_eq!(oracle.source_map.fwd.len(), 1);
        assert_eq!(oracle.source_map.bck.len(), 1);
    }

    #[test]
    fn parse_simple_enum() {
        let text = indoc! {r#"
            type Foo: enum (i32) {
                1
                2
                3
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        // Check enum was inserted and is in source map
        let enum_type = match oracle.types.get(id) {
            Type::Enum(e) => e,
            _ => panic!(),
        };
        oracle.source_map.get_bck(&Element::Type(id));

        // Check enum type is i32
        let enum_type_inner = enum_type.typ.unwrap();
        assert_eq!(enum_type_inner.typ(), Type::I32);

        // Check enum type is inside of source map
        oracle
            .source_map
            .get_bck(&Element::Type(enum_type_inner.id));

        // Check enum members
        let members: Vec<_> = match oracle.values.get(enum_type.values) {
            Value::Array(arr) => arr.collect(),
            _ => panic!(),
        };

        let member_vals: Vec<_> = members.iter().map(|m| m.value.clone()).collect();

        assert_eq!(
            member_vals,
            vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]
        );

        // Check enum members are in source map
        for member in members {
            oracle.source_map.get_bck(&Element::EnumMember(member.id));
            oracle.source_map.get_bck(&Element::Value(member.id));
        }

        // Check source map size
        assert_eq!(oracle.source_map.fwd.len(), 8);
        assert_eq!(oracle.source_map.bck.len(), 8);
    }

    #[test]
    fn parse_empty_enum() {
        let text = indoc! {r#"
            type Foo: enum (string) {}
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        let enum_type = match oracle.types.get(id) {
            Type::Enum(e) => e,
            _ => panic!(),
        };
        oracle.source_map.get_bck(&Element::Type(id));

        // Check enum members array is empty
        let members = match oracle.values.get(enum_type.values) {
            Value::Array(arr) => arr,
            _ => panic!(),
        };

        assert_eq!(members.count(), 0);

        // Check source map
        assert_eq!(oracle.source_map.fwd.len(), 2);
        assert_eq!(oracle.source_map.bck.len(), 2);
    }

    #[test]
    fn parse_invalid_enum() {
        let text = indoc! {r#"
            type Foo: enum ([i32]) {
                [1, 2, 3]
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        let enum_type = match oracle.types.get(id) {
            Type::Enum(e) => e,
            _ => panic!(),
        };
        oracle.source_map.get_bck(&Element::Type(id));

        assert_eq!(
            oracle.reports(),
            vec![
                Report::error("invalid enum type `array`".to_string())
                    .with_label(Label::new(
                        "invalid enum type `array`".to_string(),
                        Span::new(16, 21)
                    ))
                    .with_note(
                        "help: enum must have a type `i32`, `i64`, `u32`, `u64`, or `string`"
                            .to_string()
                    )
            ]
        );

        let members: Vec<_> = match oracle.values.get(enum_type.values) {
            Value::Array(arr) => arr.collect(),
            _ => panic!(),
        };

        assert_eq!(members.len(), 1);

        assert_eq!(oracle.source_map.fwd.len(), 8);
        assert_eq!(oracle.source_map.bck.len(), 8);
    }

    #[test]
    fn parse_simple_union() {
        let text = indoc! {r#"
            type Foo: union ("type") {
                foo: Foo

                // Default should be ignored
                @default(69420)
                bar: Bar
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        // Check union was inserted and is in source map
        let union = match oracle.types.get(id) {
            Type::Union(u) => u,
            _ => panic!(),
        };
        oracle.source_map.get_bck(&Element::Type(id));

        // Check discriminator
        let discriminator = union.disriminator.unwrap();
        assert_eq!(oracle.strings.resolve(discriminator).unwrap(), "type");

        let fields: Vec<_> = union.fields.collect();

        // Check field names are correct
        let names: Vec<_> = fields
            .iter()
            .map(|field| oracle.strings.resolve(field.name).unwrap())
            .collect();
        assert_eq!(names, vec!["foo", "bar"]);

        // Check field @default's are correct
        assert!(fields.iter().all(|field| field.default.is_none()));

        // Check fields and field types are in source map
        for field in &fields {
            oracle.source_map.get_bck(&Element::TypeField(field.id));
            oracle
                .source_map
                .get_bck(&Element::Type(field.id.type_id()));
        }

        // Check source map size
        assert_eq!(oracle.source_map.fwd.len(), 6);
        assert_eq!(oracle.source_map.bck.len(), 6);
    }

    #[test]
    fn parse_empty_union() {
        let text = indoc! {r#"
            type Foo: union ("kind") {}
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        let union = match oracle.types.get(id) {
            Type::Union(u) => u,
            _ => panic!(),
        };
        oracle.source_map.get_bck(&Element::Type(id));

        // Check discriminator
        let discriminator = union.disriminator.unwrap();
        assert_eq!(oracle.strings.resolve(discriminator).unwrap(), "kind");

        assert_eq!(union.fields.count(), 0);

        // Check source map
        assert_eq!(oracle.source_map.fwd.len(), 2);
        assert_eq!(oracle.source_map.bck.len(), 2);
    }

    #[test]
    fn parse_invalid_union() {
        let text = indoc! {r#"
            type Foo: union (42) {
                invalid: rec {}
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.parse_type(&typ).unwrap();

        assert_eq!(
            oracle.reports(),
            vec![
                Report::error("union discriminator must be a string, got integer".to_string()).with_label(Label::new("invalid union discriminator".to_string(), Span::new(17, 19))).with_note("help: union discrminator must be a string".to_string()),
                Report::error("inline record not allowed in union field".to_string()).with_label(
                    Label::new("inline record type not allowed".to_string(), Span::new(36, 42))
                ).with_note("help: define a named type first, then reference it\n      example: `type MyRecord: rec { ... }`".to_string())
            ]
        );

        let union = match oracle.types.get(id) {
            Type::Union(u) => u,
            _ => panic!(),
        };
        oracle.source_map.get_bck(&Element::Type(id));

        // Check discriminator
        assert!(union.disriminator.is_none());

        assert_eq!(union.fields.count(), 0);

        // Check source map
        assert_eq!(oracle.source_map.fwd.len(), 2);
        assert_eq!(oracle.source_map.bck.len(), 2);
    }
}
