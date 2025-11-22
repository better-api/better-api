use std::borrow::Cow;

use better_api_diagnostic::{Label, Report};
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
    /// Lowers type definitions and checks for cycles.
    ///
    /// The lowered types are not validated completely, that has to be done
    /// after _all_ the types have been lowered.
    pub(crate) fn lower_type_definitions(&mut self, root: &ast::Root) {
        for def in root.type_definitions() {
            self.lower_type_def(&def);
        }

        self.report_symbol_cycles();
    }

    /// Lowers type definition node.
    fn lower_type_def(&mut self, def: &ast::TypeDefinition) {
        let Some(name_id) = def.name().and_then(|n| self.lower_name(&n)) else {
            return;
        };

        let Some(type_id) = def.typ().and_then(|t| self.lower_type(&t)) else {
            return;
        };

        // There is already a symbol with the same name, so we report an error.
        if self.symbol_table.contains_key(&name_id) {
            let name = self
                .strings
                .resolve(name_id)
                // We intern the string at the beginning of the function.
                .expect("interned string should be resolvable");
            let range = def
                .name()
                // We check name node exists at the beginning of the function.
                .expect("name should exist here")
                .syntax()
                .text_range();

            // If symbol table contains the name already, we should also have a valid type def
            // node, so expects should be fine.
            let original_def = self
                .type_def_node(name_id)
                .expect("original type definition should exist");
            let original_range = original_def
                .name()
                .expect("name of original type def should exist")
                .syntax()
                .text_range();

            self.reports.push(
                Report::error(format!("name `{name}` is defined multiple times"))
                    .add_label(Label::primary(
                        format!("name `{name}` is defined multiple times"),
                        range.into(),
                    ))
                    .add_label(Label::secondary(
                        format!("name `{name}` is first defined here"),
                        original_range.into(),
                    )),
            );

            return;
        };

        self.symbol_table.insert(name_id, type_id);
        self.source_map
            .insert(def, Element::TypeDefinition(name_id));
    }

    /// Lower a syntactical type and store it into the type arena and source map.
    ///
    /// Returns [`TypeId`] if type could be parsed, and `None` otherwise.
    ///
    /// If type was lowered successfully it's not yet completely validated.
    /// This has to be done after _all_ the types have been lowered.
    pub(crate) fn lower_type(&mut self, typ: &ast::Type) -> Option<TypeId> {
        let id = match ParsedType::new(typ, &mut self.strings) {
            ParsedType::Primitive(primitive) => self.types.add_primitive(primitive),
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
                    &mut self.source_map,
                    "array",
                )?
            }
            ParsedType::Option(opt) => {
                let inner = opt.typ()?;
                let builder = self.types.start_option();
                lower_array_option(
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

    /// Lowers enum type and inserts it into arena
    fn lower_enum(&mut self, typ: &ast::Enum) -> TypeId {
        // TODO: Validate members types. For this value -> type comparison function is needed.

        // Parse type of the enum and validate it
        // Reports are handled by parser and self.parse_type methods already
        // TODO: move this in the shared type validation logic
        let enum_type_id = typ.typ().and_then(|t| self.lower_type(&t));
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

    /// Lowers response type and inserts it into arena
    fn lower_response(&mut self, resp: &ast::TypeResponse) -> TypeId {
        // Parse and validate content type
        let content_type_id = resp
            .content_type()
            .and_then(|v| v.value())
            .map(|v| self.lower_value(&v));
        // TODO: Move validation to shared validation logic.
        if let Some(id) = content_type_id {
            self.validate_response_content_type(id);
        }

        // Parse and validate header type
        let headers_id = resp
            .headers()
            .and_then(|h| h.typ())
            .and_then(|t| self.lower_type(&t));
        // TODO: Check that headers type is a record. Do not forget to resolve named references.

        // Parse and validate response body
        let body_id = resp
            .body()
            .and_then(|b| b.typ())
            .and_then(|t| self.lower_type(&t));
        // TODO: Check that body type is valid (not a response). Again, resolve named references

        // TODO: Check response body type and header mime type match.

        self.types
            .add_response(body_id, headers_id, content_type_id)
    }

    /// Lowers record type and inserts it into arena
    fn lower_record(&mut self, record: &ast::Record) -> TypeId {
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

    /// Lowers union type and inserts it into arena
    fn lower_union(&mut self, union: &ast::Union) -> TypeId {
        let discriminator = union
            .discriminator()
            .map(|v| (self.lower_value(&v), v.syntax().text_range()))
            .and_then(|(id, range)| match self.values.get(id) {
                Value::String(id) => Some(id),
                val => {
                    self.reports.push(
                        Report::error(format!("union discriminator must be a string, got {val}"))
                            .add_label(Label::primary(
                                "invalid union discriminator".to_string(),
                                range.into(),
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

                let name_id = f.name().and_then(|n| self.lower_name(&n))?;

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
                    default,
                    field: f,
                })
            })
            .collect();

        fields.sort_by_key(|f| f.name);
        // TODO: Check fields are unique in the place where type validation happens

        fields
    }

    /// Lowers name by parsing, validating and interning it.
    ///
    /// Returns interned string id of the name if it's valid.
    fn lower_name(&mut self, name: &ast::Name) -> Option<StringId> {
        let token = name.token();

        let name_str: Cow<_> = match &token {
            ast::NameToken::Identifier(ident) => ident.text().into(),
            ast::NameToken::String(string) => parse_string(string, &mut self.reports),
        };

        if let Err(report) = validate_name(&name_str, token.text_range()) {
            self.reports.push(report);
            return None;
        }

        let name_id = self.strings.get_or_intern(name_str);
        Some(name_id)
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
                        .add_label(Label::primary(
                            format!("invalid enum type {typ}"),
                            range.into(),
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
                        .add_label(Label::primary(
                            format!("invalid response content type {val}"),
                            range.into(),
                        ))
                        .with_note("help: response content type must be a string".to_string()),
                );
            }
        }
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
            let container_id = lower_array_option(
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
            let container_id = lower_array_option(
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
                let type_id = lower_array_option(
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
                let type_id = lower_array_option(
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
