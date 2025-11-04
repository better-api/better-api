use std::borrow::Cow;

use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::ast;
use better_api_syntax::ast::AstNode;

use crate::Element;
use crate::oracle::InternedField;
use crate::text::{parse_string, validate_name};
use crate::value::{PrimitiveValue, ValueId};

use super::Oracle;

impl Oracle {
    pub fn parse_value(&mut self, value: &ast::Value) -> Option<ValueId> {
        let id = match value {
            ast::Value::Object(object) => self.parse_object(object)?,
            _ => {
                let primitive = self.parse_primitive(value)?;
                self.values.add_primitive(primitive)
            }
        };

        self.source_map.insert(value, Element::Value(id));
        Some(id)
    }

    /// Helper function for parsing primitive values. If it's called on non primitive value
    /// (object or array) it will panic.
    fn parse_primitive(&mut self, value: &ast::Value) -> Option<PrimitiveValue> {
        let val = match value {
            ast::Value::String(string) => {
                let token = string.string()?;
                let parsed_str = parse_string(&token, &mut self.reports);
                let str_id = self.strings.get_or_intern(&parsed_str);

                PrimitiveValue::String(str_id)
            }
            ast::Value::Integer(integer) => PrimitiveValue::Integer(integer.integer()?),
            ast::Value::Float(float) => PrimitiveValue::Float(float.float()?),
            ast::Value::Bool(bool) => PrimitiveValue::Bool(bool.bool()?),
            ast::Value::Object(_) => {
                unreachable!("parse_primitive should be called with primitive value")
            }
        };

        Some(val)
    }

    fn parse_object(&mut self, object: &ast::Object) -> Option<ValueId> {
        // Gather object fields. It gathers only fields that have a value and valid name.
        self.object_fields_buf.clear();
        self.object_fields_buf
            .extend(object.fields().filter_map(|f| {
                f.value()?;

                let name = f.name().and_then(|n| n.token())?;
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
            }));

        self.object_fields_buf.sort_unstable_by_key(|f| f.name);

        self.check_object_fields_unique();

        let mut builder = self.values.start_object();
        for field in &self.object_fields_buf {}

        Some(builder.finish())
    }

    // Check that names of object fields present in self.objects_fields_buf are unique.
    // Fields are expected to be sorted by name.
    fn check_object_fields_unique(&mut self) {
        for idx in 0..(self.object_fields_buf.len() - 1) {
            if self.object_fields_buf[idx].name != self.object_fields_buf[idx + 1].name {
                continue;
            }

            let name = self
                .strings
                .resolve(self.object_fields_buf[idx].name)
                .expect("interned string should be present in interner");

            let range = self.object_fields_buf[idx + 1]
                .field
                .name()
                .expect("collected object field should have a name")
                .syntax()
                .text_range();

            self.reports.push(
                Report::error(format!("repeated object key `{name}`")).with_label(Label::new(
                    "repeated object key".to_string(),
                    Span::new(range.start().into(), range.end().into()),
                )),
            );
        }
    }
}
