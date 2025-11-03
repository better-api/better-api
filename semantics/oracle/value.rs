use better_api_syntax::ast;

use crate::Element;
use crate::text::parse_string;
use crate::value::{PrimitiveValue, ValueId};

use super::Oracle;

impl Oracle {
    pub fn parse_value(&mut self, value: &ast::Value) -> Option<ValueId> {
        let id = match value {
            ast::Value::String(string) => {
                let token = string.string()?;
                let parsed_str = parse_string(&token, &mut self.reports);
                let str_id = self.strings.get_or_intern(&parsed_str);

                self.values.add_primitive(PrimitiveValue::String(str_id))
            }
            ast::Value::Integer(integer) => self
                .values
                .add_primitive(PrimitiveValue::Integer(integer.integer()?)),
            ast::Value::Float(float) => self
                .values
                .add_primitive(PrimitiveValue::Float(float.float()?)),
            ast::Value::Bool(bool) => self
                .values
                .add_primitive(PrimitiveValue::Bool(bool.bool()?)),
            ast::Value::Object(object) => todo!(),
        };

        self.source_map.insert(value, Element::Value(id));
        Some(id)
    }
}
