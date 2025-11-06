use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::ast::AstNode;
use better_api_syntax::{TextRange, ast};

use crate::value::Value;

use super::Oracle;

impl<'a> Oracle<'a> {
    pub(crate) fn analyze_metadata(&mut self, root: &ast::Root) {
        for (idx, version) in root.api_versions().enumerate() {
            if idx > 0 {
                self.emit_repeated_report("version", version.syntax().text_range());
            }

            if let Some(ast_value) = version.value() {
                self.analyze_string_directive(&ast_value, "version");
            }
        }

        for (idx, name) in root.api_names().enumerate() {
            if idx > 0 {
                self.emit_repeated_report("name", name.syntax().text_range());
            }
            if let Some(ast_value) = name.value() {
                self.analyze_string_directive(&ast_value, "name");
            }
        }

        for (idx, version) in root.better_api_versions().enumerate() {
            if idx > 0 {
                self.emit_repeated_report("betterApi", version.syntax().text_range());
            }

            if let Some(ast_value) = version.value() {
                self.analyze_string_directive(&ast_value, "betterApi");
            }
        }

        for server in root.servers() {
            let Some(server_value) = server.value() else {
                continue;
            };

            let server_id = self.parse_value(&server_value);
            // TODO: Check server is valid:
            //  - Implement `parse_type`
            //  - Parse "hardcoded" server type
            //  - Implement comparison of value and type
            //  - Check server value matches server type
        }
    }

    /// Analyze directive that expects value to be a string.
    /// This is `version`, `name` and `betterApi`
    fn analyze_string_directive(&mut self, value_node: &ast::Value, directive: &str) {
        let value_id = self.parse_value(value_node);
        let value = self.values.get(value_id);

        if !matches!(value, Value::String(_)) {
            let range = value_node.syntax().text_range();
            self.reports.push(
                Report::error(format!("`{directive}` must be a string, got {value}")).with_label(
                    Label::new(
                        "expected a string".to_string(),
                        Span::new(range.start().into(), range.end().into()),
                    ),
                ),
            );
        }
    }

    /// Emits report for repeated directive (`version`, `name` or `betterApi`)
    fn emit_repeated_report(&mut self, directive: &str, range: TextRange) {
        self.reports.push(
            Report::error(format!("repeated `{directive}` directive"))
                .with_label(Label::new(
                    format!("repeated `{directive}` directive"),
                    Span::new(range.start().into(), range.end().into()),
                ))
                .with_note(format!("help: provide only one `{directive}`")),
        );
    }
}

#[cfg(test)]
mod test {
    use better_api_syntax::{parse, tokenize};
    use indoc::indoc;

    use crate::Oracle;

    #[test]
    fn analyze_basic_metadata() {
        let text = indoc! {r#"
            // Versions
            version: "1.2"
            version: 42

            // Name
            name: "test"
            name: false

            // Better API Version
            betterApi: "0.1"
            betterApi: 1.23
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let oracle = Oracle::new(&res.root);
        insta::assert_debug_snapshot!(oracle.reports());
    }
}
