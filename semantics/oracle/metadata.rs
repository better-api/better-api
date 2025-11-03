use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::ast;
use better_api_syntax::ast::AstNode;

use crate::value::Value;

use super::Oracle;

impl Oracle {
    pub fn analyze_metadata(&mut self, root: &ast::Root) {
        for (idx, version) in root.api_versions().enumerate() {
            if idx > 0 {
                let range = version.syntax().text_range();
                self.reports.push(
                    Report::error("repeated `version` directive".to_string())
                        .with_label(Label::new(
                            "repeated `version` directive".to_string(),
                            Span::new(range.start().into(), range.end().into()),
                        ))
                        .with_note("help: provide only one `version`".to_string()),
                );
            }
            self.analyze_api_version(&version);
        }
    }

    fn analyze_api_version(&mut self, version: &ast::ApiVersion) -> Option<()> {
        let ast_value = version.value()?;
        let value_id = self.parse_value(&ast_value)?;
        let value = self.values.get(value_id);

        if !matches!(value, Value::String(_)) {
            let range = ast_value.syntax().text_range();
            self.reports.push(
                Report::error(format!("`version` must be a string, got {value}")).with_label(
                    Label::new(
                        "expected a string".to_string(),
                        Span::new(range.start().into(), range.end().into()),
                    ),
                ),
            );
        };

        Some(())
    }
}

#[cfg(test)]
mod test {
    use better_api_syntax::{parse, tokenize};
    use indoc::indoc;

    use crate::Oracle;

    #[test]
    fn analyze_api_version() {
        let text = indoc! {r#"
            version: "1.2"
            version: 42
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let oracle = Oracle::new(&res.root);
        insta::assert_debug_snapshot!(oracle.reports());
    }
}
