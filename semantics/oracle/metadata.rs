use better_api_diagnostic::{Label, Report};
use better_api_syntax::ast::AstNode;
use better_api_syntax::{TextRange, ast};

use crate::spec;
use crate::text::{lower_name, parse_string};

use super::Oracle;

impl<'a> Oracle<'a> {
    pub(crate) fn lower_metadata(&mut self) {
        let mut res_version = None;
        for (idx, version) in self.root.api_versions().enumerate() {
            if idx > 0 {
                self.emit_repeated_report("version", version.syntax().text_range());
            }

            let version_id = version
                .value()
                .and_then(|ast_value| self.lower_string_directive(&ast_value, "version"));
            if let Some(version_id) = version_id {
                res_version.get_or_insert(version_id);
            }
        }
        if res_version.is_none() && self.root.api_versions().next().is_some() {
            self.emit_missing_directive_report("version", self.root.syntax().text_range());
        }

        let mut res_name = None;
        for (idx, name) in self.root.api_names().enumerate() {
            if idx > 0 {
                self.emit_repeated_report("name", name.syntax().text_range());
            }

            let name_id = name
                .value()
                .and_then(|ast_value| self.lower_string_directive(&ast_value, "name"));
            if let Some(name_id) = name_id {
                res_name.get_or_insert(name_id);
            }
        }
        if res_name.is_none() && self.root.api_names().next().is_some() {
            self.emit_missing_directive_report("name", self.root.syntax().text_range());
        }

        let mut res_api_version = None;
        for (idx, version) in self.root.better_api_versions().enumerate() {
            if idx > 0 {
                self.emit_repeated_report("betterApi", version.syntax().text_range());
            }

            let api_version_id = version
                .value()
                .and_then(|ast_value| self.lower_string_directive(&ast_value, "betterApi"));
            if let Some(version_id) = api_version_id {
                res_api_version.get_or_insert(version_id);
            }
        }
        if res_api_version.is_none() && self.root.better_api_versions().next().is_some() {
            self.emit_missing_directive_report("betterApi", self.root.syntax().text_range());
        }

        let servers: Vec<_> = self
            .root
            .servers()
            .filter_map(|srv| self.lower_server(&srv))
            .collect();

        println!("{res_version:?}, {res_name:?}, {res_api_version:?}");
        if let (Some(version), Some(name), Some(api_version)) =
            (res_version, res_name, res_api_version)
        {
            self.metadata = Some(spec::Metadata {
                better_api_version: api_version,
                version,
                name,
                servers,

                // TODO: Get description
                description: None,
            })
        }
    }

    /// Analyze directive that expects value to be a string.
    /// This is `version`, `name`, `betterApi` and server properties
    fn lower_string_directive(
        &mut self,
        value_node: &ast::Value,
        directive: &str,
    ) -> Option<String> {
        let ast::Value::String(str_node) = value_node else {
            let range = value_node.syntax().text_range();
            self.reports.push(
                Report::error(format!("`{directive}` must be a string, got {value_node}"))
                    .add_label(Label::primary(
                        "expected a string".to_string(),
                        range.into(),
                    )),
            );
            return None;
        };

        let token = str_node.string();
        let parsed = parse_string(&token, &mut self.reports);
        Some(parsed.to_string())
    }

    /// Emits report for repeated directive (`version`, `name` or `betterApi`)
    fn emit_repeated_report(&mut self, directive: &str, range: TextRange) {
        self.reports.push(
            Report::error(format!("repeated `{directive}` directive"))
                .add_label(Label::primary(
                    format!("repeated `{directive}` directive"),
                    range.into(),
                ))
                .with_note(format!("help: provide only one `{directive}`")),
        );
    }

    /// Emits report for missing directive value (`version`, `name` or `betterApi`)
    fn emit_missing_directive_report(&mut self, directive: &str, range: TextRange) {
        self.reports.push(
            Report::error(format!("missing `{directive}` directive value"))
                .add_label(Label::primary(
                    format!("missing `{directive}` directive value"),
                    range.into(),
                ))
                .with_note(format!("help: provide `{directive}` value")),
        );
    }

    fn lower_server(&mut self, server: &ast::Server) -> Option<spec::Server> {
        let value = server.value()?;
        let ast::Value::Object(object) = value else {
            let range = server.syntax().text_range();
            self.reports.push(
                Report::error(format!("`server` must be an object, got {value}")).add_label(
                    Label::primary("expected an object".to_string(), range.into()),
                ),
            );

            return None;
        };

        let name = self.get_server_field(&object, "name");
        let url = self.get_server_field(&object, "url");

        if let (Some(name), Some(url)) = (name, url) {
            Some(spec::Server {
                name,
                url,
                // TODO: Extract docs
                docs: None,
            })
        } else {
            None
        }
    }

    fn get_server_field(&mut self, server: &ast::Object, field_name: &str) -> Option<String> {
        let mut count = 0;
        let mut res = None;
        for field in server.fields() {
            let value = field.value()?;

            let name_token = field.name()?;
            let name_id = lower_name(&name_token, &mut self.strings, &mut self.reports)?;
            if self.strings.resolve(name_id) != field_name {
                continue;
            }

            count += 1;
            if count > 1 {
                self.reports.push(
                    Report::error(format!("repeated `{field_name}` field"))
                        .add_label(Label::primary(
                            format!("repeated `{field_name}` field"),
                            field.syntax().text_range().into(),
                        ))
                        .with_note(format!("help: provide only one `{field_name}` field")),
                );
            }

            let ast::Value::String(str_node) = value else {
                let range = value.syntax().text_range();
                self.reports.push(
                    Report::error(format!(
                        "`server.{field_name}` must be a string, got {value}"
                    ))
                    .add_label(Label::primary(
                        "expected a string".to_string(),
                        range.into(),
                    )),
                );
                continue;
            };

            let token = str_node.string();
            let parsed = parse_string(&token, &mut self.reports);
            res = Some(parsed.to_string())
        }

        if count == 0 {
            self.reports.push(
                Report::error(format!("missing `server.{field_name}` field"))
                    .add_label(Label::primary(
                        format!("missing `server.{field_name}` field"),
                        server.syntax().text_range().into(),
                    ))
                    .with_note(format!("help: provide `server.{field_name}` field")),
            );
        }

        res
    }
}

#[cfg(test)]
mod test {
    use better_api_syntax::{parse, tokenize};
    use indoc::indoc;

    use crate::Oracle;

    #[test]
    fn lower_basic_metadata() {
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

            // Servers
            server: { 
              name: "prod" 
              url: "https://api.example.com"
            }
            server: { 
              name: "staging"  
              url: "https://staging.example.com"
            }
            server: {
              url: 42
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let oracle = Oracle::new(&res.root);
        insta::assert_debug_snapshot!(oracle.reports());

        let metadata = oracle.metadata.as_ref().expect("metadata should be set");
        assert_eq!(metadata.version, "1.2");
        assert_eq!(metadata.name, "test");
        assert_eq!(metadata.better_api_version, "0.1");
        assert_eq!(metadata.servers.len(), 2);
        assert_eq!(metadata.servers[0].name, "prod");
        assert_eq!(metadata.servers[0].url, "https://api.example.com");
        assert_eq!(metadata.servers[1].name, "staging");
        assert_eq!(metadata.servers[1].url, "https://staging.example.com");
    }
}
