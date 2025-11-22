use better_api_diagnostic::{Label, Report, Span};

use super::Parser;
use crate::Kind::{self, *};
use crate::Token;

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    /// Parses value node.
    ///
    /// If error is found, error node will be emitted. Error node consumes
    /// until EOF is reached, or until recovery token is found `is_recovery(token) == true`
    pub fn parse_value<F: Fn(Kind) -> bool>(&mut self, is_recovery: F) {
        match self.peek() {
            Some(TOKEN_STRING) => self.with(NODE_VALUE_STRING, Self::advance),
            Some(TOKEN_INTEGER) => self.with(NODE_VALUE_INTEGER, Self::advance),
            Some(TOKEN_FLOAT) => self.with(NODE_VALUE_FLOAT, Self::advance),
            Some(TOKEN_KW_TRUE) | Some(TOKEN_KW_FALSE) => self.with(NODE_VALUE_BOOL, Self::advance),

            Some(TOKEN_BRACKET_LEFT) => self.parse_array(),
            Some(TOKEN_CURLY_LEFT) => self.parse_object(),

            Some(kind) => {
                let span = self.parse_error(&is_recovery);
                self.reports.push(
                    Report::error(format!("expected value, found {kind}"))
                        .add_label(Label::primary("expected value".to_string(), span)),
                );
            }
            None => {
                self.reports.push(
                    Report::error("expected value, found end of file".to_string()).add_label(
                        Label::primary(
                            "expected value".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        ),
                    ),
                );
            }
        }
    }

    fn parse_array(&mut self) {
        self.builder.start_node(NODE_VALUE_ARRAY.into());

        self.expect(TOKEN_BRACKET_LEFT);

        loop {
            self.skip_whitespace_eol();
            match self.peek() {
                Some(TOKEN_COMMENT) => {
                    self.advance();
                    self.expect(TOKEN_EOL);
                }

                Some(TOKEN_BRACKET_RIGHT) => {
                    self.advance();
                    break;
                }

                Some(_) => {
                    self.parse_value(|kind| kind == TOKEN_COMMA || kind == TOKEN_BRACKET_RIGHT);
                    self.skip_whitespace();

                    // Continue normally on valid delimiter
                    if self.peek() == Some(TOKEN_COMMA) {
                        self.advance();
                        continue;
                    }

                    // Skip all white space and check that array is ended with `]`
                    self.skip_whitespace_eol();
                    if self.peek() == Some(TOKEN_BRACKET_RIGHT) {
                        self.advance();
                        break;
                    }

                    // We don't have a delimiter and array is not ended yet with `]`
                    // We report a missing delimiter error
                    let mut report = Report::error(format!(
                        "expected `,` or `]`, found {0}",
                        self.peek()
                            .map_or_else(|| "end of file".to_string(), |k| k.to_string())
                    ))
                    .add_label(Label::primary(
                        "unexpected token".to_string(),
                        Span::new(
                            self.pos,
                            self.pos + self.peek_value().map_or(1, |s| s.len()),
                        ),
                    ));

                    // Help is appended only if there is more stuff in array (not EOF yet). Otherwise
                    // error message looks out of place
                    if self.peek().is_some() {
                        report = report
                            .with_note("help: array values must be separated with `,`".to_string());
                    }

                    self.reports.push(report);

                    // If we are at EOF, we break here instead of in the next iteration of the
                    // loop. This way we avoid reporting EOF multiple times and make diagnostics
                    // more readable.
                    if self.peek().is_none() {
                        break;
                    }
                }

                None => {
                    self.reports.push(
                        Report::error("expected array member, found end of file".to_string())
                            .add_label(Label::primary(
                                "expected array member".to_string(),
                                Span::new(self.pos, self.pos + 1),
                            )),
                    );
                    break;
                }
            }
        }

        self.builder.finish_node();
    }

    fn parse_object(&mut self) {
        self.builder.start_node(NODE_OBJECT.into());

        self.expect(TOKEN_CURLY_LEFT);

        loop {
            self.skip_whitespace_eol();
            match self.peek() {
                Some(TOKEN_COMMENT) => {
                    self.advance();
                    self.expect(TOKEN_EOL);
                }

                Some(TOKEN_IDENTIFIER) | Some(TOKEN_STRING) => {
                    self.builder.start_node(NODE_OBJECT_FIELD.into());

                    self.builder.start_node(NODE_NAME.into());
                    self.advance();
                    self.builder.finish_node();

                    self.assignment();

                    self.parse_value(|token| token == TOKEN_CURLY_RIGHT);
                    self.skip_whitespace();
                    self.expect(TOKEN_EOL);

                    self.builder.finish_node();
                }

                Some(TOKEN_CURLY_RIGHT) => {
                    self.advance();
                    break;
                }

                Some(kind) => {
                    let span = self.parse_error(|token| token == TOKEN_CURLY_RIGHT);
                    self.reports.push(
                        Report::error(format!("expected field name, found {kind}"))
                            .add_label(Label::primary("expected field name".to_string(), span))
                            .with_note(
                                "help: field name must be an identifier or string".to_string(),
                            ),
                    );
                }
                None => {
                    self.reports.push(
                        Report::error("expected field name, found end of file".to_string())
                            .add_label(Label::primary(
                                "expected field name".to_string(),
                                Span::new(self.pos, self.pos + 1),
                            ))
                            .with_note(
                                "help: field name must be an identifier or string".to_string(),
                            ),
                    );
                    break;
                }
            }
        }

        self.builder.finish_node();
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;

    use crate::{parse, tokenize};

    #[test]
    fn parse_value() {
        let text = indoc! {r#"
            name: false
            name: true
            name: "string"
            name: 69
            name: 4.20
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        assert_eq!(res.reports, vec![]);
    }

    #[test]
    fn parse_array() {
        let text = indoc! {r#"
            name: [true]
            name: [true, false]
            name: [true, false, 10,]
            name: []
            name: ["deeply", ["nested"]]
            name: [
                // Comment
                true,  // another one
               
                
                false 10
                "something"
            ]
            name: ["unfinished"
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        insta::assert_debug_snapshot!(res.reports);
    }

    #[test]
    fn parse_empty_object() {
        let text = indoc! {r#"
            server: {}
            server: {

            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        assert_eq!(res.reports, vec![]);
    }
}
