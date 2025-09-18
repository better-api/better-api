use better_api_diagnostic::{Label, Report, Span};

use super::Parser;
use crate::Kind::*;
use crate::Token;

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    // TODO: Recovery symbol is a function that returns true when recovery symbol is found.
    // For starters, EOF is always a recovery symbols without specifying.
    // Same approach has to be taken for parsing types.
    //
    // TODO: When parsing object/record field, if first token is not ident or string, parse error token
    // until EOL. This you won't half parse a field.
    pub fn parse_value(&mut self) {
        self.builder.start_node(NODE_VALUE.into());

        match self.peek() {
            Some(TOKEN_STRING) => self.advance(),
            Some(TOKEN_INTEGER) => self.advance(),
            Some(TOKEN_FLOAT) => self.advance(),

            Some(TOKEN_KW_TRUE) | Some(TOKEN_KW_FALSE) => {
                self.builder.start_node(NODE_BOOL_VALUE.into());
                self.advance();
                self.builder.finish_node();
            }

            Some(TOKEN_CURLY_LEFT) => self.parse_object(),

            Some(kind) => {
                let span = self.parse_error(|_| false);
                self.reports.push(
                    Report::error(format!("expected value, found {kind}"))
                        .with_label(Label::new("expected value".to_string(), span)),
                );
            }
            None => {
                self.reports.push(
                    Report::error("expected value, found end of file".to_string()).with_label(
                        Label::new(
                            "expected value".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        ),
                    ),
                );
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

                    self.parse_value();
                    self.skip_whitespace();
                    self.expect(TOKEN_EOL);

                    self.builder.finish_node();
                }

                Some(TOKEN_CURLY_RIGHT) => {
                    self.advance();
                    break;
                }

                Some(kind) => {
                    let span = self.parse_error(|_| false);
                    self.reports.push(
                        Report::error(format!("expected field name, found {kind}"))
                            .with_label(Label::new("expected field name".to_string(), span))
                            .with_note(
                                "help: field name must be an identifier or string".to_string(),
                            ),
                    );
                }
                None => self.reports.push(
                    Report::error("expected field name, found end of file".to_string())
                        .with_label(Label::new(
                            "expected field name".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        ))
                        .with_note("help: field name must be an identifier or string".to_string()),
                ),
            }
        }

        self.builder.finish_node();
    }
}
