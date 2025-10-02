use better_api_diagnostic::{Label, Report, Span};

use super::Parser;
use super::prologue::Prologue;
use crate::Kind::*;
use crate::Token;
use crate::parser::basic::PrologueBehavior;
use crate::parser::types::DefaultCompositeType;

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    pub fn parse_endpoint(&mut self, prologue: Option<Prologue>) {
        self.start_node(NODE_ENDPOINT, prologue, PrologueBehavior::NoDefault);

        debug_assert!(matches!(
            self.peek(),
            Some(TOKEN_KW_GET | TOKEN_KW_POST | TOKEN_KW_PUT | TOKEN_KW_DELETE | TOKEN_KW_PATCH)
        ));

        self.builder.start_node(NODE_ENDPOINT_METHOD.into());
        self.advance();
        self.builder.finish_node();

        self.skip_whitespace();

        // Parse optional endpoint url
        match self.peek() {
            Some(TOKEN_STRING) => {
                self.builder.start_node(NODE_PATH.into());
                self.advance();
                self.builder.finish_node();
            }
            Some(TOKEN_CURLY_LEFT) => (), // Do nothing

            Some(kind) => {
                let span = self.parse_error(|token| token == TOKEN_CURLY_LEFT);

                self.reports.push(
                    Report::error(format!("expected endpoint path, found {kind}"))
                        .with_label(Label::new("expected endpoint path".to_string(), span)),
                );
            }
            None => {
                self.reports.push(
                    Report::error("expected endpoint path, found end of file".to_string())
                        .with_label(Label::new(
                            "expected endpoint path".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        )),
                );

                self.builder.finish_node(); // Finish endpoint
                return;
            }
        }

        self.skip_whitespace();
        self.expect(TOKEN_CURLY_LEFT);
        self.skip_whitespace();
        self.expect(TOKEN_EOL);

        self.parse_endpoint_properties();

        self.skip_whitespace();
        self.expect(TOKEN_EOL);

        self.builder.finish_node();
    }

    pub fn parse_route(&mut self, prologue: Option<Prologue>) {
        self.start_node(NODE_ROUTE, prologue, PrologueBehavior::Ignore);

        debug_assert!(self.peek() == Some(TOKEN_KW_ROUTE));
        self.advance();
        self.skip_whitespace();

        match self.peek() {
            Some(TOKEN_STRING) => {
                self.builder.start_node(NODE_PATH.into());
                self.advance();
                self.builder.finish_node();
            }

            Some(kind) => {
                let span = self.parse_error(|token| token == TOKEN_CURLY_LEFT);

                self.reports.push(
                    Report::error(format!("expected route path, found {kind}"))
                        .with_label(Label::new("expected route path".to_string(), span)),
                );
            }
            None => {
                self.reports.push(
                    Report::error("expected route path, found end of file".to_string()).with_label(
                        Label::new(
                            "expected route path".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        ),
                    ),
                );

                self.builder.finish_node(); // Finish route
                return;
            }
        }

        self.skip_whitespace();
        self.expect(TOKEN_CURLY_LEFT);
        self.skip_whitespace();
        self.expect(TOKEN_EOL);

        self.parse_route_properties();

        self.skip_whitespace();
        self.expect(TOKEN_EOL);

        self.builder.finish_node();
    }

    fn parse_endpoint_properties(&mut self) {
        let is_recovery = |token| token == TOKEN_CURLY_RIGHT;
        loop {
            let prologue = self.parse_prologue();

            match self.peek() {
                Some(TOKEN_CURLY_RIGHT) => {
                    if let Some(prologue) = prologue
                        && let Some(report) = prologue.expect_no_default()
                    {
                        self.reports.push(report);
                    }
                    self.advance();
                    break;
                }

                Some(TOKEN_IDENTIFIER) => match self.peek_value() {
                    Some("name") => {
                        self.parse_field(NODE_NAME, prologue, PrologueBehavior::Ignore, |p| {
                            p.parse_value(is_recovery)
                        });
                    }

                    Some("path") => {
                        self.parse_field(
                            NODE_ENDPOINT_PATH,
                            prologue,
                            PrologueBehavior::Ignore,
                            |p| p.parse_type(DefaultCompositeType::Record, is_recovery),
                        );
                    }
                    Some("query") => {
                        self.parse_field(
                            NODE_ENDPOINT_QUERY,
                            prologue,
                            PrologueBehavior::Ignore,
                            |p| p.parse_type(DefaultCompositeType::Record, is_recovery),
                        );
                    }
                    Some("headers") => {
                        self.parse_field(
                            NODE_ENDPOINT_HEADERS,
                            prologue,
                            PrologueBehavior::Ignore,
                            |p| p.parse_type(DefaultCompositeType::Record, is_recovery),
                        );
                    }

                    Some("accept") => {
                        self.parse_field(
                            NODE_ENDPOINT_ACCEPT,
                            prologue,
                            PrologueBehavior::Ignore,
                            |p| p.parse_value(is_recovery),
                        );
                    }
                    Some("requestBody") => {
                        self.parse_field(
                            NODE_ENDPOINT_REQUEST_BODY,
                            prologue,
                            PrologueBehavior::NoDefault,
                            |p| p.parse_type(DefaultCompositeType::None, is_recovery),
                        );
                    }

                    Some(field) => {
                        let report_msg = format!("invalid endpoint field `{field}`");

                        let span = self.parse_error(|token| token == TOKEN_CURLY_RIGHT);
                        self.reports.push(
                            Report::error(report_msg)
                                .with_label(Label::new("invalid endpoint field".to_string(), span)),
                        );
                    }
                    None => unreachable!(),
                },

                Some(TOKEN_KW_ON) => self.parse_endpoint_response(prologue),

                Some(kind) => {
                    let span = self.parse_error(is_recovery);

                    self.reports.push(
                        Report::error(format!("expected field name, found {kind}"))
                            .with_label(Label::new("expected field name".to_string(), span)),
                    );
                }
                None => {
                    self.reports.push(
                        Report::error("expected field name, found end of file".to_string())
                            .with_label(Label::new(
                                "expected field name".to_string(),
                                Span::new(self.pos, self.pos + 1),
                            )),
                    );
                    break;
                }
            }
        }
    }

    fn parse_route_properties(&mut self) {
        let is_recovery = |token| token == TOKEN_CURLY_RIGHT;

        loop {
            let prologue = self.parse_prologue();

            match self.peek() {
                Some(TOKEN_CURLY_RIGHT) => {
                    if let Some(prologue) = prologue
                        && let Some(report) = prologue.expect_no_default()
                    {
                        self.reports.push(report);
                    }
                    self.advance();
                    break;
                }

                Some(TOKEN_KW_ON) => self.parse_endpoint_response(prologue),

                Some(TOKEN_KW_GET)
                | Some(TOKEN_KW_POST)
                | Some(TOKEN_KW_PUT)
                | Some(TOKEN_KW_DELETE)
                | Some(TOKEN_KW_PATCH) => self.parse_endpoint(prologue),

                Some(kind) => {
                    let span = self.parse_error(is_recovery);

                    self.reports.push(
                        Report::error(format!("expected endpoint or response, found {kind}"))
                            .with_label(Label::new(
                                "expected endpoint or response".to_string(),
                                span,
                            ))
                            .with_note(
                                "help: route can only contain enpdoints and responses".to_string(),
                            ),
                    );
                }
                None => {
                    self.reports.push(
                        Report::error(
                            "expected endpoint or resopnse, found end of file".to_string(),
                        )
                        .with_label(Label::new(
                            "expected endpoint or resopnse".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        ))
                        .with_note(
                            "help: route can only contain enpdoints and responses".to_string(),
                        ),
                    );
                    break;
                }
            }
        }
    }

    fn parse_endpoint_response(&mut self, prologue: Option<Prologue>) {
        self.start_node(
            NODE_ENDPOINT_RESPONSE,
            prologue,
            PrologueBehavior::NoDefault,
        );

        debug_assert!(self.peek() == Some(TOKEN_KW_ON));
        self.advance();

        self.skip_whitespace();

        // Parse response status
        self.builder
            .start_node(NODE_ENDPOINT_RESPONSE_STATUS.into());
        match self.peek() {
            Some(TOKEN_KW_DEFAULT) | Some(TOKEN_INTEGER) => self.advance(),

            Some(token) => {
                let span =
                    self.parse_error(|token| token == TOKEN_COLON || token == TOKEN_CURLY_RIGHT);

                self.reports.push(
                    Report::error(format!("expected response status, got {token}"))
                        .with_label(Label::new("expected response status".to_string(), span)),
                );
            }
            None => {
                self.reports.push(
                    Report::error("expected response status, got end of file".to_string())
                        .with_label(Label::new(
                            "expected response status".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        )),
                );
                self.builder.finish_node(); // Finish endpoint response status
                self.builder.finish_node(); // Finish endpoint response

                return;
            }
        }
        self.builder.finish_node();

        // Parse response type
        self.assignment();
        self.parse_type(DefaultCompositeType::None, |token| {
            // This method  should be called inside parse_endpoint_response, which ends
            // with `}` which we can use as recovery token
            token == TOKEN_CURLY_RIGHT
        });

        self.skip_whitespace();
        self.expect(TOKEN_EOL);

        self.builder.finish_node();
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;

    use crate::{parse, parser::endpoint::test, tokenize};

    #[test]
    fn parse_endpoint() {
        let text = indoc! {r#"
            /// Greet the caller
            GET "/hello" {
                name: "hello"

                query: {
                    /// Name that endpoint should greet
                    @default("World")
                    name: string 
                }

                /// Successfull greeting
                on 200: rec {
                    greeting: string
                }

                on default: ErrorResponse
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn parse_invalid_endpiont() {
        let text = indoc! {r#"
            /// Greet the caller
            @default
            GET {
                foobar: "Not a valid field"

                // This is missing a status
                on : string

                // This one is a value instead of a type
                // and ends abruptly.
                on default: "value instead of a type" }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        insta::assert_debug_snapshot!(diagnostics);
    }

    #[test]
    fn parse_route() {
        let text = indoc! {r#"
            /// This should be ignored
            route "/hello" {
                /// Greet the caller
                GET {
                    name: "hello"

                    /// Successfull greeting
                    on 200: rec {
                        greeting: string
                    }
                }

                /// Revoke the greeting
                DELETE {
                    name: "goodbye"
                }
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn parse_invalid_route() {
        let text = indoc! {r#"
            // Incorrect
            @default
            route { foo: "foo isn't valid!"

                DELETE {
                    name: "goodbye"
                }}
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        insta::assert_debug_snapshot!(diagnostics);
    }
}
