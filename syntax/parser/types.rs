use better_api_diagnostic::{Label, Report, Span};

use super::Parser;
use super::prologue::Prologue;
use crate::Kind::{self, *};
use crate::Token;
use crate::parser::basic::PrologueBehavior;

#[derive(Clone, Copy, Default)]
pub enum DefaultCompositeType {
    #[default]
    None,
    Record,
}

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    pub fn parse_type_def(&mut self, prologue: Option<Prologue>) {
        self.start_node(NODE_TYPE_DEF, prologue, PrologueBehavior::NoDefault);

        debug_assert_eq!(self.peek_value(), Some("type"));
        self.advance();

        self.skip_whitespace();

        self.builder.start_node(NODE_NAME.into());
        self.expect(TOKEN_IDENTIFIER);
        self.builder.finish_node();

        self.assignment();

        self.parse_type(DefaultCompositeType::None, |_| false);

        self.skip_whitespace();
        self.expect(TOKEN_EOL);

        self.builder.finish_node();
    }

    pub fn parse_type<F: Fn(Kind) -> bool>(
        &mut self,
        default_composite_type: DefaultCompositeType,
        is_recovery: F,
    ) {
        // Checkpoint for wrapping the type into OPTION
        let checkpoint = self.builder.checkpoint();

        match self.peek() {
            Some(TOKEN_IDENTIFIER) => self.with(NODE_TYPE_REF, Self::advance),
            Some(TOKEN_KW_I32) => self.with(NODE_TYPE_I32, Self::advance),
            Some(TOKEN_KW_I64) => self.with(NODE_TYPE_I64, Self::advance),
            Some(TOKEN_KW_U32) => self.with(NODE_TYPE_U32, Self::advance),
            Some(TOKEN_KW_U64) => self.with(NODE_TYPE_U64, Self::advance),
            Some(TOKEN_KW_F32) => self.with(NODE_TYPE_F32, Self::advance),
            Some(TOKEN_KW_F64) => self.with(NODE_TYPE_F64, Self::advance),
            Some(TOKEN_KW_DATE) => self.with(NODE_TYPE_DATE, Self::advance),
            Some(TOKEN_KW_TIMESTAMP) => self.with(NODE_TYPE_TIMESTAMP, Self::advance),
            Some(TOKEN_KW_BOOL) => self.with(NODE_TYPE_BOOL, Self::advance),
            Some(TOKEN_KW_STRING) => self.with(NODE_TYPE_STRING, Self::advance),
            Some(TOKEN_KW_FILE) => self.with(NODE_TYPE_FILE, Self::advance),

            Some(TOKEN_BRACKET_LEFT) => self.parse_array_type(),

            Some(TOKEN_KW_REC) => self.parse_record_type(),
            Some(TOKEN_KW_ENUM) => self.parse_enum_type(),
            Some(TOKEN_KW_UNION) => self.parse_union_type(),
            Some(TOKEN_KW_RESP) => self.parse_response_type(),

            Some(TOKEN_CURLY_LEFT) => {
                match default_composite_type {
                    DefaultCompositeType::None => {
                        // We don't use self.parse_error here, because we don't want to stop at EOL.
                        // We want to keep going until CURLY_RIGHT.
                        let start = self.pos;

                        self.builder.start_node(NODE_ERROR.into());
                        self.advance_while(|token| token != TOKEN_CURLY_RIGHT);
                        self.eat(TOKEN_CURLY_RIGHT);
                        self.builder.finish_node();

                        self.reports.push(
                            Report::error("ambiguous type".to_string())
                                .with_label(Label::new(
                                    "ambiguous type".to_string(),
                                    Span::new(start, self.pos),
                                ))
                                .with_note(
                                    "help: use `rec`, `enum`, `union` or `resp` keyword to specify the type"
                                        .to_string(),
                                ),
                        );
                    }
                    DefaultCompositeType::Record => self.parse_record_type(),
                }
            }

            Some(kind) => {
                let span = self.parse_error(&is_recovery);
                self.reports.push(
                    Report::error(format!("expected type, found {kind}"))
                        .with_label(Label::new("expected value".to_string(), span)),
                );
            }
            None => {
                self.reports.push(
                    Report::error("expected type, found end of file".to_string()).with_label(
                        Label::new(
                            "expected type".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        ),
                    ),
                );
            }
        }

        self.skip_whitespace();
        if self.peek() == Some(TOKEN_OPTION) {
            self.builder
                .start_node_at(checkpoint, NODE_TYPE_OPTION.into());
            self.advance();
            self.builder.finish_node();
        }
    }

    fn parse_array_type(&mut self) {
        debug_assert_eq!(self.peek(), Some(TOKEN_BRACKET_LEFT));

        self.builder.start_node(NODE_TYPE_ARRAY.into());

        self.advance();
        self.skip_whitespace();

        self.parse_type(DefaultCompositeType::None, |token| {
            token == TOKEN_BRACKET_RIGHT
        });

        self.skip_whitespace();
        self.expect(TOKEN_BRACKET_RIGHT);

        self.builder.finish_node();
    }

    fn parse_record_type(&mut self) {
        self.builder.start_node(NODE_TYPE_RECORD.into());

        self.eat(TOKEN_KW_REC);

        self.skip_whitespace();
        self.expect(TOKEN_CURLY_LEFT);

        self.parse_type_fields();

        self.builder.finish_node();
    }

    fn parse_enum_type(&mut self) {
        self.builder.start_node(NODE_TYPE_ENUM.into());

        self.eat(TOKEN_KW_ENUM);

        self.skip_whitespace();
        self.expect(TOKEN_PAREN_LEFT);
        self.skip_whitespace();
        self.parse_type(DefaultCompositeType::None, |token| {
            token == TOKEN_PAREN_RIGHT || token == TOKEN_CURLY_LEFT
        });
        self.skip_whitespace();
        self.expect(TOKEN_PAREN_RIGHT);
        self.skip_whitespace();
        self.expect(TOKEN_CURLY_LEFT);

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

                Some(_) => {
                    self.start_node(NODE_TYPE_ENUM_MEMBER, prologue, PrologueBehavior::NoDefault);

                    self.parse_value(|token| token == TOKEN_CURLY_RIGHT);
                    self.skip_whitespace();
                    self.expect(TOKEN_EOL);

                    self.builder.finish_node();
                }

                None => {
                    self.reports.push(
                        Report::error(
                            "expected enum member (value), found end of file".to_string(),
                        )
                        .with_label(Label::new(
                            "expected enum member".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        )),
                    );
                    break;
                }
            }
        }

        self.builder.finish_node();
    }

    fn parse_union_type(&mut self) {
        self.builder.start_node(NODE_TYPE_UNION.into());

        self.eat(TOKEN_KW_UNION);

        self.skip_whitespace();
        self.expect(TOKEN_PAREN_LEFT);
        self.skip_whitespace();
        self.parse_value(|token| token == TOKEN_PAREN_RIGHT || token == TOKEN_CURLY_LEFT);
        self.skip_whitespace();
        self.expect(TOKEN_PAREN_RIGHT);
        self.skip_whitespace();
        self.expect(TOKEN_CURLY_LEFT);

        self.parse_type_fields();

        self.builder.finish_node();
    }

    fn parse_response_type(&mut self) {
        let is_recovery = |token| token == TOKEN_CURLY_RIGHT;

        self.builder.start_node(NODE_TYPE_RESPONSE.into());

        self.eat(TOKEN_KW_RESP);

        self.skip_whitespace();
        self.expect(TOKEN_CURLY_LEFT);

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
                    Some("contentType") => {
                        self.parse_field(
                            NODE_TYPE_RESP_CONTENT_TYPE,
                            prologue,
                            PrologueBehavior::Ignore,
                            |p| p.parse_value(is_recovery),
                        );
                    }
                    Some("headers") => {
                        self.parse_field(
                            NODE_TYPE_RESP_HEADERS,
                            prologue,
                            PrologueBehavior::Ignore,
                            |p| p.parse_type(DefaultCompositeType::Record, is_recovery),
                        );
                    }
                    Some("body") => self.parse_field(
                        NODE_TYPE_RESP_BODY,
                        prologue,
                        PrologueBehavior::Ignore,
                        |p| p.parse_type(DefaultCompositeType::Record, is_recovery),
                    ),
                    Some(field) => {
                        let report_msg = format!("invalid response field `{field}`");

                        let span = self.parse_error(|token| token == TOKEN_CURLY_RIGHT);
                        self.reports.push(
                            Report::error(report_msg)
                                .with_label(Label::new("invalid response field".to_string(), span)).with_note("help: valid response fields are `body`, `contentType` and `headers`".to_string()),
                            );
                    }
                    None => unreachable!(),
                },

                Some(token) => {
                    let span = self.parse_error(|token| token == TOKEN_CURLY_RIGHT);
                    self.reports.push(
                        Report::error(format!("expected response field, got {token}"))
                            .with_label(Label::new("expected response field".to_string(), span)),
                    );
                }

                None => {
                    self.reports.push(
                        Report::error("expected `}`, found end of file".to_string()).with_label(
                            Label::new(
                                "expected `}`".to_string(),
                                Span::new(self.pos, self.pos + 1),
                            ),
                        ),
                    );
                    break;
                }
            }
        }

        self.builder.finish_node();
    }

    fn parse_type_fields(&mut self) {
        loop {
            let prologue = self.parse_prologue();

            match self.peek() {
                Some(TOKEN_IDENTIFIER) | Some(TOKEN_STRING) => {
                    self.start_node(NODE_TYPE_FIELD, prologue, PrologueBehavior::Full);

                    self.builder.start_node(NODE_NAME.into());
                    self.advance();
                    self.builder.finish_node();

                    self.assignment();

                    self.parse_type(DefaultCompositeType::None, |token| {
                        token == TOKEN_CURLY_RIGHT
                    });
                    self.skip_whitespace();
                    self.expect(TOKEN_EOL);

                    self.builder.finish_node();
                }

                Some(TOKEN_CURLY_RIGHT) => {
                    if let Some(prologue) = prologue
                        && let Some(report) = prologue.expect_no_default()
                    {
                        self.reports.push(report);
                    }

                    self.advance();
                    break;
                }

                Some(kind) => {
                    let span = self.parse_error(|_| false);

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
}

#[cfg(test)]
mod test {
    use indoc::indoc;

    use crate::{parse, tokenize};

    #[test]
    fn parse_simple_type_def() {
        let text = indoc! {r#"
            /// Doc comment 
            type Foo: Bar
            
            type Str: string
            type I32: i32
            type I64: i64
            type U32: u32
            type U64: u64
            type F32: f32
            type F64: f64
            type Date: date
            type TimeStamp: timestamp
            type Boolean: bool
            type File: file
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        assert_eq!(res.reports, vec![]);
    }

    #[test]
    fn parse_option_type() {
        let text = indoc! {r#"
            type Foo: i32?
            type Foo: string   ?
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        assert_eq!(res.reports, vec![]);
    }

    #[test]
    fn parse_array_type() {
        let text = indoc! {r#"
            type Foo: [i32]
            type Foo: [ string ?]
            type Foo: [string?]?
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        assert_eq!(res.reports, vec![]);
    }

    #[test]
    fn parse_record_type() {
        let text = indoc! {r#"
            type Foo: rec {
                // comment
                /// doc comment
                foo: bool

                /// More doc comment
                @default("foobar")
                bar: string

                // Just a comment
                hey: i32

                hoy: rec {
                    @default(true)
                    nested: bool
                }
            }

            // Invalid record
            type Bar: rec{
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        insta::assert_debug_snapshot!(res.reports);
    }

    #[test]
    fn parse_enum_type() {
        let text = indoc! {r#"
            type Foo: enum(string) {
                "foo"

                /// Documentation for enum member
                @default("this is an error")
                "bar"
            }

            // Very invalid enum
            type Bar: enum string {"foo" "bar"}
            // Should parse correctly!
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        insta::assert_debug_snapshot!(res.reports);
    }

    #[test]
    fn ambiguous_type() {
        let text = indoc! {r#"
            type Foo: {
                bar: string
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        insta::assert_debug_snapshot!(res.reports);
    }

    #[test]
    fn parse_union_type() {
        let text = indoc! {r#"
            type Foo: union("type") {
                bar: Bar
                "baz str": Baz
            }

            // Very invalid union
            type Bar: union "type" {bar: Bar}
            // Should parse correctly!
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        insta::assert_debug_snapshot!(res.reports);
    }

    #[test]
    fn parse_response() {
        let text = indoc! {r#"
            /// This is a valid response
            type Foo: resp {
                contentType: "application/json"
                body: FooBody
            }

            // A bit invalid response
            type Bar: resp {
                /// This doc comment should be ignored
                body: {
                    @default(true)
                    shouldWork: bool
                }

                invalidField: "hello!"
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        insta::assert_debug_snapshot!(res.reports);
    }
}
