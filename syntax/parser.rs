use better_api_diagnostic::{Label, Report, Span};

use crate::Kind::{self, *};
use crate::{Token, node};

pub fn parse<'a, T: Iterator<Item = Token<'a>>>(tokens: T) -> (node::SyntaxNode, Vec<Report>) {
    let mut parser = Parser::new(tokens);
    parser.parse();

    let node = node::SyntaxNode::new_root(parser.builder.finish());
    (node, parser.reports)
}

struct Parser<'a, T: Iterator<Item = Token<'a>>> {
    builder: rowan::GreenNodeBuilder<'a>,
    tokens: std::iter::Peekable<T>,

    /// Position in the file, measured in bytes (utf8) offset.
    pos: usize,

    reports: Vec<Report>,
}

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    fn new(tokens: T) -> Self {
        Self {
            builder: rowan::GreenNodeBuilder::new(),
            tokens: tokens.peekable(),
            pos: 0,
            reports: vec![],
        }
    }

    /// Advances to the next token.
    /// Panics if there are not tokens left.
    /// Caller must check that there is at least one token.
    fn advance(&mut self) {
        let (kind, value) = self.tokens.next().unwrap();
        self.pos += value.len();
        self.builder.token(kind.into(), value);
    }

    /// Returns the token kind of the next token.
    fn peek(&mut self) -> Option<Kind> {
        self.tokens.peek().map(|(kind, _)| *kind)
    }

    fn peek_value(&mut self) -> Option<&str> {
        self.tokens.peek().map(|(_, value)| *value)
    }

    /// Skips whitespace that is not EOL.
    /// If you want to skip EOL as well, use
    /// skip_whitespace_eol.
    fn skip_whitespace(&mut self) {
        while self.peek() == Some(TOKEN_SPACE) {
            self.advance();
        }
    }

    /// Skips whitespace and EOL.
    /// If you don't want to skip EOL as well, use
    /// skip_whitespace.
    fn skip_whitespace_eol(&mut self) {
        while matches!(self.peek(), Some(TOKEN_SPACE) | Some(TOKEN_EOL)) {
            self.advance();
        }
    }

    /// If token is of kind, we advance it, otherwise we ignore it
    /// without any errors. Acts as "optional expect"
    fn eat(&mut self, kind: Kind) -> bool {
        if self.peek() == Some(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Expects that next token is of certain kind and advances the tokens.
    /// Otherwise reports an error and doesn't advance the tokens.
    fn expect(&mut self, kind: Kind) -> bool {
        if self.peek() == Some(kind) {
            self.advance();
            true
        } else {
            let report = Report::error(format!(
                "expected {kind}, found {0}",
                self.peek()
                    .map_or_else(|| "end of file".to_string(), |k| k.to_string())
            ))
            .with_label(Label::new(
                "unexpected token".to_string(),
                Span::new(
                    self.pos,
                    self.pos + self.peek_value().map_or(1, |s| s.len()),
                ),
            ));
            self.reports.push(report);

            false
        }
    }

    /// Parses assignment trivia "<whitespace> : <whitespace>"
    fn assignment(&mut self) {
        self.skip_whitespace();
        self.expect(TOKEN_COLON);
        self.skip_whitespace();
    }

    /// Parses the tokens
    fn parse(&mut self) {
        self.builder.start_node(NODE_ROOT.into());

        while self.peek().is_some() {
            self.parse_root_node();
        }

        self.builder.finish_node();
    }

    /// Parses prologue trivia. This includes white space, comments,
    /// doc comments and modifiers (@default, ...).
    fn parse_prologue(&mut self) -> Option<Prologue> {
        let mut res = None;

        loop {
            self.skip_whitespace_eol();
            match self.peek() {
                Some(TOKEN_COMMENT) => {
                    self.advance();
                    self.expect(TOKEN_EOL);
                }

                Some(TOKEN_DOC_COMMENT) => {
                    if res.is_none() {
                        res = Some(Prologue {
                            start: self.builder.checkpoint(),
                            default: None,
                        });
                    }

                    self.advance();
                    self.expect(TOKEN_EOL);
                }

                Some(TOKEN_KW_DEFAULT) => {
                    if res.is_none() {
                        res = Some(Prologue {
                            start: self.builder.checkpoint(),
                            default: None,
                        });
                    }

                    let start_pos = self.pos;

                    let is_duplicated = res.as_ref().unwrap().default.is_some();
                    if is_duplicated {
                        self.builder.start_node(NODE_ERROR.into());
                    }

                    self.parse_default();

                    if is_duplicated {
                        // Finish error node
                        self.builder.finish_node();

                        // Report error
                        self.reports.push(
                            Report::error("duplicated `@default` is not allowed".to_string())
                                .with_label(Label::new(
                                    "duplicated `@default`".to_string(),
                                    Span::new(start_pos, self.pos),
                                )),
                        );
                    } else {
                        res.as_mut().unwrap().default = Some(Span::new(start_pos, self.pos));
                    }
                }

                _ => break,
            }
        }

        res
    }

    /// Parses `@default` node
    fn parse_default(&mut self) {
        debug_assert_eq!(self.peek(), Some(TOKEN_KW_DEFAULT));
        self.builder.start_node(NODE_DEFAULT.into());

        self.advance();

        if self.eat(TOKEN_PAREN_LEFT) {
            self.parse_value();
            self.expect(TOKEN_PAREN_RIGHT);
        }

        self.expect(TOKEN_EOL);

        self.builder.finish_node();
    }

    /// Parses a child of the file. This can be ie. path, name, endpoint, ...
    fn parse_root_node(&mut self) {
        let prologue = self.parse_prologue();

        match self.peek() {
            Some(TOKEN_TOP_COMMENT) => {
                if let Some(prologue) = prologue
                    && let Some(report) = prologue.expect_no_default()
                {
                    self.reports.push(report);
                }

                self.advance();
                self.expect(TOKEN_EOL);
            }

            Some(TOKEN_IDENTIFIER) => {
                match self.peek_value() {
                    Some("name") => self.parse_root_node_field(NODE_API_NAME, prologue, false),
                    Some("betterApi") => {
                        self.parse_root_node_field(NODE_BETTER_API, prologue, false)
                    }
                    Some("version") => self.parse_root_node_field(NODE_VERSION, prologue, false),
                    Some("server") => self.parse_root_node_field(NODE_SERVER, prologue, true),
                    Some("type") => self.parse_type_def(prologue),
                    Some("example") => todo!("Parse example"),
                    Some("path") => todo!("Parse path"),
                    Some(_) => todo!("parse error node"),

                    // Unreachable because `self.peek()` is token identifier.
                    None => unreachable!(),
                };
            }

            Some(TOKEN_KW_GET)
            | Some(TOKEN_KW_POST)
            | Some(TOKEN_KW_PUT)
            | Some(TOKEN_KW_DELETE)
            | Some(TOKEN_KW_PATCH) => todo!("parse endpoint"),

            Some(_) => todo!("parse error node"),

            // We reached end of the file
            None => (),
        }
    }

    /// Helper function for parsing some of the root nodes.
    fn parse_root_node_field(
        &mut self,
        kind: Kind,
        prologue: Option<Prologue>,
        use_prologue: bool,
    ) {
        if let Some(prologue) = prologue {
            if let Some(report) = prologue.expect_no_default() {
                self.reports.push(report);
            }

            self.builder.start_node_at(prologue.start, kind.into());

            if use_prologue {
                self.builder
                    .start_node_at(prologue.start, NODE_PROLOGUE.into());
                self.builder.finish_node();
            }
        } else {
            self.builder.start_node(kind.into());
        }

        self.advance();
        self.assignment();
        self.parse_value();
        self.expect(TOKEN_EOL);

        self.builder.finish_node();
    }

    fn parse_type_def(&mut self, prologue: Option<Prologue>) {
        if let Some(prologue) = prologue {
            if let Some(report) = prologue.expect_no_default() {
                self.reports.push(report);
            }

            self.builder
                .start_node_at(prologue.start, NODE_TYPE_DEF.into());

            self.builder
                .start_node_at(prologue.start, NODE_PROLOGUE.into());
            self.builder.finish_node();
        } else {
            self.builder.start_node(NODE_TYPE_DEF.into());
        }

        debug_assert_eq!(self.peek_value(), Some("type"));
        self.advance();

        self.skip_whitespace();

        self.builder.start_node(NODE_NAME.into());
        self.expect(TOKEN_IDENTIFIER);
        self.builder.finish_node();

        self.assignment();

        self.parse_type(DefaultCompositeType::None);

        self.skip_whitespace();
        self.expect(TOKEN_EOL);

        self.builder.finish_node();
    }

    fn parse_type(&mut self, default_composite_type: DefaultCompositeType) {
        self.builder.start_node(NODE_TYPE.into());

        match self.peek() {
            Some(TOKEN_IDENTIFIER) => self.advance(),
            Some(TOKEN_KW_I32)
            | Some(TOKEN_KW_I64)
            | Some(TOKEN_KW_U32)
            | Some(TOKEN_KW_U64)
            | Some(TOKEN_KW_F32)
            | Some(TOKEN_KW_F64)
            | Some(TOKEN_KW_DATE)
            | Some(TOKEN_KW_TIMESTAMP)
            | Some(TOKEN_KW_BOOL)
            | Some(TOKEN_KW_STRING)
            | Some(TOKEN_KW_FILE) => self.advance(),

            Some(TOKEN_BRACKET_LEFT) => todo!("parse array"),

            Some(TOKEN_CURLY_LEFT) => match default_composite_type {
                DefaultCompositeType::None => todo!("handle ambigous error"),
                DefaultCompositeType::Record => todo!("parse record"),
                DefaultCompositeType::Enum => todo!("parse enum"),
                DefaultCompositeType::Union => todo!("parse union"),
                DefaultCompositeType::Response => todo!("parse response"),
            },

            Some(_) => todo!("handle unexpected token error"),
            None => todo!("handle unepxected eof error"),
        }

        self.builder.finish_node();
    }

    fn parse_value(&mut self) {
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
                let start = self.pos;
                self.builder.start_node(NODE_ERROR.into());
                self.advance();
                self.builder.finish_node();

                self.reports.push(
                    Report::error(format!("expected value, found {kind}")).with_label(Label::new(
                        "expected value".to_string(),
                        Span::new(start, self.pos),
                    )),
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
                    self.expect(TOKEN_EOL);

                    self.builder.finish_node();
                }

                Some(TOKEN_CURLY_RIGHT) => {
                    self.advance();
                    break;
                }

                Some(kind) => {
                    let start = self.pos;

                    self.builder.start_node(NODE_ERROR.into());
                    self.advance();
                    self.builder.finish_node();

                    self.reports.push(
                        Report::error(format!("expected field name, found {kind}")).with_label(
                            Label::new(
                                "expected field name".to_string(),
                                Span::new(start, self.pos),
                            ),
                        ),
                    );
                }
                None => self.reports.push(
                    Report::error("expected field name, found end of file".to_string()).with_label(
                        Label::new(
                            "expected field value".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        ),
                    ),
                ),
            }
        }

        self.builder.finish_node();
    }
}

struct Prologue {
    start: rowan::Checkpoint,
    default: Option<Span>,
}

impl Prologue {
    /// Checks that there is no `@default` in the parsed prologue.
    /// If there is a `@default`, a diagnostic for it is emitted.
    fn expect_no_default(&self) -> Option<Report> {
        let default_span = self.default?;

        Some(
            Report::error("unexpected `@default`".to_string()).with_label(Label::new(
                "unexpected `@default`".to_string(),
                default_span,
            )),
        )
    }
}

#[derive(Clone, Copy, Default)]
enum DefaultCompositeType {
    #[default]
    None,
    Record,
    Enum,
    Union,
    Response,
}

#[cfg(test)]
mod test {
    use better_api_diagnostic::{Label, Report, Span};
    use indoc::indoc;

    use super::parse;
    use crate::tokenize;

    #[test]
    fn parse_basic_info() {
        let text = indoc! {r#"
            //! This is a top comment
            // This is a normal comment
            //! Here is another top comment
            
            /// A doc comment
            name:  "foobar"

            // This should report an error
            @default("asdf")
            betterApi: "1.0.0"

            version: "1.0"
        "#};
        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(
            diagnostics,
            vec![
                Report::error("unexpected `@default`".to_string()).with_label(Label::new(
                    "unexpected `@default`".to_string(),
                    Span::new(153, 170)
                ))
            ]
        );
    }

    #[test]
    fn parse_server() {
        let text = indoc! {r#"
            /// doc comment
            server: {
                name: "foo"
                url: "bar"
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn parse_server_error() {
        let text = indoc! {r#"
            /// doc comment
            server: {
                name: foobar
                123: "bar"
                url: "baz"
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        insta::assert_debug_snapshot!(diagnostics);
    }

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

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }

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

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }
}
