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

    /// Parses header trivia. This includes white space, comments,
    /// doc comments and modifiers (@default, ...).
    ///
    /// Returns starting checkpoint of the important trivia (doc comments and modifiers),
    /// if there is any.
    fn parse_header(&mut self) -> Option<rowan::Checkpoint> {
        // TODO: Implement me
        None
    }

    /// Parses a child of the file. This can be ie. path, name, endpoint, ...
    fn parse_root_node(&mut self) {
        let mut start = None;

        // Optional span of `@default`, used for reporting errors
        let mut default_span = None;

        loop {
            self.skip_whitespace_eol();

            match self.peek() {
                Some(TOKEN_COMMENT) => {
                    self.advance();
                    self.expect(TOKEN_EOL);
                }

                Some(TOKEN_DOC_COMMENT) => {
                    if start.is_none() {
                        start = Some(self.builder.checkpoint());
                    }

                    self.advance();
                    self.expect(TOKEN_EOL);
                }

                Some(TOKEN_KW_DEFAULT) => {
                    if start.is_none() {
                        start = Some(self.builder.checkpoint())
                    }

                    let span = Span::new(self.pos, self.peek_value().unwrap().len());

                    if default_span.is_none() {
                        default_span = Some(span);
                        self.advance();
                        self.expect(TOKEN_EOL);
                    } else {
                        self.reports.push(
                            Report::error("repeated `@default`".to_string())
                                .with_label(Label::new(
                                    "this `@default` is repeated".to_string(),
                                    span,
                                ))
                                .with_note(
                                    "help: only one `@default` per node is allowed".to_string(),
                                ),
                        );
                        self.builder.start_node(NODE_ERROR.into());
                        self.advance();
                        self.expect(TOKEN_EOL);
                        self.builder.finish_node();
                    }
                }

                Some(TOKEN_TOP_COMMENT) => {
                    self.advance();
                    break;
                }

                Some(TOKEN_IDENTIFIER) => {
                    match self.peek_value() {
                        Some("name") => self.parse_root_node_field(NODE_NAME),
                        Some("betterApi") => self.parse_root_node_field(NODE_BETTER_API),
                        Some("version") => self.parse_root_node_field(NODE_VERSION),
                        Some("server") => self.parse_root_node_field(NODE_SERVER),
                        Some("type") => todo!("Parse type"),
                        Some("example") => todo!("Parse example"),
                        Some("path") => todo!("Parse path"),
                        Some(_) => todo!("parse error node"),
                        None => unreachable!(),
                    };

                    break;
                }

                Some(TOKEN_KW_GET)
                | Some(TOKEN_KW_POST)
                | Some(TOKEN_KW_PUT)
                | Some(TOKEN_KW_DELETE)
                | Some(TOKEN_KW_PATCH) => todo!("parse endpoint"),

                Some(_) => todo!("parse error node"),

                // We reached end of the file
                None => return,
            }
        }

        self.expect(TOKEN_EOL);
    }

    /// Helper function for parsing some of the root nodes.
    fn parse_root_node_field(&mut self, kind: Kind) {
        self.builder.start_node(kind.into());

        self.advance();
        self.assignment();
        self.parse_value();

        self.builder.finish_node();
    }

    fn parse_value(&mut self) {
        self.builder.start_node(NODE_VALUE.into());

        match self.peek() {
            Some(TOKEN_STRING) => self.advance(),
            Some(kind) => {
                self.builder.start_node(NODE_ERROR.into());
                self.advance();
                self.builder.finish_node();

                todo!("emit report");
            }
            None => todo!("emit report"),
        }

        self.builder.finish_node();
    }
}

#[cfg(test)]
mod test {
    use crate::tokenize;

    use super::parse;

    #[test]
    fn placeholder_test() {
        let text = r#"   
        // A comment
        name :  "foobar""#;
        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }
}
