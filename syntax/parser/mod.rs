use better_api_diagnostic::{Label, Report};

use crate::Kind::{self, *};
use crate::{Token, node};

use prologue::Prologue;

mod basic;
mod prologue;
mod types;
mod values;

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

    /// Parses the tokens
    fn parse(&mut self) {
        self.builder.start_node(NODE_ROOT.into());

        while self.peek().is_some() {
            self.parse_root_node();
        }

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
                    Some("example") => todo!("Parse example"),
                    Some("path") => todo!("Parse path"),

                    Some(field) => {
                        let error_msg = format!("invalid property `{field}`");
                        let span = self.parse_error(|_| false);
                        self.reports.push(
                            Report::error(error_msg)
                                .with_label(Label::new("invalid property".to_string(), span)),
                        );
                    }

                    // Unreachable because `self.peek()` is token identifier.
                    None => unreachable!(),
                };
            }

            Some(TOKEN_KW_TYPE) => self.parse_type_def(prologue),

            Some(TOKEN_KW_GET)
            | Some(TOKEN_KW_POST)
            | Some(TOKEN_KW_PUT)
            | Some(TOKEN_KW_DELETE)
            | Some(TOKEN_KW_PATCH) => todo!("parse endpoint"),

            Some(token) => {
                let span = self.parse_error(|_| false);
                self.reports.push(
                    Report::error(format!("unexpected token {token}"))
                        .with_label(Label::new("unexpected token".to_string(), span)),
                );
            }

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
        self.parse_value(|_| false);
        self.expect(TOKEN_EOL);

        self.builder.finish_node();
    }
}

// TODO: Test some errors
// - a field that is not valid
// - a token that is not valid
