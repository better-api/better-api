use better_api_diagnostic::Report;

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

    /// Expects that next token is of certain kind and advances the tokens.
    /// Otherwise reports an error and doesn't advance the tokens.
    fn expect(&mut self, kind: Kind) -> bool {
        if self.peek() == Some(kind) {
            self.advance();
            true
        } else {
            // TODO: report diagnostic
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

    /// Parses a child of the file. This can be ie. path, name, endpoint, ...
    fn parse_root_node(&mut self) {
        self.skip_whitespace_eol();

        match self.peek() {
            Some(TOKEN_IDENTIFIER) => match self.peek_value() {
                Some("name") => {
                    self.builder.start_node(NODE_NAME.into());

                    self.advance();
                    self.assignment();

                    // TODO: Parse value

                    self.builder.finish_node();
                }
                Some("betterApi") => todo!("Parse betterApi version"),
                Some("version") => todo!("Parse version string"),
                Some("server") => todo!("Parse server object"),
                Some("type") => todo!("Parse type"),
                Some("example") => todo!("Parse example"),
                Some("path") => todo!("Parse path"),
                Some(_) => todo!("parse error node"),
                None => unreachable!(),
            },

            Some(TOKEN_KW_GET)
            | Some(TOKEN_KW_POST)
            | Some(TOKEN_KW_PUT)
            | Some(TOKEN_KW_DELETE)
            | Some(TOKEN_KW_PATCH) => todo!("parse endpoint"),

            Some(TOKEN_COMMENT) => todo!("parse comment"),
            Some(TOKEN_DOC_COMMENT) => todo!("parse doc comment"),
            Some(TOKEN_TOP_COMMENT) => todo!("parse top comment"),

            Some(_) => todo!("parse error node"),

            // We reached end of the file
            None => return,
        }

        self.expect(TOKEN_EOL);
    }
}

#[cfg(test)]
mod test {
    use crate::tokenize;

    use super::parse;

    #[test]
    fn placeholder_test() {
        let text = "   \n\nname :  ";
        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }
}
