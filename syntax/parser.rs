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

    /// Parses the tokens
    fn parse(&mut self) {
        self.builder.start_node(NODE_ROOT.into());

        while self.tokens.peek().is_some() {
            self.parse_root_node();
        }

        // Skip leading whitespace
        self.skip_whitespace_eol();

        self.builder.finish_node();
    }

    /// Parses a child of the file. This can be ie. path, name, endpoint, ...
    fn parse_root_node(&mut self) {
        self.skip_whitespace_eol();
        // TODO: Implement me
    }
}

#[cfg(test)]
mod test {
    use crate::tokenize;

    use super::parse;

    #[test]
    fn placeholder_test() {
        let text = "   \n\n  ";
        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }
}
