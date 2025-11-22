//! Implementation of basic parse blocks.

use better_api_diagnostic::{Label, Report, Span};

use super::Parser;
use crate::Kind::{self, *};
use crate::Token;
use crate::parser::prologue::Prologue;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum PrologueBehavior {
    /// Ignore the prologue.
    Ignore,

    /// Only parse doc comments and raise an error if there
    /// is `@default` in the prologue
    NoDefault,

    /// Parse the full prologue
    Full,
}

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    /// Advances to the next token.
    /// Panics if there are not tokens left.
    /// Caller must check that there is at least one token.
    pub fn advance(&mut self) {
        let (kind, value) = self.tokens.next().unwrap();
        self.pos += value.len();
        self.builder.token(kind.into(), value);
    }

    // Advances tokens while `check` is returning true.
    pub fn advance_while<F: Fn(Kind) -> bool>(&mut self, check: F) {
        loop {
            if self.peek().is_some_and(&check) {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Returns the token kind of the next token.
    pub fn peek(&mut self) -> Option<Kind> {
        self.tokens.peek().map(|(kind, _)| *kind)
    }

    pub fn peek_value(&mut self) -> Option<&str> {
        self.tokens.peek().map(|(_, value)| *value)
    }

    /// Skips whitespace that is not EOL.
    /// If you want to skip EOL as well, use
    /// skip_whitespace_eol.
    pub fn skip_whitespace(&mut self) {
        while self.peek() == Some(TOKEN_SPACE) {
            self.advance();
        }
    }

    /// Skips whitespace and EOL.
    /// If you don't want to skip EOL as well, use
    /// skip_whitespace.
    pub fn skip_whitespace_eol(&mut self) {
        while matches!(self.peek(), Some(TOKEN_SPACE) | Some(TOKEN_EOL)) {
            self.advance();
        }
    }

    /// If token is of kind, we advance it, otherwise we ignore it
    /// without any errors. Acts as "optional expect"
    pub fn eat(&mut self, kind: Kind) -> bool {
        if self.peek() == Some(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Expects that next token is of certain kind and advances the tokens.
    /// Otherwise reports an error and doesn't advance the tokens.
    pub fn expect(&mut self, kind: Kind) -> bool {
        if self.peek() == Some(kind) {
            self.advance();
            true
        } else {
            let report = Report::error(format!(
                "expected {kind}, found {0}",
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
            self.reports.push(report);

            false
        }
    }

    /// Parses error node. It advances the tokens until EOF is reached,
    /// or until recovery token is found `is_recovery(token) == true`.
    pub fn parse_error<F: Fn(Kind) -> bool>(&mut self, is_recovery: F) -> Span {
        let start = self.pos;
        let end = self.pos + self.peek_value().map_or(1, |val| val.len());

        self.builder.start_node(NODE_ERROR.into());
        self.advance_while(|token| token != TOKEN_EOL && !is_recovery(token));
        self.builder.finish_node();

        Span::new(start, end)
    }

    /// Parses assignment trivia "<whitespace> : <whitespace>"
    pub fn assignment(&mut self) {
        self.skip_whitespace();
        self.expect(TOKEN_COLON);
        self.skip_whitespace();
    }

    /// Starts a new node with given prologue and it's behavior.
    pub fn start_node(
        &mut self,
        kind: Kind,
        prologue: Option<Prologue>,
        prologue_behavior: PrologueBehavior,
    ) {
        match (prologue, prologue_behavior) {
            (None, _) | (_, PrologueBehavior::Ignore) => self.builder.start_node(kind.into()),
            (Some(p), _) => {
                // If only doc comment should be parsed,
                // check there is no `@default`.
                if prologue_behavior == PrologueBehavior::NoDefault
                    && let Some(report) = p.expect_no_default()
                {
                    self.reports.push(report);
                }

                self.builder.start_node_at(p.start, kind.into());

                self.builder.start_node_at(p.start, NODE_PROLOGUE.into());
                self.builder.finish_node();
            }
        }
    }

    pub fn parse_field<F: Fn(&mut Self)>(
        &mut self,
        kind: Kind,
        prologue: Option<Prologue>,
        prologue_behavior: PrologueBehavior,
        inner: F,
    ) {
        self.start_node(kind, prologue, prologue_behavior);

        self.advance();
        self.assignment();

        inner(self);

        self.skip_whitespace();
        self.expect(TOKEN_EOL);

        self.builder.finish_node();
    }

    pub fn with<F: Fn(&mut Self)>(&mut self, kind: Kind, inner: F) {
        self.builder.start_node(kind.into());
        inner(self);
        self.builder.finish_node();
    }
}

#[cfg(test)]
mod test {
    use better_api_diagnostic::{Label, Report, Span};
    use indoc::indoc;

    use crate::{parse, tokenize};

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

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        assert_eq!(
            res.reports,
            vec![
                Report::error("unexpected `@default`".to_string()).add_label(Label::primary(
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

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        assert_eq!(res.reports, vec![]);
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

            server: {
                field: "unfinished"
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        insta::assert_debug_snapshot!(res.reports);
    }
}
