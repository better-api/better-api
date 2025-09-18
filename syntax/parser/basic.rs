//! Implementation of basic parse blocks.

use better_api_diagnostic::{Label, Report, Span};

use super::Parser;
use crate::Kind::{self, *};
use crate::Token;

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
}
