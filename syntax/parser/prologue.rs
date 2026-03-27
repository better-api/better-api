use better_api_diagnostic::{Label, Report, Span};
use smallvec::SmallVec;

use super::Parser;
use crate::Kind::*;
use crate::Token;

pub struct Prologue {
    pub start: rowan::Checkpoint,

    /// Span of the first `@default`
    pub default: Option<Span>,

    /// Spans of doc comments.
    pub doc_spans: SmallVec<[Span; 2]>,
}

impl Prologue {
    /// Checks that there is no `@default` in the parsed prologue.
    /// If there is a `@default`, a diagnostic for it is emitted.
    pub fn expect_no_default(&self) -> Option<Report> {
        let default_span = self.default?;

        Some(
            Report::error("unexpected `@default`".to_string()).add_label(Label::primary(
                "unexpected `@default`".to_string(),
                default_span,
            )),
        )
    }
}

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    /// Parses prologue trivia. This includes white space, comments,
    /// doc comments and modifiers (@default, ...).
    pub fn parse_prologue(&mut self) -> Option<Prologue> {
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
                            doc_spans: SmallVec::new(),
                        });
                    }

                    res.as_mut().unwrap().doc_spans.push(self.peek_span());

                    self.advance();
                    self.expect(TOKEN_EOL);
                }

                Some(TOKEN_DECORATOR_DEFAULT) => {
                    if res.is_none() {
                        res = Some(Prologue {
                            start: self.builder.checkpoint(),
                            default: None,
                            doc_spans: SmallVec::new(),
                        });
                    }

                    let start_pos = self.pos;

                    let is_duplicated = res.as_ref().unwrap().default.is_some();
                    if is_duplicated {
                        self.builder.start_node(NODE_ERROR.into());
                    }

                    let doc_span = self.parse_default();
                    if let Some(span) = doc_span {
                        res.as_mut().unwrap().doc_spans.push(span);
                    }

                    if is_duplicated {
                        // Finish error node
                        self.builder.finish_node();

                        // Report error
                        self.reports.push(
                            Report::error("duplicated `@default` is not allowed".to_string())
                                .add_label(Label::primary(
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
    ///
    /// Returns span of inline doc comment, if there is any.
    fn parse_default(&mut self) -> Option<Span> {
        debug_assert_eq!(self.peek(), Some(TOKEN_DECORATOR_DEFAULT));
        self.builder.start_node(NODE_DEFAULT.into());

        self.advance();

        if self.eat(TOKEN_PAREN_LEFT) {
            self.parse_value(|token| token == TOKEN_PAREN_RIGHT);
            self.expect(TOKEN_PAREN_RIGHT);
        }

        // We don't use expect_line_end here, because we don't want to emit
        // a warning for doc comment. Default is part of the prologue, so
        // doc comment in here is valid prologue.
        self.skip_whitespace();
        self.eat(TOKEN_COMMENT);

        let mut span = None;
        if self.peek() == Some(TOKEN_DOC_COMMENT) {
            span = Some(self.peek_span());
            self.advance();
        }

        self.expect(TOKEN_EOL);

        self.builder.finish_node();

        span
    }
}
