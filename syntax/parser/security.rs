use better_api_diagnostic::Label;
use better_api_diagnostic::Report;
use better_api_diagnostic::Span;

use super::Parser;
use super::prologue::Prologue;
use crate::Kind::*;
use crate::Token;
use crate::parser::basic::PrologueBehavior;

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    pub fn parse_security(&mut self, prologue: Option<Prologue>) {
        self.start_node(NODE_SECURITY, prologue, PrologueBehavior::NoDefault);

        debug_assert_eq!(self.peek(), Some(TOKEN_KW_SECURITY));
        self.advance();

        self.skip_whitespace();

        self.builder.start_node(NODE_NAME.into());
        self.expect(TOKEN_IDENTIFIER);
        self.builder.finish_node();

        self.assignment();

        self.expect(TOKEN_CURLY_LEFT);
        self.parse_security_properties();

        self.expect_line_end();

        self.builder.finish_node(); // NODE_SECURITY
    }

    fn parse_security_properties(&mut self) {
        let is_recovery = |token| token == TOKEN_CURLY_RIGHT;
        loop {
            let prologue = self.parse_prologue();

            match self.peek() {
                Some(TOKEN_CURLY_RIGHT) => {
                    if let Some(prologue) = prologue {
                        self.check_prologue_no_default(&prologue);
                    }
                    self.advance();
                    break;
                }

                Some(TOKEN_IDENTIFIER) => match self.peek_value() {
                    Some("type") => todo!(),         // String
                    Some("scheme") => todo!(),       // String
                    Some("header") => todo!(),       // String
                    Some("query") => todo!(),        // String
                    Some("unauthorized") => todo!(), // Ident
                    Some("forbidden") => todo!(),    // Ident

                    Some(field) => {
                        let report_msg = format!("invalid security field `{field}`");

                        let span = self.parse_error(is_recovery);
                        self.reports.push(
                            Report::error(report_msg).add_label(Label::primary(
                                "invalid security field".to_string(),
                                span,
                            )),
                        );
                    }
                    None => unreachable!(),
                },

                Some(kind) => {
                    let span = self.parse_error(is_recovery);
                    self.reports.push(
                        Report::error(format!("expected field name, found {kind}"))
                            .add_label(Label::primary("expected field name".to_string(), span)),
                    );
                }
                None => {
                    self.reports.push(
                        Report::error("expected field name, found end of file".to_string())
                            .add_label(Label::primary(
                                "expected field name".to_string(),
                                Span::new(self.pos, self.pos + 1),
                            )),
                    );
                }
            }
        }
    }
}
