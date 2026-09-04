use better_api_diagnostic::Label;
use better_api_diagnostic::Report;
use better_api_diagnostic::Span;

use super::Parser;
use super::prologue::Prologue;
use crate::Kind;
use crate::Kind::*;
use crate::Token;
use crate::parser::basic::PrologueBehavior;

enum SecurityFieldType {
    String,
    TypeRef,
}

impl SecurityFieldType {
    fn token(&self) -> Kind {
        match self {
            Self::String => TOKEN_STRING,
            Self::TypeRef => TOKEN_IDENTIFIER,
        }
    }

    fn node(&self) -> Kind {
        match self {
            Self::String => NODE_VALUE_STRING,
            Self::TypeRef => NODE_TYPE_REF,
        }
    }
}

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

        if self.expect(TOKEN_CURLY_LEFT) {
            self.parse_security_properties();
        }

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
                    Some("kind") => self.parse_security_field(
                        SecurityFieldType::String,
                        NODE_SECURITY_KIND,
                        prologue,
                    ),
                    Some("scheme") => self.parse_security_field(
                        SecurityFieldType::String,
                        NODE_SECURITY_SCHEME,
                        prologue,
                    ),
                    Some("header") => self.parse_security_field(
                        SecurityFieldType::String,
                        NODE_SECURITY_HEADER,
                        prologue,
                    ),
                    Some("query") => self.parse_security_field(
                        SecurityFieldType::String,
                        NODE_SECURITY_QUERY,
                        prologue,
                    ),

                    Some("unauthorized") => self.parse_security_field(
                        SecurityFieldType::TypeRef,
                        NODE_SECURITY_UNAUTHORIZED,
                        prologue,
                    ),
                    Some("forbidden") => self.parse_security_field(
                        SecurityFieldType::TypeRef,
                        NODE_SECURITY_FORBIDDEN,
                        prologue,
                    ),

                    Some(field) => {
                        let report_msg = format!("invalid security field `{field}`");

                        if let Some(p) = prologue {
                            self.check_prologue_no_doc_comments(&p);
                            self.check_prologue_no_default(&p);
                        }

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

                    if let Some(p) = prologue {
                        self.check_prologue_no_doc_comments(&p);
                        self.check_prologue_no_default(&p);
                    }

                    self.reports.push(
                        Report::error(format!("expected field name, found {kind}"))
                            .add_label(Label::primary("expected field name".to_string(), span)),
                    );
                }
                None => {
                    if let Some(p) = prologue {
                        self.check_prologue_no_doc_comments(&p);
                        self.check_prologue_no_default(&p);
                    }

                    self.reports.push(
                        Report::error("expected field name, found end of file".to_string())
                            .add_label(Label::primary(
                                "expected field name".to_string(),
                                Span::new(self.pos, self.pos + 1),
                            )),
                    );
                    break;
                }
            }
        }
    }

    fn parse_security_field(
        &mut self,
        typ: SecurityFieldType,
        node: Kind,
        prologue: Option<Prologue>,
    ) {
        self.parse_field(node, prologue, PrologueBehavior::Ignore, |p| {
            let token = typ.token();
            match p.peek() {
                Some(kind) if kind == typ.token() => {
                    p.builder.start_node(typ.node().into());
                    p.advance();
                    p.builder.finish_node();
                }

                Some(kind) => {
                    let span = p.parse_error(|t| t == TOKEN_CURLY_RIGHT);
                    p.reports.push(
                        Report::error(format!("expected {token}, found {kind}"))
                            .add_label(Label::primary(format!("expected {token}"), span)),
                    );
                }
                None => {
                    let span = p.peek_span();
                    p.reports.push(
                        Report::error(format!("expected {token}, found end of file"))
                            .add_label(Label::primary(format!("expected {token}"), span)),
                    );
                }
            }
        });
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;

    use crate::{parse, tokenize};

    #[test]
    fn parse_security() {
        let text = indoc! {r#"
            /// Bearer token authentication
            security HttpBearer: {
                kind: "http"
                scheme: "bearer"
                unauthorized: UnauthorizedError
            }

            security ApiKey: {
                kind: "api_key"
                header: "X-API-KEY"
                query: "api_key"
                unauthorized: UnauthorizedError
                forbidden: ForbiddenError
            }

            security Empty: {}
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        assert_eq!(res.reports, vec![]);
    }

    #[test]
    fn parse_invalid_security_fields() {
        let text = indoc! {r#"
            security Broken: {
                /// Field documentation is ignored
                @default("http")
                kind: Http

                scheme: Bearer
                unauthorized: "UnauthorizedError"
                forbidden: 403

                invalidField: "value"
                42
            }

            security ValidAfterErrors: {
                kind: "http"
                scheme: "basic"
                unauthorized: UnauthorizedError
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        insta::assert_debug_snapshot!(res.reports);
    }

    #[test]
    fn parse_invalid_security_declarations() {
        let text = indoc! {r#"
            @default
            security : {}

            security MissingColon {}
            security MissingBody:

            security Unclosed: {
                kind: "http"
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let res = parse(tokens);
        insta::assert_debug_snapshot!(res.node);
        insta::assert_debug_snapshot!(res.reports);
    }
}
