use better_api_diagnostic::{Label, Report, Span};

use super::Parser;
use super::prologue::Prologue;
use crate::Kind::*;
use crate::Token;

#[derive(Clone, Copy, Default)]
pub enum DefaultCompositeType {
    #[default]
    None,
    Record,
    Enum,
    Union,
    Response,
}

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    pub fn parse_type_def(&mut self, prologue: Option<Prologue>) {
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

    pub fn parse_type(&mut self, default_composite_type: DefaultCompositeType) {
        self.builder.start_node(NODE_TYPE.into());

        let checkpoint = self.builder.checkpoint();

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

            Some(TOKEN_BRACKET_LEFT) => {
                self.builder.start_node(NODE_TYPE_ARRAY.into());

                self.advance();
                self.skip_whitespace();

                self.parse_type(DefaultCompositeType::None);

                self.skip_whitespace();
                self.expect(TOKEN_BRACKET_RIGHT);

                self.builder.finish_node();
            }

            Some(TOKEN_KW_REC) => self.parse_record_type(),
            Some(TOKEN_KW_ENUM) => todo!("parse enum type"),
            Some(TOKEN_KW_UNION) => todo!("parse union type"),
            Some(TOKEN_KW_RESP) => todo!("parse response type"),

            Some(TOKEN_CURLY_LEFT) => match default_composite_type {
                DefaultCompositeType::None => todo!("handle ambigous error"),
                DefaultCompositeType::Record => self.parse_record_type(),
                DefaultCompositeType::Enum => todo!("parse enum"),
                DefaultCompositeType::Union => todo!("parse union"),
                DefaultCompositeType::Response => todo!("parse response"),
            },

            Some(_) => todo!("handle unexpected token error"),
            None => todo!("handle unepxected eof error"),
        }

        self.skip_whitespace();
        if self.peek() == Some(TOKEN_OPTION) {
            self.builder
                .start_node_at(checkpoint, NODE_TYPE_OPTION.into());
            self.builder.start_node_at(checkpoint, NODE_TYPE.into());
            self.builder.finish_node();

            self.advance();

            self.builder.finish_node();
        }

        self.builder.finish_node();
    }

    fn parse_record_type(&mut self) {
        self.builder.start_node(NODE_TYPE_RECORD.into());

        if self.peek() == Some(TOKEN_KW_REC) {
            self.advance();
        }

        self.skip_whitespace();
        self.expect(TOKEN_CURLY_LEFT);

        loop {
            let prologue = self.parse_prologue();

            match self.peek() {
                Some(TOKEN_IDENTIFIER) | Some(TOKEN_STRING) => {
                    if let Some(prologue) = prologue {
                        self.builder
                            .start_node_at(prologue.start, NODE_RECORD_FIELD.into());

                        self.builder
                            .start_node_at(prologue.start, NODE_PROLOGUE.into());
                        self.builder.finish_node();
                    } else {
                        self.builder.start_node(NODE_RECORD_FIELD.into());
                    }

                    self.builder.start_node(NODE_NAME.into());
                    self.advance();
                    self.builder.finish_node();

                    self.assignment();

                    self.parse_type(DefaultCompositeType::None);
                    self.skip_whitespace();
                    self.expect(TOKEN_EOL);

                    self.builder.finish_node();
                }

                Some(TOKEN_CURLY_RIGHT) => {
                    if let Some(prologue) = prologue
                        && let Some(report) = prologue.expect_no_default()
                    {
                        self.reports.push(report);
                    }

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
                            "expected field name".to_string(),
                            Span::new(self.pos, self.pos + 1),
                        ),
                    ),
                ),
            }
        }

        self.builder.finish_node();
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;

    use crate::{parse, tokenize};

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

    #[test]
    fn parse_option_type() {
        let text = indoc! {r#"
            type Foo: i32?
            type Foo: string   ?
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn parse_array_type() {
        let text = indoc! {r#"
            type Foo: [i32]
            type Foo: [ string ?]
            type Foo: [string?]?
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn parse_record_type() {
        let text = indoc! {r#"
            type Foo: rec {
                // comment
                /// doc comment
                foo: bool

                /// More doc comment
                @default("foobar")
                bar: string

                // Just a comment
                hey: i32

                hoy: rec {
                    @default(true)
                    nested: bool
                }
            }
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);

        let (tree, diagnostics) = parse(tokens);
        insta::assert_debug_snapshot!(tree);
        assert_eq!(diagnostics, vec![]);
    }
}
