mod parser;
mod tokenizer;

pub mod node;

pub use parser::parse;
pub use tokenizer::{Token, tokenize};

/// All possible kinds of tokens and composite nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::Display)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum Kind {
    #[display("space")]
    TOKEN_SPACE = 0, // white space without new line (' ', '\t', ...)
    #[display("new line")]
    TOKEN_EOL, // End of line (new line)
    #[display("an unknown token")]
    TOKEN_ERROR,

    #[display("an identifier")]
    TOKEN_IDENTIFIER,
    #[display("a string")]
    TOKEN_STRING,
    #[display("an integer")]
    TOKEN_INTEGER,
    #[display("a float")]
    TOKEN_FLOAT,

    #[display("a comment")]
    TOKEN_COMMENT, // '//  comment'
    #[display("a doc comment")]
    TOKEN_DOC_COMMENT, // '/// doc comment'
    #[display("a top comment")]
    TOKEN_TOP_COMMENT, // '//! top comment'

    #[display("`,`")]
    TOKEN_COMMA, // ,
    #[display("`:`")]
    TOKEN_COLON, // :
    #[display("`?`")]
    TOKEN_OPTION, // ?
    #[display("`[`")]
    TOKEN_BRACKET_LEFT, // [
    #[display("`]`")]
    TOKEN_BRACKET_RIGHT, // ]
    #[display("`(`")]
    TOKEN_PAREN_LEFT, // (
    #[display("`)`")]
    TOKEN_PAREN_RIGHT, // )
    #[display("`{{`")]
    TOKEN_CURLY_LEFT, // {
    #[display("`}}`")]
    TOKEN_CURLY_RIGHT, // }

    #[display("`GET`")]
    TOKEN_KW_GET,
    #[display("`POST`")]
    TOKEN_KW_POST,
    #[display("`PUT`")]
    TOKEN_KW_PUT,
    #[display("`DELETE`")]
    TOKEN_KW_DELETE,
    #[display("`PATCH`")]
    TOKEN_KW_PATCH,

    #[display("`true`")]
    TOKEN_KW_TRUE, // true
    #[display("`false`")]
    TOKEN_KW_FALSE, // false
    #[display("`@default`")]
    TOKEN_KW_DEFAULT, // @default
    #[display("`i32`")]
    TOKEN_KW_I32, // i32
    #[display("`i64`")]
    TOKEN_KW_I64, // i64
    #[display("`u32`")]
    TOKEN_KW_U32, // u32
    #[display("`u64`")]
    TOKEN_KW_U64, // u64
    #[display("`f32`")]
    TOKEN_KW_F32, // f32
    #[display("`f64`")]
    TOKEN_KW_F64, // f64
    #[display("`date`")]
    TOKEN_KW_DATE, // date
    #[display("`timestamp`")]
    TOKEN_KW_TIMESTAMP, // timestamp
    #[display("`bool`")]
    TOKEN_KW_BOOL, // bool
    #[display("`string`")]
    TOKEN_KW_STRING, // string
    #[display("`file`")]
    TOKEN_KW_FILE, // file

    /// Name of the PAI
    NODE_API_NAME,
    /// Better API Version root node
    NODE_BETTER_API,
    /// API Version root node
    NODE_VERSION,
    /// Server root node
    NODE_SERVER,

    /// Underlying value represents a name. It can be
    /// name of the field, name of the endpoint, ...
    NODE_NAME,

    /// Represents a value. Should have exactly one child.
    NODE_VALUE,

    /// Represents a prologue of type, property, endpoint, ...
    NODE_PROLOGUE,

    NODE_DEFAULT,

    NODE_OBJECT,
    NODE_OBJECT_FIELD,
    NODE_BOOL_VALUE,

    NODE_ERROR,

    NODE_ROOT, // Root node of the file that's parsed
}

use Kind::*;

impl From<Kind> for rowan::SyntaxKind {
    fn from(value: Kind) -> Self {
        Self(value as u16)
    }
}

/// A type that implements rowan::Language trait for nicer
/// API for working with tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Language {}

impl rowan::Language for Language {
    type Kind = Kind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= NODE_ROOT as u16);
        unsafe { std::mem::transmute::<u16, Kind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}
