mod parser;
mod tokenizer;

pub mod ast;

pub use parser::{Parse, parse};
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
    #[display("`default`")]
    TOKEN_KW_DEFAULT, // default
    #[display("`on`")]
    TOKEN_KW_ON,
    #[display("`for`")]
    TOKEN_KW_FOR,
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
    #[display("`type`")]
    TOKEN_KW_TYPE, // type
    #[display("`rec`")]
    TOKEN_KW_REC, // rec
    #[display("`enum`")]
    TOKEN_KW_ENUM, // enum
    #[display("`union`")]
    TOKEN_KW_UNION, // union
    #[display("`resp`")]
    TOKEN_KW_RESP, // response
    #[display("`route`")]
    TOKEN_KW_ROUTE,

    #[display("`@default`")]
    TOKEN_DECORATOR_DEFAULT, // @default

    /// Name of the API
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

    /// Represents a prologue of type, property, endpoint, ...
    NODE_PROLOGUE,

    NODE_DEFAULT,

    NODE_OBJECT,
    NODE_OBJECT_FIELD,
    NODE_VALUE_BOOL,
    NODE_VALUE_STRING,
    NODE_VALUE_INTEGER,
    NODE_VALUE_FLOAT,

    /// Represents definition of a type connected to an ident.
    NODE_TYPE_DEF,

    /// Reference to a type. Holds an identifier.
    NODE_TYPE_REF,
    NODE_TYPE_I32,
    NODE_TYPE_I64,
    NODE_TYPE_U32,
    NODE_TYPE_U64,
    NODE_TYPE_F32,
    NODE_TYPE_F64,
    NODE_TYPE_DATE,
    NODE_TYPE_TIMESTAMP,
    NODE_TYPE_BOOL,
    NODE_TYPE_STRING,
    NODE_TYPE_FILE,

    NODE_TYPE_OPTION,
    NODE_TYPE_ARRAY,

    NODE_TYPE_RECORD,
    NODE_TYPE_ENUM,
    NODE_TYPE_UNION,

    /// Field used in records and unions
    NODE_TYPE_FIELD,

    /// Member of an enum type. Contains prologue and NODE_VALUE
    NODE_TYPE_ENUM_MEMBER,

    NODE_TYPE_RESPONSE,
    NODE_TYPE_RESP_CONTENT_TYPE,
    NODE_TYPE_RESP_HEADERS,
    NODE_TYPE_RESP_BODY,

    /// Node containing a path (string) of an endpoint or route.
    NODE_PATH,

    NODE_ENDPOINT,
    NODE_ENDPOINT_METHOD,
    NODE_ENDPOINT_PATH,         // Path parameters
    NODE_ENDPOINT_QUERY,        // Query parameters
    NODE_ENDPOINT_HEADERS,      // Header parameters
    NODE_ENDPOINT_ACCEPT,       // Accept header / type of request body
    NODE_ENDPOINT_REQUEST_BODY, // Type of request body

    NODE_RESPONSE,        // Response - `on 200: Foo`
    NODE_RESPONSE_STATUS, // Response status - `200`, `default`, ...

    NODE_ROUTE, // Route group

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
