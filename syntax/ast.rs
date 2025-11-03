use crate::Kind::*;
use crate::{Kind, Language, SyntaxNode, SyntaxToken};

pub use rowan::ast::AstNode;

// Helper macros to generator AST nodes as structs.
macro_rules! ast_node {
    (
        #[from($kind:ident)]
        $(#[$attr:meta])*
        struct $name:ident;
    ) => {
        $(#[$attr])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $name(SyntaxNode);

        impl $name {
            pub const KIND: Kind = $kind;
        }

        impl AstNode for $name {
            type Language = Language;

            fn can_cast(kind: Kind) -> bool {
                kind == $kind
            }

            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == $kind {
                    Some(Self(node))
                } else {
                    None
               }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    };

    (
        #[from($($variant:ident),+ $(,)?)]
        $(#[$attr:meta])*
        enum $name:ident;
    ) => {
        $(#[$attr])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variant($variant)),+
        }

        impl AstNode for $name {
            type Language = Language;

            fn can_cast(kind: Kind) -> bool {
                match kind {
                    $($variant::KIND => true,)+
                    _ => false
                }
            }

            fn cast(node: SyntaxNode) -> Option<Self> {
                let node = match node.kind() {
                    $($variant::KIND => Self::$variant($variant::cast(node)?),)+
                    _ => return None,
                };

                Some(node)
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$variant(v) => v.syntax(),)+
                }
            }
        }
    };
}

///////////////
// Root node //
///////////////

ast_node! {
    #[from(NODE_ROOT)]
    /// Root node
    struct Root;
}

impl Root {
    /// Get iterator through all defined [`ApiVersion`s](ApiVersion).
    pub fn api_versions(&self) -> impl Iterator<Item = ApiVersion> {
        self.0.children().filter_map(ApiVersion::cast)
    }

    /// Get iterator through all defined [`ApiName`s](ApiName).
    pub fn api_names(&self) -> impl Iterator<Item = ApiName> {
        self.0.children().filter_map(ApiName::cast)
    }

    /// Get iterator through all defined [`BetterApiVersion`s](BetterApiVersion).
    pub fn better_api_versions(&self) -> impl Iterator<Item = BetterApiVersion> {
        self.0.children().filter_map(BetterApiVersion::cast)
    }
}

/////////////////
// Basic nodes //
/////////////////

ast_node! {
    #[from(NODE_ERROR)]
    /// Error node
    struct Error;
}

ast_node! {
    #[from(NODE_NAME)]
    /// Represents a name.
    ///
    /// Can be name of an object/record field, endpoint name, ...
    struct Name;
}

pub enum NameToken {
    Identifier(SyntaxToken),
    String(SyntaxToken),
}

impl Name {
    pub fn token(&self) -> Option<NameToken> {
        self.syntax().first_token().and_then(|t| match t.kind() {
            TOKEN_IDENTIFIER => Some(NameToken::Identifier(t)),
            TOKEN_STRING => Some(NameToken::String(t)),
            _ => None,
        })
    }
}

ast_node! {
    #[from(NODE_PROLOGUE)]
    /// Prologue of type, property, endpoint, ...
    struct Prologue;
}

//////////////////////////////////
// Direct children of root node //
//////////////////////////////////

ast_node! {
    #[from(NODE_BETTER_API)]
    /// Version of Better API used.
    struct BetterApiVersion;
}

impl BetterApiVersion {
    pub fn value(&self) -> Option<Value> {
        self.0.children().find_map(Value::cast)
    }
}

ast_node! {
    #[from(NODE_API_NAME)]
    /// Name of the API in the spec.
    struct ApiName;
}

impl ApiName {
    pub fn value(&self) -> Option<Value> {
        self.0.children().find_map(Value::cast)
    }
}

ast_node! {
    #[from(NODE_VERSION)]
    /// Version of the API
    struct ApiVersion;
}

impl ApiVersion {
    pub fn value(&self) -> Option<Value> {
        self.0.children().find_map(Value::cast)
    }
}

ast_node! {
    #[from(NODE_SERVER)]
    /// Server info
    struct Server;
}

impl Server {
    pub fn value(&self) -> Option<Value> {
        self.0.children().find_map(Value::cast)
    }
}

////////////
// Values //
////////////

ast_node! {
    #[from(NODE_VALUE_STRING)]
    /// String value.
    struct String;
}

impl String {
    /// Returns TOKEN_STRING syntax token.
    pub fn string(&self) -> Option<SyntaxToken> {
        self.0.first_token().and_then(|t| {
            if t.kind() == TOKEN_STRING {
                Some(t)
            } else {
                None
            }
        })
    }
}

ast_node! {
    #[from(NODE_VALUE_INTEGER)]
    /// Integer value.
    struct Integer;
}

impl Integer {
    /// Returns integer representation.
    pub fn integer(&self) -> Option<i128> {
        self.0.first_token().and_then(|t| {
            if t.kind() == TOKEN_INTEGER {
                Some(
                    t.text()
                        .parse()
                        .expect("tokenizer should emit valid integers"),
                )
            } else {
                None
            }
        })
    }
}

ast_node! {
    #[from(NODE_VALUE_FLOAT)]
    /// Float value.
    struct Float;
}

impl Float {
    /// Returns float representation.
    pub fn float(&self) -> Option<f64> {
        self.0.first_token().and_then(|t| {
            if t.kind() == TOKEN_FLOAT {
                Some(
                    t.text()
                        .parse()
                        .expect("tokenizer should emit valid floats"),
                )
            } else {
                None
            }
        })
    }
}

ast_node! {
    #[from(NODE_VALUE_BOOL)]
    /// Boolean value.
    struct Bool;
}

impl Bool {
    /// Returns bool representation.
    pub fn bool(&self) -> Option<bool> {
        self.0.first_token().and_then(|t| match t.kind() {
            TOKEN_KW_TRUE => Some(true),
            TOKEN_KW_FALSE => Some(false),
            _ => None,
        })
    }
}

ast_node! {
    #[from(NODE_OBJECT)]
    /// An object.
    struct Object;
}

ast_node! {
    #[from(NODE_OBJECT_FIELD)]
    /// Field of an object.
    struct ObjectField;
}

ast_node! {
    #[from(
        String,
        Integer,
        Float,
        Bool,
        Object,
    )]
    /// A value of any type.
    enum Value;
}

///////////
// Types //
///////////

ast_node! {
    #[from(NODE_TYPE_DEF)]
    /// Type definition
    struct TypeDefinition;
}

ast_node! {
    #[from(NODE_TYPE_REF)]
    /// Reference to a type. Holds an identifier.
    struct TypeRef;
}

ast_node! {
    #[from(NODE_TYPE_I32)]
    /// 32 bit signed integer
    struct TypeI32;
}

ast_node! {
    #[from(NODE_TYPE_I64)]
    /// 64 bit signed integer
    struct TypeI64;
}

ast_node! {
    #[from(NODE_TYPE_U32)]
    /// 32 bit unsigned integer
    struct TypeU32;
}

ast_node! {
    #[from(NODE_TYPE_U64)]
    /// 64 bit unsigned integer
    struct TypeU64;
}

ast_node! {
    #[from(NODE_TYPE_F32)]
    /// 32 bit float
    struct TypeF32;
}

ast_node! {
    #[from(NODE_TYPE_F64)]
    /// 64 bit float
    struct TypeF64;
}

ast_node! {
    #[from(NODE_TYPE_DATE)]
    /// Date type
    struct TypeDate;
}

ast_node! {
    #[from(NODE_TYPE_TIMESTAMP)]
    /// Timestamp type
    struct TypeTimestamp;
}

ast_node! {
    #[from(NODE_TYPE_BOOL)]
    /// Boolean type
    struct TypeBool;
}

ast_node! {
    #[from(NODE_TYPE_STRING)]
    /// String type
    struct TypeString;
}

ast_node! {
    #[from(NODE_TYPE_FILE)]
    /// File type
    struct TypeFile;
}

ast_node! {
    #[from(NODE_TYPE_OPTION)]
    /// Option type.
    struct TypeOption;
}

ast_node! {
    #[from(NODE_TYPE_ARRAY)]
    /// Array type
    struct TypeArray;
}

ast_node! {
    #[from(NODE_TYPE_RECORD)]
    /// Record type
    struct Record;
}

ast_node! {
    #[from(NODE_TYPE_ENUM)]
    /// Enum type
    struct Enum;
}

ast_node! {
    #[from(NODE_TYPE_UNION)]
    /// Union type
    struct Union;
}

ast_node! {
    #[from(NODE_TYPE_FIELD)]
    /// Field used in records and unions
    struct TypeField;
}

ast_node! {
    #[from(NODE_TYPE_ENUM_MEMBER)]
    /// Member of an enum type.
    struct EnumMember;
}

ast_node! {
    #[from(NODE_TYPE_RESPONSE)]
    /// Response type.
    struct TypeResponse;
}

ast_node! {
    #[from(NODE_TYPE_RESP_CONTENT_TYPE)]
    /// Response content type.
    struct ResponseContentType;
}

ast_node! {
    #[from(NODE_TYPE_RESP_HEADERS)]
    /// Response headers.
    struct ResponseHeaders;
}

ast_node! {
    #[from(NODE_TYPE_RESP_BODY)]
    /// Response body.
    struct ResponseBody;
}

ast_node! {
    #[from(
        TypeOption,
        TypeArray,
        Record,
        Enum,
        Union,
        TypeResponse,
        TypeRef,
        TypeI32,
        TypeI64,
        TypeU32,
        TypeU64,
        TypeF32,
        TypeF64,
        TypeDate,
        TypeTimestamp,
        TypeBool,
        TypeString,
        TypeFile,
    )]
    /// Represents a type.
    enum Type;
}

///////////////
// Endpoints //
///////////////

ast_node! {
    #[from(NODE_ENDPOINT)]
    /// An endpoint
    struct Endpoint;
}

ast_node! {
    #[from(NODE_ENDPOINT_METHOD)]
    /// Endpoint method
    struct EndpointMethod;
}

ast_node! {
    #[from(NODE_PATH)]
    /// Path of an endpoint or a route.
    struct Path;
}

ast_node! {
    #[from(NODE_ENDPOINT_PATH)]
    /// Endpoint's path parameters
    struct EndpointPathParams;
}

ast_node! {
    #[from(NODE_ENDPOINT_QUERY)]
    /// Endpoint's query parameters
    struct EndpointQueryParams;
}

ast_node! {
    #[from(NODE_ENDPOINT_HEADERS)]
    /// Endpoint's header parameters
    struct EndpointHeaderParams;
}

ast_node! {
    #[from(NODE_ENDPOINT_ACCEPT)]
    /// Content type of the endpoint's request body.
    ///
    /// Represents how the request body is encoded as a MIME type.
    struct EndpointAccept;
}

ast_node! {
    #[from(NODE_ENDPOINT_REQUEST_BODY)]
    /// Type of the endpoint's request body.
    ///
    /// Represents the type of the endpoint's request body
    /// as a Better API type.
    struct EndpointRequestBody;
}

ast_node! {
    #[from(NODE_RESPONSE)]
    /// Endpoint's response.
    struct EndpointResponse;
}

ast_node! {
    #[from(NODE_RESPONSE_STATUS)]
    /// Status code of an endpoint response.
    ///
    /// This is a child node of [`EndpointResponse`]
    /// and represents the status of a single response.
    struct EndpointResponseStatus;
}

////////////
// Routes //
////////////

ast_node! {
    #[from(NODE_ROUTE)]
    /// Route group
    struct Route;
}
