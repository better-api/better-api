use crate::Kind::*;
use crate::{Kind, Language, SyntaxNode, SyntaxToken};

use rowan::TextRange;
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

    /// Get iterator through all defined [`Server`s](Server).
    pub fn servers(&self) -> impl Iterator<Item = Server> {
        self.0.children().filter_map(Server::cast)
    }

    /// A placeholder, just so that warning about unused `Oracle::parse_type`
    /// goes away
    /// TODO: Remove this in the future.
    pub fn dummy_type(&self) -> Option<Type> {
        None
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

/// [`Name`] can be an identifier or a string. This type provides that information
/// so that user knows if `text::parse_string` should be called or not.
pub enum NameToken {
    Identifier(SyntaxToken),
    String(SyntaxToken),
}

impl NameToken {
    /// Returns the text range of the underlying token.
    pub fn text_range(&self) -> TextRange {
        match self {
            NameToken::Identifier(token) | NameToken::String(token) => token.text_range(),
        }
    }
}

impl Name {
    /// Returns the token representing the name.
    pub fn token(&self) -> NameToken {
        let token = self
            .syntax()
            .first_token()
            .expect("parser should parse name correctly");

        match token.kind() {
            TOKEN_IDENTIFIER => NameToken::Identifier(token),
            TOKEN_STRING => NameToken::String(token),
            _ => unreachable!("parser should parse name correctly"),
        }
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
    pub fn string(&self) -> SyntaxToken {
        let token = self
            .0
            .first_token()
            .expect("parser should parse strings correctly");

        debug_assert_eq!(
            token.kind(),
            TOKEN_STRING,
            "parser should parse strings correctly"
        );

        token
    }
}

ast_node! {
    #[from(NODE_VALUE_INTEGER)]
    /// Integer value.
    struct Integer;
}

impl Integer {
    /// Returns integer representation.
    pub fn integer(&self) -> i128 {
        let token = self
            .0
            .first_token()
            .expect("parser should parse integers correctly");

        debug_assert_eq!(
            token.kind(),
            TOKEN_INTEGER,
            "parser should parse integers correctly"
        );

        token
            .text()
            .parse()
            .expect("tokenizer should emit valid integers")
    }
}

ast_node! {
    #[from(NODE_VALUE_FLOAT)]
    /// Float value.
    struct Float;
}

impl Float {
    /// Returns float representation.
    pub fn float(&self) -> f64 {
        let token = self
            .0
            .first_token()
            .expect("parser should parse floats correctly");

        debug_assert_eq!(
            token.kind(),
            TOKEN_FLOAT,
            "parser should parse floats correctly"
        );

        token
            .text()
            .parse()
            .expect("tokenizer should emit valid floats")
    }
}

ast_node! {
    #[from(NODE_VALUE_BOOL)]
    /// Boolean value.
    struct Bool;
}

impl Bool {
    /// Returns bool representation.
    pub fn bool(&self) -> bool {
        let token = self
            .0
            .first_token()
            .expect("parser should parse bools correctly");

        match token.kind() {
            TOKEN_KW_TRUE => true,
            TOKEN_KW_FALSE => false,
            _ => unreachable!("parser should parse bools correctly"),
        }
    }
}

ast_node! {
    #[from(NODE_VALUE_ARRAY)]
    /// Array value.
    struct Array;
}

impl Array {
    /// Returns iterator over values in array.
    pub fn values(&self) -> impl Iterator<Item = Value> {
        self.0.children().filter_map(Value::cast)
    }
}

ast_node! {
    #[from(NODE_OBJECT)]
    /// An object.
    struct Object;
}

impl Object {
    /// Returns iterator over fields in the object
    pub fn fields(&self) -> impl Iterator<Item = ObjectField> {
        self.0.children().filter_map(ObjectField::cast)
    }
}

ast_node! {
    #[from(NODE_OBJECT_FIELD)]
    /// Field of an object.
    struct ObjectField;
}

impl ObjectField {
    /// Returns name of the field.
    pub fn name(&self) -> Option<Name> {
        self.0.children().find_map(Name::cast)
    }

    /// Returns value of the field.
    pub fn value(&self) -> Option<Value> {
        self.0.children().find_map(Value::cast)
    }
}

ast_node! {
    #[from(
        String,
        Integer,
        Float,
        Bool,
        Array,
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

impl TypeRef {
    /// Returns TOKEN_IDENTIFIER syntax token.
    pub fn name(&self) -> SyntaxToken {
        let token = self
            .0
            .first_token()
            .expect("parser should parse type reference correctly");

        debug_assert_eq!(
            token.kind(),
            TOKEN_IDENTIFIER,
            "parser should parse type reference correctly"
        );

        token
    }
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

impl TypeOption {
    /// Returns the type of the option
    pub fn typ(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_ARRAY)]
    /// Array type
    struct TypeArray;
}

impl TypeArray {
    /// Returns the type of the array
    pub fn typ(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_RECORD)]
    /// Record type
    struct Record;
}

impl Record {
    /// Returns iterator over fields in the record
    pub fn fields(&self) -> impl Iterator<Item = TypeField> {
        self.0.children().filter_map(TypeField::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_ENUM)]
    /// Enum type
    struct Enum;
}

impl Enum {
    /// Returns type of the enum.
    pub fn typ(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }

    /// Returns iterator over it's members.
    pub fn members(&self) -> impl Iterator<Item = EnumMember> {
        self.0.children().filter_map(EnumMember::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_UNION)]
    /// Union type
    struct Union;
}

impl Union {
    /// Returns discriminator value of the union
    pub fn discriminator(&self) -> Option<Value> {
        self.0.children().find_map(Value::cast)
    }

    /// Returns iterator over fields in the union
    pub fn fields(&self) -> impl Iterator<Item = TypeField> {
        self.0.children().filter_map(TypeField::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_FIELD)]
    /// Field used in records and unions
    struct TypeField;
}

impl TypeField {
    /// Returns name of the field.
    pub fn name(&self) -> Option<Name> {
        self.0.children().find_map(Name::cast)
    }

    /// Returns type of the field.
    pub fn typ(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }

    /// Returns field's prologue.
    pub fn prologue(&self) -> Option<Prologue> {
        self.0.children().find_map(Prologue::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_ENUM_MEMBER)]
    /// Member of an enum type.
    struct EnumMember;
}

impl EnumMember {
    /// Returns value of the member.
    ///
    /// Member can contain an error instead of a value, in which case
    /// this method returns `None`.
    pub fn value(&self) -> Option<Value> {
        self.0.children().find_map(Value::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_RESPONSE)]
    /// Response type.
    struct TypeResponse;
}

impl TypeResponse {
    /// Returns content type
    pub fn content_type(&self) -> Option<ResponseContentType> {
        self.0.children().find_map(ResponseContentType::cast)
    }

    /// Returns headers
    pub fn headers(&self) -> Option<ResponseHeaders> {
        self.0.children().find_map(ResponseHeaders::cast)
    }

    /// Returns body
    pub fn body(&self) -> Option<ResponseBody> {
        self.0.children().find_map(ResponseBody::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_RESP_CONTENT_TYPE)]
    /// Response content type.
    struct ResponseContentType;
}

impl ResponseContentType {
    /// Returns value of content type
    pub fn value(&self) -> Option<Value> {
        self.0.children().find_map(Value::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_RESP_HEADERS)]
    /// Response headers.
    struct ResponseHeaders;
}

impl ResponseHeaders {
    /// Returns type of headers
    pub fn typ(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
}

ast_node! {
    #[from(NODE_TYPE_RESP_BODY)]
    /// Response body.
    struct ResponseBody;
}

impl ResponseBody {
    /// Returns type of response body
    pub fn typ(&self) -> Option<Type> {
        self.0.children().find_map(Type::cast)
    }
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
