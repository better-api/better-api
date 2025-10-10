use rowan::ast::AstNode;

use crate::Kind;
use crate::Kind::*;
use crate::Language;

pub type SyntaxNode = rowan::SyntaxNode<Language>;

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
        #[wraps($wraps:ident)]
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
                    $wraps => true,
                    $($variant::KIND => true,)+
                    _ => false
                }
            }

            fn cast(node: SyntaxNode) -> Option<Self> {
                match node.kind() {
                    $wraps => node.children().find_map(Self::cast),
                    $($variant::KIND => Some(Self::$variant($variant::cast(node)?)),)+
                    _ => None,
                }
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

/////////////////
// Basic nodes //
/////////////////

ast_node! {
    #[from(NODE_NAME)]
    /// Represents a name.
    ///
    /// Can be name of an object/record field, endpoint name, ...
    struct Name;
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
    #[from(NODE_API_NAME)]
    /// Name of the API in the spec.
    struct ApiName;
}

impl ApiName {
    pub fn value(&self) -> Option<Value> {
        self.0.children().find_map(Value::cast)
    }
}

////////////
// Values //
////////////

ast_node! {
    #[from(TOKEN_STRING)]
    /// String value.
    struct String;
}

ast_node! {
    #[from(TOKEN_INTEGER)]
    /// Integer value.
    struct Integer;
}

ast_node! {
    #[from(TOKEN_FLOAT)]
    /// Float value.
    struct Float;
}

ast_node! {
    #[from(NODE_BOOL_VALUE)]
    /// Boolean value.
    struct Bool;
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
    #[wraps(NODE_VALUE)]
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
    #[from(TOKEN_IDENTIFIER)]
    /// Reference to a type.
    struct TypeRef;
}

ast_node! {
    #[from(TOKEN_KW_I32)]
    /// 32 bit signed integer
    struct TypeI32;
}

ast_node! {
    #[from(TOKEN_KW_I64)]
    /// 64 bit signed integer
    struct TypeI64;
}

ast_node! {
    #[from(TOKEN_KW_U32)]
    /// 32 bit unsigned integer
    struct TypeU32;
}

ast_node! {
    #[from(TOKEN_KW_U64)]
    /// 64 bit unsigned integer
    struct TypeU64;
}

ast_node! {
    #[from(TOKEN_KW_F32)]
    /// 32 bit float
    struct TypeF32;
}

ast_node! {
    #[from(TOKEN_KW_F64)]
    /// 64 bit float
    struct TypeF64;
}

ast_node! {
    #[from(TOKEN_KW_DATE)]
    /// Date type
    struct TypeDate;
}

ast_node! {
    #[from(TOKEN_KW_TIMESTAMP)]
    /// Timestamp type
    struct TypeTimestamp;
}

ast_node! {
    #[from(TOKEN_KW_BOOL)]
    /// Boolean type
    struct TypeBool;
}

ast_node! {
    #[from(TOKEN_KW_STRING)]
    /// String type
    struct TypeString;
}

ast_node! {
    #[from(TOKEN_KW_FILE)]
    /// File type
    struct TypeFile;
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
    #[wraps(NODE_TYPE)]
    enum Type;
}
