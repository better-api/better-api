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

            #[allow(unused)]
            pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == $kind {
                    Some(Self(node))
                } else {
                    None
                }
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
        impl $name {
            #[allow(unused)]
            pub(crate) fn cast(node: SyntaxNode) -> Option<Self> {
                let node = match node.kind() {
                    $($variant::KIND => Self::$variant($variant::cast(node)?),)+
                    _ => return None,
                };
                Some(node)
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
    /// A value of any type.
    enum Value;
}
