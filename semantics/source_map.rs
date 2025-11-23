use std::collections::HashMap;

use better_api_syntax::ast::AstNode;
use better_api_syntax::{Language, SyntaxNodePtr, ast};

use crate::{Element, StringId, typ::TypeId, value::ValueId};

/// Maps syntax nodes to semantic elements.
///
/// Methods for getting syntax node from semantic elements assume that the
/// `SourceMap` is constructed correctly. That is, each semantic element should have an
/// entry in the source map. Correctly working [`Oracle`](crate::Oracle) should do that.
/// These methods will panic if given semantic element, doesn't have a syntax node in the source
/// map. In which case there is something wrong with the [`Oracle`](crate::Oracle) implementation.
#[derive(Clone)]
pub struct SourceMap<'a> {
    /// Root node of the parsed tree.
    root: &'a ast::Root,

    // Mappings from semantic element to syntax node
    values: HashMap<ValueId, SyntaxNodePtr>,
    types: HashMap<TypeId, SyntaxNodePtr>,
    type_definitions: HashMap<StringId, SyntaxNodePtr>,

    // Mapping from syntax node to semantic element.
    syntax: HashMap<SyntaxNodePtr, Element>,
}

impl<'a> SourceMap<'a> {
    /// Construct a new source map.
    pub fn new(root: &'a ast::Root) -> Self {
        Self {
            root,
            values: Default::default(),
            types: Default::default(),
            type_definitions: Default::default(),
            syntax: Default::default(),
        }
    }

    /// Insert a semantic value element.
    pub fn insert_value(&mut self, value: ValueId, node: &ast::Value) {
        let ptr = SyntaxNodePtr::new(node.syntax());
        self.values.insert(value, ptr);
        self.syntax.insert(ptr, Element::Value(value));
    }

    /// Insert a semantic type element.
    pub fn insert_type(&mut self, typ: TypeId, node: &ast::Type) {
        let ptr = SyntaxNodePtr::new(node.syntax());
        self.types.insert(typ, ptr);
        self.syntax.insert(ptr, Element::Type(typ));
    }

    /// Insert a semantic type definition element.
    pub fn insert_type_definition(&mut self, name: StringId, node: &ast::TypeDefinition) {
        let ptr = SyntaxNodePtr::new(node.syntax());
        self.type_definitions.insert(name, ptr);
        self.syntax.insert(ptr, Element::TypeDefinition(name));
    }

    /// Get value node.
    pub fn get_value(&self, value: ValueId) -> ast::Value {
        let ptr = self
            .values
            .get(&value)
            .expect("source map for values should be constructed correctly");
        let node = ptr.to_node(self.root.syntax());
        ast::Value::cast(node).expect("source map for value should point to ast::Value")
    }

    /// Get type node.
    pub fn get_type(&self, typ: TypeId) -> ast::Type {
        let ptr = self
            .types
            .get(&typ)
            .expect("source map for types should be constructed correctly");
        let node = ptr.to_node(self.root.syntax());
        ast::Type::cast(node).expect("source map for type should point to ast::Value")
    }

    /// Get type definition node.
    pub fn get_type_definition(&self, name: StringId) -> ast::TypeDefinition {
        let ptr = self
            .type_definitions
            .get(&name)
            .expect("source map for type definitions should be constructed correctly");
        let node = ptr.to_node(self.root.syntax());
        ast::TypeDefinition::cast(node)
            .expect("source map for type definition should point to ast::Value")
    }

    /// Get semantic element for specified syntax node.
    pub fn get_for_syntax(&self, node: &impl AstNode<Language = Language>) -> Option<&Element> {
        let ptr = SyntaxNodePtr::new(node.syntax());
        self.syntax.get(&ptr)
    }
}
