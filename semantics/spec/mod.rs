//! Contains definition of a valid spec

use std::collections::HashMap;

use crate::{
    spec::arena::typ,
    text::{StringId, StringInterner},
};

pub(crate) mod arena;
pub mod view;

pub(crate) type SymbolTable = HashMap<StringId, typ::TypeDefData>;

/// Valid semantic specification
///
/// Usually constructed by [`Analyzer`](crate::analyzer::Analyzer).
#[derive(derive_more::Debug, Clone)]
pub struct Spec {
    #[debug(skip)]
    pub(crate) strings: StringInterner,
    pub(crate) symbol_table: SymbolTable,

    /// Metadata declared by the spec.
    pub metadata: Metadata,

    pub(crate) values: arena::value::ValueArena,
    pub(crate) types: arena::typ::TypeArena,
    pub(crate) endpoints: arena::endpoint::EndpointArena,
}

/// Metadata of Better API spec
#[derive(Debug, Clone, Default)]
pub struct Metadata {
    /// Better API version used by the spec.
    pub better_api_version: String,

    /// Version of the described API.
    pub version: String,
    /// Name of the described API.
    pub name: String,
    /// Description of the described API.
    pub description: Option<String>,

    /// Servers declared by the spec.
    pub servers: Vec<Server>,
}

/// Server that is part of spec metadata
#[derive(Debug, Clone)]
pub struct Server {
    /// Name of the server.
    pub name: String,
    /// URL of the server.
    pub url: String,

    /// Documentation for the server.
    pub docs: Option<String>,
}

impl Spec {
    #[cfg(test)]
    pub(crate) fn new_test() -> Self {
        let symbol_table = SymbolTable::default();
        let types = typ::builder::TypeArenaBuilder::default()
            .finish(&symbol_table)
            .expect("empty type arena builder should always finish");

        Self {
            strings: Default::default(),
            symbol_table,

            values: Default::default(),
            types,
            endpoints: Default::default(),
            metadata: Metadata {
                better_api_version: "0.1.0".to_string(),
                version: "1.0".to_string(),
                name: "test spec".to_string(),
                description: None,
                servers: vec![],
            },
        }
    }
}
