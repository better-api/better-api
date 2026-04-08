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
/// Usually constructed by [`Oracle`](crate::Oracle).
#[derive(derive_more::Debug, Clone)]
pub struct Spec {
    #[debug(skip)]
    pub(crate) strings: StringInterner,
    pub(crate) symbol_table: SymbolTable,

    pub metadata: Metadata,

    pub(crate) values: arena::value::ValueArena,
    pub(crate) types: arena::typ::TypeArena,
    // pub(crate) endpoints: arena::endpoint::EndpointArena,
}

/// Metadata of Better API spec
#[derive(Debug, Clone, Default)]
pub struct Metadata {
    pub better_api_version: String,

    pub version: String,
    pub name: String,
    pub description: Option<String>,

    pub servers: Vec<Server>,
}

/// Server that is part of spec metadata
#[derive(Debug, Clone)]
pub struct Server {
    pub name: String,
    pub url: String,

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
            // endpoints: Default::default(),
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
