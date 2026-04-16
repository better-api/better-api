//! Contains definition of a valid spec
//!
//! <div class="warning">TODO: Add overview of the spec module and how to use it</div>

use std::collections::HashMap;

use crate::{
    spec::arena::typ,
    text::{StringId, StringInterner},
};

pub(crate) mod arena;

pub mod metadata;
pub mod view;

pub(crate) type SymbolTable = HashMap<StringId, typ::TypeDefData>;

/// Valid semantic specification
#[derive(derive_more::Debug, Clone)]
pub struct Spec {
    #[debug(skip)]
    pub(crate) strings: StringInterner,
    pub(crate) symbol_table: SymbolTable,

    /// Metadata of the spec.
    pub metadata: metadata::Metadata,

    pub(crate) values: arena::value::ValueArena,
    pub(crate) types: arena::typ::TypeArena,
    pub(crate) endpoints: arena::endpoint::EndpointArena,
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
            metadata: metadata::Metadata {
                better_api_version: "0.1.0".to_string(),
                version: "1.0".to_string(),
                name: "test spec".to_string(),
                description: None,
                servers: vec![],
            },
        }
    }
}
