//! Contains definition of a valid spec

use std::collections::HashMap;

use crate::string::{StringId, StringInterner};

pub mod endpoint;
pub mod typ;
pub mod value;

pub(crate) type SymbolTable = HashMap<StringId, typ::TypeDef>;

/// Valid semantic specification
///
/// Usually constructed by [`Oracle`](crate::Oracle).
#[derive(derive_more::Debug, Clone)]
pub struct Spec {
    #[debug(skip)]
    pub(crate) strings: StringInterner,
    pub(crate) symbol_table: SymbolTable,

    pub metadata: Metadata,

    pub(crate) values: value::ValueArena,
    pub(crate) types: typ::TypeArena,
    pub(crate) endpoints: endpoint::EndpointArena,
}

/// Metadata of Better API spec
#[derive(Debug, Clone)]
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
    /// Get [view](SpecContext) over a spec.
    pub(crate) fn ctx<'a>(&'a self) -> SpecContext<'a> {
        SpecContext {
            strings: &self.strings,
            symbol_table: &self.symbol_table,
            values: &self.values,
            types: &self.types,
            endpoints: &self.endpoints,
        }
    }

    #[cfg(test)]
    pub(crate) fn new_test() -> Self {
        Self {
            strings: Default::default(),
            symbol_table: Default::default(),

            values: Default::default(),
            types: Default::default(),
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

/// View over a [`Spec`] used for querying data.
#[derive(Clone, Copy)]
pub(crate) struct SpecContext<'a> {
    pub(crate) strings: &'a StringInterner,
    pub(crate) symbol_table: &'a SymbolTable,

    pub(crate) values: &'a value::ValueArena,
    pub(crate) types: &'a typ::TypeArena,
    pub(crate) endpoints: &'a endpoint::EndpointArena,
}
