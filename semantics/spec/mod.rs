//! Contains definition of a valid spec

use std::collections::HashMap;

use crate::string::{StringId, StringInterner};

pub mod endpoint;
pub mod typ;
pub mod value;

pub type SymbolTable = HashMap<StringId, typ::TypeDef>;

/// Valid semantic specification
///
/// Usually constructed by [`Oracle`](crate::Oracle).
#[derive(derive_more::Debug, Clone)]
pub struct Spec {
    #[debug(skip)]
    pub strings: StringInterner,
    pub symbol_table: SymbolTable,

    pub metadata: Metadata,

    pub values: value::ValueArena,
    pub types: typ::TypeArena,
    pub endpoints: endpoint::EndpointArena,
}

/// Metadata of Better API spec
#[derive(Debug, Clone)]
pub struct Metadata {
    pub better_api_version: StringId,

    pub version: StringId,
    pub name: StringId,
    pub description: Option<StringId>,

    pub servers: Vec<Server>,
}

/// Server that is part of spec metadata
#[derive(Debug, Clone)]
pub struct Server {
    pub name: StringId,
    pub url: StringId,

    pub docs: Option<StringId>,
}
