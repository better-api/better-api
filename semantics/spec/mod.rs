//! Contains definition of a valid spec

use std::collections::HashMap;

use crate::string::{StringId, StringInterner};

pub mod endpoint;
pub mod typ;
pub mod value;

/// Valid semantic specification
///
/// Usually constructed by [`Oracle`](crate::Oracle).
pub struct Spec {
    pub strings: StringInterner,
    pub symbol_table: HashMap<StringId, typ::TypeId>,

    pub metadata: Metadata,

    pub values: value::ValueArena,
    pub types: typ::TypeArena,
    pub endpoints: endpoint::EndpointArena,
}

/// Metadata of Better API spec
pub struct Metadata {
    pub better_api_version: StringId,

    pub version: StringId,
    pub name: StringId,

    pub servers: Vec<Server>,
}

/// Server that is part of spec metadata
pub struct Server {
    pub name: StringId,
    pub url: StringId,
}
