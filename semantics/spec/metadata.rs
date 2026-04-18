//! Metadata of Better API spec.
//!
//! Metadata contains general information about the described API, such as its name, version and description.
//! It also contains information about Better API version used by the spec and servers declared by the spec.

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

/// Server where the API is available.
#[derive(Debug, Clone)]
pub struct Server {
    /// Name of the server.
    pub name: String,
    /// URL of the server.
    pub url: String,

    /// Documentation for the server.
    pub docs: Option<String>,
}
