//! Defines semantic representation of endpoints and route groups.
//!
//! The main data structure is an [`EndpointArena`] that holds the semantic
//! endpoints and routes. Endpoints and routes are referenced with [`EndpointId`]
//! and [`RouteId`].
//!
//! ## Building An Arena
//!
//! Endpoints are created with [`EndpointArena::add_endpoint`] and route groups
//! with [`EndpointArena::add_route`]. Builders let us append responses without
//! extra allocations while preserving a compact arena layout.
//!
//! ## Getting Endpoints and Routes
//!
//! To retrieve route or endpoint by id, use [`EndpointArena::get_route`] or
//! [`EndpointArena::get_endpoint`]. [`Route`] and [`Endpoint`] contain methods
//! for iterating through children (inner routes, endpoints and responses).
//! See documentation for individual types for more info on available methods.

// TODO:
// - [ ] iterators
// - [ ] tests

use crate::path::{Path, PathArena, PathId, PathPart};
use crate::string::StringId;
use crate::typ::TypeId;

/// Route group representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone, PartialEq)]
pub struct Route<'a> {
    /// Route path at this level of the hierarchy.
    pub path: Path<'a>,

    /// Arena that holds the route, used to iterate through responses, endpoints and routes
    #[debug(skip)]
    arena: &'a EndpointArena,
    /// End index of the route in the arena, used to know where to stop
    /// child iteration.
    end: u32,
}

impl<'a> Route<'a> {
    /// Returns an iterator over endpoints in this route group.
    pub fn endpoints(&self) {
        todo!("implement endpoint iterator")
    }

    /// Returns an iterator over routes in this route group.
    pub fn routes(&self) {
        todo!("implement routes iterator")
    }

    /// Returns an iterator over responses defined for this route group.
    pub fn responses(&self) {
        todo!("implement responses iterator")
    }
}

/// Core fields used to describe an endpoint.
#[derive(Clone, Debug, PartialEq)]
pub struct EndpointFields {
    /// HTTP method used by the endpoint.
    pub method: http::Method,

    /// Name assigned to the endpoint.
    pub name: Option<StringId>,

    /// Path parameter type.
    pub path: Option<TypeId>,
    /// Query parameter type.
    pub query: Option<TypeId>,
    /// Headers type.
    pub headers: Option<TypeId>,

    // TODO: Accept should actually be an array of mime types.
    // Where exactly this validation happens (during construction or late) is yet to be decided.
    // It is also yet to be decided how to validate mime type is correct. This probably boils
    // down to using a library, but I haven't looked into it yet.
    /// Request `Content-Type` MIME types that are allowed.
    pub accept: Option<StringId>,
    /// Request body type.
    pub request_body: Option<TypeId>,
}

/// Endpoint representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone, PartialEq)]
pub struct Endpoint<'a> {
    /// Path of the endpoint.
    pub path: Path<'a>,
    /// Endpoint fields.
    pub fields: &'a EndpointFields,

    /// Arena that holds the endpoint, used to iterate through responses
    #[debug(skip)]
    arena: &'a EndpointArena,
    /// End index of the endpoint in the arena, used to know where to stop
    /// response iteration.
    end: u32,
}

impl<'a> Endpoint<'a> {
    /// Returns an iterator over responses for this endpoint.
    pub fn responses(&self) {
        todo!("implement responses iterator")
    }
}

/// Status definition for a response.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResponseStatus {
    /// Default response for a route or endpoint.
    Default,
    /// Response for a specific HTTP status code.
    Code(http::StatusCode),
}

/// Response definition
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Response {
    /// Status code of the response.
    pub status: Option<ResponseStatus>,
    /// Response body type.
    pub type_id: Option<TypeId>,
}

/// Id of an endpoint stored in the [`EndpointArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EndpointId(u32);

/// Id of a route stored in the [`EndpointArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RouteId(u32);

struct Parent<'p> {
    data: &'p mut Vec<Slot>,
    paths: &'p mut PathArena,

    /// Path ID of the parent's path
    path_id: Option<PathId>,
}

/// Helper type for adding responses to an endpoint.
///
/// Constructed via [`EndpointArena::add_endpoint`] or [`RouteBuilder::add_endpoint`].
/// Call [`finish`](EndpointBuilder::finish) once all responses are added.
/// Dropping the builder without finishing rolls back any changes.
pub struct EndpointBuilder<'p> {
    parent: Parent<'p>,

    start: EndpointId,

    finished: bool,
}

impl<'p> EndpointBuilder<'p> {
    fn new(parent: Parent<'p>, path: PathPart, fields: EndpointFields) -> Self {
        let path_id = parent.paths.insert(parent.path_id, path);

        let idx = parent.data.len();
        parent.data.push(Slot::Endpoint {
            path: path_id,
            fields,
            end: 0,
        });

        Self {
            parent,
            start: EndpointId(idx as u32),
            finished: false,
        }
    }

    /// Add a response to this endpoint.
    pub fn add_response(&mut self, resp: Response) {
        self.parent.data.push(Slot::Response(resp))
    }

    /// Finalize the endpoint and return its [`EndpointId`].
    pub fn finish(mut self) -> EndpointId {
        self.finished = true;

        let idx = self.parent.data.len();

        let start = self.start.0 as usize;
        let head = &mut self.parent.data[start];
        match head {
            Slot::Endpoint { end, .. } => *end = idx as u32,
            _ => unreachable!("invalid EndpintBuilder start"),
        }

        self.start
    }
}

impl<'p> Drop for EndpointBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        self.parent.data.truncate(self.start.0 as usize);
    }
}

/// Helper type for adding endpoints and responses to a route.
///
/// Constructed via [`EndpointArena::add_route`] or [`RouteBuilder::add_route`]. Call
/// [`finish`](RouteBuilder::finish) once all nested entries are added.
/// Dropping the builder without finishing rolls back any changes.
pub struct RouteBuilder<'p> {
    parent: Parent<'p>,

    /// Path id of the whole route of this path. Used as
    /// prefix of children routes.
    path_id: PathId,

    start: RouteId,

    finished: bool,
}

impl<'p> RouteBuilder<'p> {
    fn new(parent: Parent<'p>, path: PathPart) -> Self {
        let path_id = parent.paths.insert(parent.path_id, path);

        let idx = parent.data.len();
        parent.data.push(Slot::Route {
            path: path_id,
            end: 0,
        });

        Self {
            parent,
            path_id,
            start: RouteId(idx as u32),
            finished: false,
        }
    }

    /// Returns self as Parent
    fn as_parent<'a>(&'a mut self) -> Parent<'a> {
        Parent {
            data: self.parent.data,
            paths: self.parent.paths,
            path_id: Some(self.path_id),
        }
    }

    /// Add a response to this route.
    pub fn add_response(&mut self, resp: Response) {
        self.parent.data.push(Slot::Response(resp))
    }

    /// Start building an endpoint under this route.
    pub fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        fields: EndpointFields,
    ) -> EndpointBuilder<'a> {
        EndpointBuilder::new(self.as_parent(), path, fields)
    }

    /// Starts building a nested route group under this route.
    pub fn add_route<'a>(&'a mut self, path: PathPart) -> RouteBuilder<'a> {
        RouteBuilder::new(self.as_parent(), path)
    }

    /// Finalize the route and return its [`RouteId`].
    pub fn finish(mut self) -> RouteId {
        self.finished = true;

        let idx = self.parent.data.len();

        let start = self.start.0 as usize;
        let head = &mut self.parent.data[start];
        match head {
            Slot::Endpoint { end, .. } => *end = idx as u32,
            _ => unreachable!("invalid EndpintBuilder start"),
        }

        self.start
    }
}

impl<'p> Drop for RouteBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        self.parent.data.truncate(self.start.0 as usize);
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Slot {
    Route {
        path: PathId,

        // Id after the last child of the route.
        // Used for knowing when to stop iteration
        end: u32,
    },
    Endpoint {
        path: PathId,
        fields: EndpointFields,

        // Id after the last response of the endpoint.
        // Used for knowing when to stop iteration
        end: u32,
    },

    Response(Response),
}

/// Arena that holds semantic endpoints and routes.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct EndpointArena {
    data: Vec<Slot>,
    paths: PathArena,
}

impl EndpointArena {
    /// Create a new endpoint arena.
    pub fn new() -> Self {
        Self::default()
    }

    fn parent<'a>(&'a mut self) -> Parent<'a> {
        Parent {
            data: &mut self.data,
            paths: &mut self.paths,
            path_id: None,
        }
    }

    /// Start building an endpoint at the root.
    pub fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        fields: EndpointFields,
    ) -> EndpointBuilder<'a> {
        EndpointBuilder::new(self.parent(), path, fields)
    }

    /// Start building a route at the root.
    pub fn add_route<'a>(&'a mut self, path: PathPart) -> RouteBuilder<'a> {
        RouteBuilder::new(self.parent(), path)
    }

    /// Get [`Endpoint`] by id.
    pub fn get_endpoint<'a>(&'a self, id: EndpointId) -> Endpoint<'a> {
        match &self.data[id.0 as usize] {
            Slot::Endpoint { path, fields, end } => {
                let path = self.paths.get(*path);

                Endpoint {
                    arena: self,
                    path,
                    fields,
                    end: *end,
                }
            }
            _ => unreachable!("slot at endpoint id should contain an endpoint"),
        }
    }

    /// Get [`Route`] by id.
    pub fn get_route<'a>(&'a self, id: RouteId) -> Route<'a> {
        match &self.data[id.0 as usize] {
            Slot::Route { path, end } => {
                let path = self.paths.get(*path);

                Route {
                    path,
                    arena: self,
                    end: *end,
                }
            }
            _ => unreachable!("slot at route id should contain a route"),
        }
    }
}
