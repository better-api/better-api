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
// - [ ] tests

use crate::path::{Path, PathArena, PathId, PathPart};
use crate::string::StringId;
use crate::typ::TypeId;
use crate::value::ValueId;

/// Route group representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone)]
pub struct Route<'a> {
    /// Id of the route
    pub id: RouteId,
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
    pub fn endpoints(&self) -> EndpointIterator<'a> {
        EndpointIterator {
            arena: self.arena,
            current: self.id.0 + 1,
            end: self.end,
        }
    }

    /// Returns an iterator over routes in this route group.
    pub fn routes(&self) -> RouteIterator<'a> {
        RouteIterator {
            arena: self.arena,
            current: self.id.0 + 1,
            end: self.end,
        }
    }

    /// Returns an iterator over responses defined for this route group.
    pub fn responses(&self) -> ResponseIterator<'a> {
        ResponseIterator {
            arena: self.arena,
            current: self.id.0 + 1,
            end: self.end,
        }
    }
}

/// Core fields used to describe an endpoint.
#[derive(Clone, Debug)]
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

    // TODO: Accept should be an array of mime types.
    // Where exactly this validation happens (during construction or late) is yet to be decided.
    // It is also yet to be decided how to validate mime type is correct. This probably boils
    // down to using a library, but I haven't looked into it yet.
    /// Request `Content-Type` MIME types that are allowed.
    pub accept: Option<ValueId>,
    /// Request body type.
    pub request_body: Option<TypeId>,
}

/// Endpoint representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone)]
pub struct Endpoint<'a> {
    /// Id of the endpoint
    pub id: EndpointId,
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
    pub fn responses(&self) -> ResponseIterator<'a> {
        ResponseIterator {
            arena: self.arena,
            current: self.id.0 + 1,
            end: self.end,
        }
    }
}

/// Iterator over endpoints inside a [`Route`].
pub struct EndpointIterator<'a> {
    arena: &'a EndpointArena,

    current: u32,
    end: u32,
}

impl<'a> Iterator for EndpointIterator<'a> {
    type Item = Endpoint<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current < self.end {
            match &self.arena.data[self.current as usize] {
                Slot::Route { end, .. } => self.current = *end,
                Slot::Response(_) => self.current += 1,
                Slot::Endpoint { end, .. } => {
                    let id = EndpointId(self.current);
                    self.current = *end;
                    return Some(self.arena.get_endpoint(id));
                }
            }
        }

        None
    }
}

/// Iterator over routes inside a [`Route`]
pub struct RouteIterator<'a> {
    arena: &'a EndpointArena,

    current: u32,
    end: u32,
}

impl<'a> Iterator for RouteIterator<'a> {
    type Item = Route<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current < self.end {
            match &self.arena.data[self.current as usize] {
                Slot::Response(_) => self.current += 1,
                Slot::Endpoint { end, .. } => self.current = *end,
                Slot::Route { end, .. } => {
                    let id = RouteId(self.current);
                    self.current = *end;
                    return Some(self.arena.get_route(id));
                }
            }
        }

        None
    }
}

/// Iterator over responses inside a [`Route`] or [`Endpoint`]
pub struct ResponseIterator<'a> {
    arena: &'a EndpointArena,

    current: u32,
    end: u32,
}

impl<'a> Iterator for ResponseIterator<'a> {
    type Item = Response;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current < self.end {
            match &self.arena.data[self.current as usize] {
                Slot::Endpoint { end, .. } => self.current = *end,
                Slot::Route { end, .. } => self.current = *end,
                Slot::Response(resp) => {
                    self.current += 1;
                    return Some(*resp);
                }
            }
        }

        None
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
#[derive(Debug, Clone, Copy)]
pub struct Response {
    /// Id of the response
    pub id: ResponseId,
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

/// Id of a response stored in the [`EndpointArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ResponseId(u32);

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

#[derive(Clone, Debug)]
enum Slot {
    Route {
        path: PathId,

        /// Id after the last child of the route.
        /// Used for knowing when to stop iteration
        end: u32,
    },
    Endpoint {
        path: PathId,
        fields: EndpointFields,

        /// Id after the last response of the endpoint.
        /// Used for knowing when to stop iteration
        end: u32,
    },

    Response(Response),
}

/// Arena that holds semantic endpoints and routes.
#[derive(Debug, Clone, Default)]
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
                    id,
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
                    id,
                    path,
                    arena: self,
                    end: *end,
                }
            }
            _ => unreachable!("slot at route id should contain a route"),
        }
    }

    /// Get [`Response`] by id.
    pub fn get_response(&self, id: ResponseId) -> Response {
        match &self.data[id.0 as usize] {
            Slot::Response(resp) => *resp,
            _ => unreachable!("slot at response id should contain a response"),
        }
    }
}
