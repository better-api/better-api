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

use crate::path::{Path, PathArena, PathId, PathPart};
use crate::spec::typ::TypeId;
use crate::spec::value;
use crate::string::StringId;

/// Route group representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone)]
pub struct Route<'a> {
    /// Id of the route
    pub id: RouteId,
    /// Route path at this level of the hierarchy.
    pub path: Path<'a>,
    /// Doc comments
    pub docs: Option<StringId>,

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
    pub name: StringId,

    /// Path parameter type.
    pub path: Option<TypeId>,
    /// Query parameter type.
    pub query: Option<TypeId>,
    /// Headers type.
    pub headers: Option<TypeId>,

    /// MIME types the endpoint accepts for the request body.
    pub accept: Option<value::MimeTypesId>,
    /// Request body type.
    pub request_body: Option<TypeId>,
    /// Additional documentation for request body.
    pub request_body_docs: Option<StringId>,
}

/// Endpoint representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone)]
pub struct Endpoint<'a> {
    /// Id of the endpoint
    pub id: EndpointId,
    /// Path of the endpoint.
    pub path: Path<'a>,
    /// Doc comments
    pub docs: Option<StringId>,
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
                    let id = ResponseId(self.current);
                    self.current += 1;
                    return Some(Response { id, data: *resp });
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

/// Core data used to describe a response
#[derive(Debug, Clone, Copy)]
pub struct ResponseData {
    /// Status code of the response.
    pub status: ResponseStatus,
    /// Response body type.
    pub type_id: TypeId,
    /// Doc comments
    pub docs: Option<StringId>,
}

/// Response definition
#[derive(Debug, Clone, Copy)]
pub struct Response {
    /// Id of the response
    pub id: ResponseId,

    /// Actual data of the response
    pub data: ResponseData,
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
    fn new(
        parent: Parent<'p>,
        path: PathPart,
        fields: EndpointFields,
        docs: Option<StringId>,
    ) -> Self {
        let path_id = parent.paths.insert(parent.path_id, path);

        let idx = parent.data.len();
        parent.data.push(Slot::Endpoint {
            path: path_id,
            fields,
            docs,
            end: 0,
        });

        Self {
            parent,
            start: EndpointId(idx as u32),
            finished: false,
        }
    }

    /// Add a response to this endpoint.
    pub fn add_response(&mut self, resp: ResponseData) -> ResponseId {
        let id = self.parent.data.len();
        self.parent.data.push(Slot::Response(resp));
        ResponseId(id as u32)
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
    fn new(parent: Parent<'p>, path: PathPart, docs: Option<StringId>) -> Self {
        let path_id = parent.paths.insert(parent.path_id, path);

        let idx = parent.data.len();
        parent.data.push(Slot::Route {
            path: path_id,
            docs,
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
    pub fn add_response(&mut self, resp: ResponseData) -> ResponseId {
        let id = self.parent.data.len();
        self.parent.data.push(Slot::Response(resp));
        ResponseId(id as u32)
    }

    /// Start building an endpoint under this route.
    pub fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        fields: EndpointFields,
        docs: Option<StringId>,
    ) -> EndpointBuilder<'a> {
        EndpointBuilder::new(self.as_parent(), path, fields, docs)
    }

    /// Starts building a nested route group under this route.
    pub fn add_route<'a>(&'a mut self, path: PathPart, docs: Option<StringId>) -> RouteBuilder<'a> {
        RouteBuilder::new(self.as_parent(), path, docs)
    }

    /// Finalize the route and return its [`RouteId`].
    pub fn finish(mut self) -> RouteId {
        self.finished = true;

        let idx = self.parent.data.len();

        let start = self.start.0 as usize;
        let head = &mut self.parent.data[start];
        match head {
            Slot::Route { end, .. } => *end = idx as u32,
            _ => unreachable!("invalid RouteBuilder start"),
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
        docs: Option<StringId>,

        /// Id after the last child of the route.
        /// Used for knowing when to stop iteration
        end: u32,
    },
    Endpoint {
        path: PathId,
        docs: Option<StringId>,
        fields: EndpointFields,

        /// Id after the last response of the endpoint.
        /// Used for knowing when to stop iteration
        end: u32,
    },

    Response(ResponseData),
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
        docs: Option<StringId>,
    ) -> EndpointBuilder<'a> {
        EndpointBuilder::new(self.parent(), path, fields, docs)
    }

    /// Start building a route at the root.
    pub fn add_route<'a>(&'a mut self, path: PathPart, docs: Option<StringId>) -> RouteBuilder<'a> {
        RouteBuilder::new(self.parent(), path, docs)
    }

    /// Get [`Endpoint`] by id.
    pub fn get_endpoint<'a>(&'a self, id: EndpointId) -> Endpoint<'a> {
        match &self.data[id.0 as usize] {
            Slot::Endpoint {
                path,
                fields,
                docs,
                end,
            } => {
                let path = self.paths.get(*path);

                Endpoint {
                    id,
                    arena: self,
                    path,
                    fields,
                    docs: *docs,
                    end: *end,
                }
            }
            _ => unreachable!("slot at endpoint id should contain an endpoint"),
        }
    }

    /// Get [`Route`] by id.
    pub fn get_route<'a>(&'a self, id: RouteId) -> Route<'a> {
        match &self.data[id.0 as usize] {
            Slot::Route { path, docs, end } => {
                let path = self.paths.get(*path);

                Route {
                    id,
                    path,
                    docs: *docs,
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
            Slot::Response(resp) => Response { id, data: *resp },
            _ => unreachable!("slot at response id should contain a response"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{EndpointArena, EndpointFields, ResponseData, ResponseStatus};

    use crate::path::PathPart;
    use crate::spec::typ::{PrimitiveTy, TypeArena};
    use crate::string::StringInterner;

    use http::{Method, StatusCode};

    #[test]
    fn builds_endpoint_arena() {
        let mut types = TypeArena::new();
        let string_type = types.add_primitive(PrimitiveTy::String);
        let bool_type = types.add_primitive(PrimitiveTy::Bool);
        let i64_type = types.add_primitive(PrimitiveTy::I64);

        let mut interner = StringInterner::default();
        let status_name = interner.get_or_intern("status");
        let list_name = interner.get_or_intern("list");
        let create_name = interner.get_or_intern("create");
        let admin_name = interner.get_or_intern("admin");

        let mut arena = EndpointArena::new();

        // route {
        //   on default: bool
        //
        //   on 404:
        //
        //   /// status endpoint
        //   GET "/status" {
        //     name: "status"
        //
        //     on 200: string
        //     on 400:
        //   }
        //
        //   route "/users" {
        //     on default:
        //
        //     /// list endpoint
        //     GET "/list" {
        //       name: list
        //
        //       on 200: string
        //     }
        //
        //     /// create endpoint
        //     POST "/create" {
        //       name: create
        //
        //       on 201: bool
        //     }
        //
        //     route "/admin" {
        //       DELETE "/ban" {
        //         on 404:
        //       }
        //     }
        //   }
        //
        // }

        let mut root = arena.add_route(PathPart::Empty, None);
        root.add_response(ResponseData {
            status: ResponseStatus::Default,
            type_id: bool_type,
            docs: None,
        });
        let root_not_found_id = root.add_response(ResponseData {
            status: ResponseStatus::Code(StatusCode::NOT_FOUND),
            type_id: string_type,
            docs: None,
        });

        let mut status_endpoint = root.add_endpoint(
            PathPart::Segment("/status"),
            EndpointFields {
                method: Method::GET,
                name: status_name,
                path: None,
                query: Some(i64_type),
                headers: None,
                accept: None,
                request_body: None,
                request_body_docs: None,
            },
            None,
        );
        status_endpoint.add_response(ResponseData {
            status: ResponseStatus::Code(StatusCode::OK),
            type_id: string_type,
            docs: None,
        });
        status_endpoint.add_response(ResponseData {
            status: ResponseStatus::Code(StatusCode::BAD_REQUEST),
            type_id: string_type,
            docs: None,
        });
        let status_endpoint_id = status_endpoint.finish();

        let mut users_route = root.add_route(PathPart::Segment("/users"), None);
        users_route.add_response(ResponseData {
            status: ResponseStatus::Default,
            type_id: string_type,
            docs: None,
        });

        let mut list_endpoint = users_route.add_endpoint(
            PathPart::Segment("/list"),
            EndpointFields {
                method: Method::GET,
                name: list_name,
                path: None,
                query: None,
                headers: None,
                accept: None,
                request_body: None,
                request_body_docs: None,
            },
            None,
        );
        list_endpoint.add_response(ResponseData {
            status: ResponseStatus::Code(StatusCode::OK),
            type_id: string_type,
            docs: None,
        });
        let list_endpoint_id = list_endpoint.finish();

        let mut create_endpoint = users_route.add_endpoint(
            PathPart::Segment("/create"),
            EndpointFields {
                method: Method::POST,
                name: create_name,
                path: None,
                query: None,
                headers: None,
                accept: None,
                request_body: Some(string_type),
                request_body_docs: None,
            },
            None,
        );
        create_endpoint.add_response(ResponseData {
            status: ResponseStatus::Code(StatusCode::CREATED),
            type_id: bool_type,
            docs: None,
        });
        let create_endpoint_id = create_endpoint.finish();

        let mut admin_route = users_route.add_route(PathPart::Segment("/admin"), None);
        admin_route.add_response(ResponseData {
            status: ResponseStatus::Code(StatusCode::UNAUTHORIZED),
            type_id: string_type,
            docs: None,
        });

        let mut admin_endpoint = admin_route.add_endpoint(
            PathPart::Segment("/ban"),
            EndpointFields {
                method: Method::DELETE,
                name: admin_name,
                path: Some(i64_type),
                query: None,
                headers: None,
                accept: None,
                request_body: None,
                request_body_docs: None,
            },
            None,
        );
        let admin_no_content_id = admin_endpoint.add_response(ResponseData {
            status: ResponseStatus::Code(StatusCode::NO_CONTENT),
            type_id: string_type,
            docs: None,
        });
        let admin_endpoint_id = admin_endpoint.finish();

        let admin_route_id = admin_route.finish();
        let users_route_id = users_route.finish();
        let root_route_id = root.finish();

        // Check root route
        let root_route = arena.get_route(root_route_id);
        assert_eq!(root_route.path.part(), PathPart::Empty);
        let root_response_statuses: Vec<_> = root_route
            .responses()
            .map(|resp| resp.data.status)
            .collect();
        assert_eq!(
            root_response_statuses,
            vec![
                ResponseStatus::Default,
                ResponseStatus::Code(StatusCode::NOT_FOUND),
            ]
        );

        // Check root endpoints
        let root_endpoints: Vec<_> = root_route.endpoints().map(|endpoint| endpoint.id).collect();
        assert_eq!(root_endpoints, vec![status_endpoint_id]);
        // Check root sub-routes
        let root_routes: Vec<_> = root_route.routes().map(|route| route.id).collect();
        assert_eq!(root_routes, vec![users_route_id]);

        // Status endpoint
        let status_endpoint = arena.get_endpoint(status_endpoint_id);
        assert_eq!(status_endpoint.fields.method, Method::GET);
        assert_eq!(status_endpoint.fields.name, status_name);
        assert_eq!(status_endpoint.path.segments().as_slice(), &["/status"]);
        let status_responses: Vec<_> = status_endpoint
            .responses()
            .map(|resp| resp.data.status)
            .collect();
        assert_eq!(
            status_responses,
            vec![
                ResponseStatus::Code(StatusCode::OK),
                ResponseStatus::Code(StatusCode::BAD_REQUEST),
            ]
        );

        // Users route
        let users_route = arena.get_route(users_route_id);
        assert_eq!(users_route.path.segments().as_slice(), &["/users"]);
        // Users route responses
        let users_responses: Vec<_> = users_route
            .responses()
            .map(|resp| resp.data.status)
            .collect();
        assert_eq!(users_responses, vec![ResponseStatus::Default]);

        // Users route endpoints
        let users_endpoints: Vec<_> = users_route
            .endpoints()
            .map(|endpoint| endpoint.id)
            .collect();
        assert_eq!(users_endpoints, vec![list_endpoint_id, create_endpoint_id]);

        // User route sub-routes
        let users_routes: Vec<_> = users_route.routes().map(|route| route.id).collect();
        assert_eq!(users_routes, vec![admin_route_id]);

        // Users list endpoint
        let list_endpoint = arena.get_endpoint(list_endpoint_id);
        assert_eq!(
            list_endpoint.path.segments().as_slice(),
            &["/users", "/list"]
        );
        let list_responses: Vec<_> = list_endpoint
            .responses()
            .map(|resp| resp.data.status)
            .collect();
        assert_eq!(list_responses, vec![ResponseStatus::Code(StatusCode::OK)]);

        // Users create endpoint
        let create_endpoint = arena.get_endpoint(create_endpoint_id);
        assert_eq!(
            create_endpoint.path.segments().as_slice(),
            &["/users", "/create"]
        );
        assert_eq!(create_endpoint.fields.request_body, Some(string_type));
        let create_responses: Vec<_> = create_endpoint
            .responses()
            .map(|resp| resp.data.status)
            .collect();
        assert_eq!(
            create_responses,
            vec![ResponseStatus::Code(StatusCode::CREATED)]
        );

        // Users admin route
        let admin_route = arena.get_route(admin_route_id);
        assert_eq!(
            admin_route.path.segments().as_slice(),
            &["/users", "/admin"]
        );
        assert_eq!(admin_route.routes().count(), 0);
        // Admin route responses
        let admin_route_responses: Vec<_> = admin_route
            .responses()
            .map(|resp| resp.data.status)
            .collect();
        assert_eq!(
            admin_route_responses,
            vec![ResponseStatus::Code(StatusCode::UNAUTHORIZED)]
        );
        // Admin route endpoints
        let admin_endpoints: Vec<_> = admin_route
            .endpoints()
            .map(|endpoint| endpoint.id)
            .collect();
        assert_eq!(admin_endpoints, vec![admin_endpoint_id]);

        // Admin ban endpoint
        let admin_endpoint = arena.get_endpoint(admin_endpoint_id);
        assert_eq!(
            admin_endpoint.path.segments().as_slice(),
            &["/users", "/admin", "/ban"]
        );
        assert_eq!(admin_endpoint.fields.path, Some(i64_type));

        // Root route 404 response
        let root_not_found = arena.get_response(root_not_found_id);
        assert_eq!(root_not_found.id, root_not_found_id);
        assert_eq!(
            root_not_found.data.status,
            ResponseStatus::Code(StatusCode::NOT_FOUND)
        );

        // Admin ban 404 response
        let admin_no_content = arena.get_response(admin_no_content_id);
        assert_eq!(admin_no_content.id, admin_no_content_id);
        assert_eq!(
            admin_no_content.data.status,
            ResponseStatus::Code(StatusCode::NO_CONTENT)
        );
    }
}
