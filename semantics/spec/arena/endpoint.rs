//! Defines semantic representation of endpoints and routes.
//!
//! ## Querying
//!
//! Querying starts from [`Spec`](crate::spec::Spec) via
//! [`Spec::root_routes`](crate::spec::Spec::root_routes) and
//! [`Spec::root_endpoints`](crate::spec::Spec::root_endpoints). The higher-level
//! wrappers used during traversal are [`RouteView`](crate::spec::view::endpoint::RouteView)
//! and [`EndpointView`](crate::spec::view::endpoint::EndpointView).
//!
//! ## Construction
//!
//! Construction is handled by [`Analyzer`](crate::analyzer::Analyzer). It builds the internal
//! arenas and performs validation before data is exposed through [`Spec`](crate::spec::Spec).

use crate::mime::MimeRangeId;
use crate::path::{Path, PathArena, PathId, PathPart};
use crate::spec::arena::typ::id::{
    InlineTypeId, ResponseReferenceProof, SimpleRecordReferenceProof,
};
use crate::text::{NameId, StringId};

/// Status definition for a response.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResponseStatus {
    /// Default response for a route or endpoint.
    Default,
    /// Response for a specific HTTP status code.
    Code(http::StatusCode),
}

impl std::fmt::Display for ResponseStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Default => f.write_str("Default"),
            Self::Code(code) => code.as_u16().fmt(f),
        }
    }
}

/// Id of an endpoint stored in the [`EndpointArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct EndpointId(u32);

/// Id of a route stored in the [`EndpointArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RouteId(u32);

/// Id of a response stored in the [`EndpointArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct EndpointResponseId(u32);

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
pub(crate) struct EndpointBuilder<'p> {
    parent: Parent<'p>,

    start: EndpointId,

    finished: bool,
}

impl<'p> EndpointBuilder<'p> {
    fn new(parent: Parent<'p>, path: PathPart, data: EndpointFields) -> (Self, PathId) {
        let path_id = parent.paths.insert(parent.path_id, path);

        let idx = parent.data.len();
        parent.data.push(Slot::Endpoint {
            path: path_id,
            fields: data,
            end: 0,
        });

        let builder = Self {
            parent,
            start: EndpointId(idx as u32),
            finished: false,
        };
        (builder, path_id)
    }

    /// Add a response to this endpoint.
    pub(crate) fn add_response(
        &mut self,
        status: ResponseStatus,
        type_id: EndpointResponseTypeId,
        docs: Option<StringId>,
    ) -> EndpointResponseId {
        let id = self.parent.data.len();
        self.parent.data.push(Slot::Response {
            status,
            type_id,
            docs,
        });
        EndpointResponseId(id as u32)
    }

    /// Finalize the endpoint and return its [`EndpointId`].
    pub(crate) fn finish(mut self) -> EndpointId {
        self.finished = true;

        let idx = self.parent.data.len();

        let start = self.start.0 as usize;
        let head = &mut self.parent.data[start];
        match head {
            Slot::Endpoint { end, .. } => *end = idx as u32,
            _ => unreachable!("EndpointBuilder::start must point to Slot::Endpoint"),
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

/// Helper type for adding nested routes, endpoints, and responses to a route.
///
/// Constructed via [`EndpointArena::add_route`] or [`RouteBuilder::add_route`]. Call
/// [`finish`](RouteBuilder::finish) once all nested entries are added.
/// Dropping the builder without finishing rolls back any changes.
pub(crate) struct RouteBuilder<'p> {
    parent: Parent<'p>,

    /// Path id of the whole route of this path. Used as
    /// prefix of children routes.
    path_id: PathId,

    start: RouteId,

    finished: bool,
}

impl<'p> RouteBuilder<'p> {
    fn new(parent: Parent<'p>, path: PathPart, docs: Option<StringId>) -> (Self, PathId) {
        let path_id = parent.paths.insert(parent.path_id, path);

        let idx = parent.data.len();
        parent.data.push(Slot::Route {
            path: path_id,
            docs,
            end: 0,
        });

        let builder = Self {
            parent,
            path_id,
            start: RouteId(idx as u32),
            finished: false,
        };
        (builder, path_id)
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
    pub(crate) fn add_response(
        &mut self,
        status: ResponseStatus,
        type_id: EndpointResponseTypeId,
        docs: Option<StringId>,
    ) -> EndpointResponseId {
        let id = self.parent.data.len();
        self.parent.data.push(Slot::Response {
            status,
            type_id,
            docs,
        });
        EndpointResponseId(id as u32)
    }

    /// Start building an endpoint under this route.
    pub(crate) fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        data: EndpointFields,
    ) -> (EndpointBuilder<'a>, PathId) {
        EndpointBuilder::new(self.as_parent(), path, data)
    }

    /// Starts building a nested route under this route.
    pub(crate) fn add_route<'a>(
        &'a mut self,
        path: PathPart,
        docs: Option<StringId>,
    ) -> (RouteBuilder<'a>, PathId) {
        RouteBuilder::new(self.as_parent(), path, docs)
    }

    /// Finalize the route and return its [`RouteId`].
    pub(crate) fn finish(mut self) -> RouteId {
        self.finished = true;

        let idx = self.parent.data.len();

        let start = self.start.0 as usize;
        let head = &mut self.parent.data[start];
        match head {
            Slot::Route { end, .. } => *end = idx as u32,
            _ => unreachable!("RouteBuilder::start must point to Slot::Route"),
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

/// Core fields used to describe an endpoint.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct EndpointFields {
    /// Documentation
    pub docs: Option<StringId>,

    /// HTTP method used by the endpoint.
    pub method: http::Method,

    /// Name assigned to the endpoint.
    pub name: NameId,

    /// Path parameter type.
    pub path_param: Option<SimpleRecordReferenceProof>,

    /// Query parameter type.
    pub query: Option<SimpleRecordReferenceProof>,

    /// Headers type.
    pub headers: Option<SimpleRecordReferenceProof>,

    /// MIME types the endpoint accepts for the request body.
    pub accept: Option<MimeRangeId>,

    /// Request body type.
    pub request_body: Option<InlineTypeId>,
    /// Additional documentation for request body.
    pub request_body_docs: Option<StringId>,
}

/// Id of the type used in [`EndpointResponse`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum EndpointResponseTypeId {
    Response(ResponseReferenceProof),
    InlineType(InlineTypeId),
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
        /// Path of the endpoint
        path: PathId,

        fields: EndpointFields,

        /// Id after the last response of the endpoint.
        /// Used for knowing when to stop iteration
        end: u32,
    },

    Response {
        status: ResponseStatus,
        type_id: EndpointResponseTypeId,
        docs: Option<StringId>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct RouteData {
    pub path: PathId,
    pub docs: Option<StringId>,

    /// Index of the first child slot after the route slot itself.
    first: u32,
    end: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct EndpointData {
    pub(crate) id: EndpointId,

    pub path: PathId,
    pub fields: EndpointFields,

    /// Index of the first response slot after the endpoint slot itself.
    first: u32,
    end: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ResponseData {
    pub status: ResponseStatus,
    pub typ: EndpointResponseTypeId,
    pub docs: Option<StringId>,
}

/// Cursor for iterating over child routes inside a route.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RouteCursor {
    next: u32,
    end: u32,
}

/// Cursor for iterating over endpoints inside a route.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EndpointCursor {
    next: u32,
    end: u32,
}

/// Cursor for iterating over responses inside a route or endpoint.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ResponseCursor {
    next: u32,
    end: u32,
}

/// Arena that holds semantic endpoints and routes.
#[derive(Debug, Clone, Default)]
pub struct EndpointArena {
    data: Vec<Slot>,
    paths: PathArena,
}

impl EndpointArena {
    fn parent<'a>(&'a mut self) -> Parent<'a> {
        Parent {
            data: &mut self.data,
            paths: &mut self.paths,
            path_id: None,
        }
    }

    /// Start building an endpoint at the root.
    pub(crate) fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        fields: EndpointFields,
    ) -> (EndpointBuilder<'a>, PathId) {
        EndpointBuilder::new(self.parent(), path, fields)
    }

    /// Start building a route at the root.
    pub(crate) fn add_route<'a>(
        &'a mut self,
        path: PathPart,
        docs: Option<StringId>,
    ) -> (RouteBuilder<'a>, PathId) {
        RouteBuilder::new(self.parent(), path, docs)
    }

    /// Returns the path stored for `id`.
    pub(crate) fn get_path<'a>(&'a self, id: PathId) -> Path<'a> {
        self.paths.get(id)
    }

    /// Returns endpoint data for `id`.
    pub(crate) fn get_endpoint(&self, id: EndpointId) -> EndpointData {
        let Slot::Endpoint { path, fields, end } = &self.data[id.0 as usize] else {
            unreachable!("EndpointId must point to Slot::Endpoint");
        };

        EndpointData {
            id,
            path: *path,
            fields: fields.clone(),
            first: id.0 + 1,
            end: *end,
        }
    }

    /// Returns route data for `id`.
    pub(crate) fn get_route(&self, id: RouteId) -> RouteData {
        let slot = &self.data[id.0 as usize];
        let Slot::Route { path, docs, end } = slot else {
            unreachable!("RouteId must point to Slot::Route");
        };

        RouteData {
            path: *path,
            docs: *docs,
            first: id.0 + 1,
            end: *end,
        }
    }

    /// Returns response data for `id`.
    pub(crate) fn get_response(&self, id: EndpointResponseId) -> ResponseData {
        let Slot::Response {
            status,
            type_id,
            docs,
        } = &self.data[id.0 as usize]
        else {
            unreachable!("EndpointResponseId must point to Slot::Response");
        };

        ResponseData {
            status: *status,
            typ: *type_id,
            docs: *docs,
        }
    }

    /// Returns a cursor over child routes of `route`.
    pub(crate) fn route_cursor(&self, route: RouteData) -> RouteCursor {
        RouteCursor {
            next: route.first,
            end: route.end,
        }
    }

    /// Returns a cursor over root-level routes.
    pub(crate) fn root_route_cursor(&self) -> RouteCursor {
        RouteCursor {
            next: 0,
            end: self.data.len() as u32,
        }
    }

    /// Returns a cursor over root-level endpoints.
    pub(crate) fn root_endpoint_cursor(&self) -> EndpointCursor {
        EndpointCursor {
            next: 0,
            end: self.data.len() as u32,
        }
    }

    /// Returns a cursor over endpoints inside `route`.
    pub(crate) fn endpoint_cursor(&self, route: RouteData) -> EndpointCursor {
        EndpointCursor {
            next: route.first,
            end: route.end,
        }
    }

    /// Returns a cursor over responses defined on `route`.
    pub(crate) fn route_response_cursor(&self, route: RouteData) -> ResponseCursor {
        ResponseCursor {
            next: route.first,
            end: route.end,
        }
    }

    /// Returns a cursor over responses defined on `endpoint`.
    pub(crate) fn endpoint_response_cursor(&self, endpoint: &EndpointData) -> ResponseCursor {
        ResponseCursor {
            next: endpoint.first,
            end: endpoint.end,
        }
    }

    /// Get next route.
    ///
    /// If cursor is pointing to the end of the route, None is returned.
    pub(crate) fn next_route(&self, c: RouteCursor) -> Option<(RouteData, RouteCursor)> {
        let mut current = c.next;
        while current < c.end {
            let slot = &self.data[current as usize];
            match slot {
                Slot::Endpoint { end, .. } => {
                    debug_assert!(*end > current && *end <= c.end);
                    current = *end;
                }
                Slot::Response { .. } => current += 1,
                Slot::Route { path, docs, end } => {
                    debug_assert!(*end > current && *end <= c.end);
                    let next = RouteCursor {
                        next: *end,
                        end: c.end,
                    };
                    let data = RouteData {
                        path: *path,
                        docs: *docs,
                        first: current + 1,
                        end: *end,
                    };

                    return Some((data, next));
                }
            }
        }

        None
    }

    /// Get next endpoint.
    ///
    /// If cursor is pointing to the end, None is returned.
    pub(crate) fn next_endpoint(
        &self,
        c: EndpointCursor,
    ) -> Option<(EndpointData, EndpointCursor)> {
        let mut current = c.next;
        while current < c.end {
            let slot = &self.data[current as usize];
            match slot {
                Slot::Route { end, .. } => {
                    debug_assert!(*end > current && *end <= c.end);
                    current = *end;
                }
                Slot::Response { .. } => current += 1,
                Slot::Endpoint { path, fields, end } => {
                    debug_assert!(*end > current && *end <= c.end);
                    let next = EndpointCursor {
                        next: *end,
                        end: c.end,
                    };
                    let data = EndpointData {
                        id: EndpointId(current),
                        path: *path,
                        fields: fields.clone(),
                        first: current + 1,
                        end: *end,
                    };

                    return Some((data, next));
                }
            }
        }

        None
    }

    /// Get next response.
    ///
    /// If cursor is pointing to the end, None is returned.
    pub(crate) fn next_response(
        &self,
        c: ResponseCursor,
    ) -> Option<(ResponseData, ResponseCursor)> {
        let mut current = c.next;
        while current < c.end {
            let slot = &self.data[current as usize];
            match slot {
                Slot::Route { end, .. } | Slot::Endpoint { end, .. } => {
                    debug_assert!(*end > current && *end <= c.end);
                    current = *end;
                }
                Slot::Response {
                    status,
                    type_id,
                    docs,
                } => {
                    let next = ResponseCursor {
                        next: current + 1,
                        end: c.end,
                    };
                    let data = ResponseData {
                        status: *status,
                        typ: *type_id,
                        docs: *docs,
                    };

                    return Some((data, next));
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::{EndpointArena, EndpointFields, EndpointResponseTypeId, ResponseStatus};
    use crate::path::PathPart;
    use crate::spec::Spec;
    use crate::spec::arena::typ::PrimitiveTy;
    use crate::spec::arena::typ::builder::TypeArenaBuilder;
    use crate::spec::arena::typ::id::InlineTypeId;
    use crate::text::{NameId, StringId};
    use http::{Method, StatusCode};

    fn endpoint_fields(name: NameId, method: Method, docs: Option<StringId>) -> EndpointFields {
        EndpointFields {
            docs,
            method,
            name,
            path_param: None,
            query: None,
            headers: None,
            accept: None,
            request_body: None,
            request_body_docs: None,
        }
    }

    fn primitive_types(spec: &mut Spec) -> (InlineTypeId, InlineTypeId) {
        let mut builder = TypeArenaBuilder::default();
        let string_id = builder.add_primitive(PrimitiveTy::String);
        let bool_id = builder.add_primitive(PrimitiveTy::Bool);
        spec.types = builder
            .finish(&spec.symbol_table)
            .expect("test type arena should build");
        (string_id, bool_id)
    }

    #[test]
    fn builds_endpoints_and_walks_root_and_nested_cursors() {
        let mut spec = Spec::new_test();
        let (string_id, bool_id) = primitive_types(&mut spec);

        let route_docs = spec.strings.get_or_intern("api docs");
        let users_docs = spec.strings.get_or_intern("users docs");

        let health_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("health")) };
        let users_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("users")) };
        let stats_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("stats")) };
        let dropped_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("dropped")) };

        let mut arena = EndpointArena::default();

        let (mut health, health_path) = arena.add_endpoint(
            PathPart::Segment("/health"),
            endpoint_fields(health_name, Method::GET, None),
        );
        let health_response_id = health.add_response(
            ResponseStatus::Code(StatusCode::OK),
            EndpointResponseTypeId::InlineType(string_id),
            None,
        );
        let health_id = health.finish();

        let (mut api, api_path) = arena.add_route(PathPart::Segment("/api"), Some(route_docs));
        let api_response_id = api.add_response(
            ResponseStatus::Default,
            EndpointResponseTypeId::InlineType(bool_id),
            None,
        );

        {
            let (mut dropped, _) = api.add_endpoint(
                PathPart::Segment("/dropped"),
                endpoint_fields(dropped_name, Method::DELETE, None),
            );
            dropped.add_response(
                ResponseStatus::Code(StatusCode::NO_CONTENT),
                EndpointResponseTypeId::InlineType(string_id),
                None,
            );
        }

        let (mut users, users_path) = api.add_endpoint(
            PathPart::Segment("/users"),
            endpoint_fields(users_name, Method::POST, Some(users_docs)),
        );
        let users_response_id = users.add_response(
            ResponseStatus::Code(StatusCode::CREATED),
            EndpointResponseTypeId::InlineType(bool_id),
            None,
        );
        let users_id = users.finish();

        let (mut admin, admin_path) = api.add_route(PathPart::Segment("/admin"), None);
        let (mut stats, stats_path) = admin.add_endpoint(
            PathPart::Segment("/stats"),
            endpoint_fields(stats_name, Method::GET, None),
        );
        let stats_response_id = stats.add_response(
            ResponseStatus::Code(StatusCode::OK),
            EndpointResponseTypeId::InlineType(string_id),
            None,
        );
        let stats_id = stats.finish();
        let admin_id = admin.finish();
        let api_id = api.finish();

        assert_eq!(
            arena.get_path(health_path).segments().as_slice(),
            &["/health"]
        );
        assert_eq!(arena.get_path(api_path).segments().as_slice(), &["/api"]);
        assert_eq!(
            arena.get_path(users_path).segments().as_slice(),
            &["/api", "/users"]
        );
        assert_eq!(
            arena.get_path(admin_path).segments().as_slice(),
            &["/api", "/admin"]
        );
        assert_eq!(
            arena.get_path(stats_path).segments().as_slice(),
            &["/api", "/admin", "/stats"]
        );

        let (root_route, next_root_route) = arena
            .next_route(arena.root_route_cursor())
            .expect("root route");
        assert_eq!(root_route, arena.get_route(api_id));
        assert_eq!(
            arena.get_path(root_route.path).segments().as_slice(),
            &["/api"]
        );
        assert_eq!(root_route.docs, Some(route_docs));
        assert!(arena.next_route(next_root_route).is_none());

        let (root_endpoint, next_root_endpoint) = arena
            .next_endpoint(arena.root_endpoint_cursor())
            .expect("root endpoint");
        assert_eq!(root_endpoint, arena.get_endpoint(health_id));
        assert_eq!(
            arena.get_path(root_endpoint.path).segments().as_slice(),
            &["/health"]
        );
        assert_eq!(root_endpoint.fields.method, Method::GET);
        assert!(arena.next_endpoint(next_root_endpoint).is_none());

        let (route_response, route_response_cursor) = arena
            .next_response(arena.route_response_cursor(root_route))
            .expect("route response");
        assert_eq!(route_response.status, ResponseStatus::Default);
        assert_eq!(
            route_response.typ,
            EndpointResponseTypeId::InlineType(bool_id)
        );
        assert_eq!(route_response, arena.get_response(api_response_id));
        assert!(arena.next_response(route_response_cursor).is_none());

        let (users, next_users) = arena
            .next_endpoint(arena.endpoint_cursor(root_route))
            .expect("users endpoint");
        assert_eq!(users, arena.get_endpoint(users_id));
        assert_eq!(
            arena.get_path(users.path).segments().as_slice(),
            &["/api", "/users"]
        );
        assert_eq!(users.fields.docs, Some(users_docs));
        assert_eq!(users.fields.method, Method::POST);
        assert!(arena.next_endpoint(next_users).is_none());

        let (users_response, users_response_cursor) = arena
            .next_response(arena.endpoint_response_cursor(&users))
            .expect("users response");
        assert_eq!(users_response, arena.get_response(users_response_id));
        assert_eq!(
            users_response.typ,
            EndpointResponseTypeId::InlineType(bool_id)
        );
        assert!(arena.next_response(users_response_cursor).is_none());

        let (admin, next_admin) = arena
            .next_route(arena.route_cursor(root_route))
            .expect("admin route");
        assert_eq!(admin, arena.get_route(admin_id));
        assert_eq!(
            arena.get_path(admin.path).segments().as_slice(),
            &["/api", "/admin"]
        );
        assert!(arena.next_route(next_admin).is_none());

        let (stats, next_stats) = arena
            .next_endpoint(arena.endpoint_cursor(admin))
            .expect("stats endpoint");
        assert_eq!(stats, arena.get_endpoint(stats_id));
        assert_eq!(
            arena.get_path(stats.path).segments().as_slice(),
            &["/api", "/admin", "/stats"]
        );
        assert!(arena.next_endpoint(next_stats).is_none());

        let (health_response, health_response_cursor) = arena
            .next_response(arena.endpoint_response_cursor(&root_endpoint))
            .expect("health response");
        assert_eq!(health_response, arena.get_response(health_response_id));
        assert_eq!(
            health_response.typ,
            EndpointResponseTypeId::InlineType(string_id)
        );
        assert!(arena.next_response(health_response_cursor).is_none());

        let (stats_response, stats_response_cursor) = arena
            .next_response(arena.endpoint_response_cursor(&stats))
            .expect("stats response");
        assert_eq!(stats_response, arena.get_response(stats_response_id));
        assert_eq!(
            stats_response.typ,
            EndpointResponseTypeId::InlineType(string_id)
        );
        assert!(arena.next_response(stats_response_cursor).is_none());
    }
}
