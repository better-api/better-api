//! Defines semantic representation of endpoints and route groups.
//!
//! ## Querying
//!
//! Querying starts from [`Spec`](crate::spec::Spec). [`Route`] and [`Endpoint`]
//! provide iterators over nested routes, endpoints, and responses.
//!
//! ## Construction
//!
//! Construction is handled by [`Analyzer`](crate::analyzer::Analyzer). It builds the internal
//! arenas and performs validation before data is exposed through [`Spec`](crate::spec::Spec).

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

/// Helper type for adding endpoints and responses to a route.
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

    /// Starts building a nested route group under this route.
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
    pub accept: Option<()>, // TODO: Mime types

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

    pub(crate) fn get_path<'a>(&'a self, id: PathId) -> Path<'a> {
        self.paths.get(id)
    }

    pub(crate) fn get_endpoint(&self, id: EndpointId) -> EndpointData {
        let Slot::Endpoint { path, fields, end } = &self.data[id.0 as usize] else {
            unreachable!("EndpointId must point to Slot::Endpoint");
        };

        EndpointData {
            path: *path,
            fields: fields.clone(),
            first: id.0 + 1,
            end: *end,
        }
    }

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

    pub(crate) fn route_cursor(&self, route: RouteData) -> RouteCursor {
        RouteCursor {
            next: route.first,
            end: route.end,
        }
    }

    pub(crate) fn endpoint_cursor(&self, route: RouteData) -> EndpointCursor {
        EndpointCursor {
            next: route.first,
            end: route.end,
        }
    }

    pub(crate) fn route_response_cursor(&self, route: RouteData) -> ResponseCursor {
        ResponseCursor {
            next: route.first,
            end: route.end,
        }
    }

    pub(crate) fn endpoint_response_cursor(&self, endpoint: &EndpointData) -> ResponseCursor {
        ResponseCursor {
            next: endpoint.first,
            end: endpoint.end,
        }
    }

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

// impl<'a> SpecContext<'a> {
//     /// Return iterator through root routes.
//     ///
//     /// Root routes are routes without a parent.
//     pub(crate) fn root_routes(&self) -> RouteIterator<'a> {
//         RouteIterator {
//             ctx: *self,
//             current: 0,
//             end: self.endpoints.data.len() as u32,
//         }
//     }
//
//     /// Return iterator through root endpoints.
//     ///
//     /// Root endpoints are endpoints without a parent.
//     pub(crate) fn root_endpoints(&self) -> EndpointIterator<'a> {
//         EndpointIterator {
//             ctx: *self,
//             current: 0,
//             end: self.endpoints.data.len() as u32,
//         }
//     }
//
//     /// Get endpoint or route [`Response`] by id.
//     pub(crate) fn get_endpoint_response(&self, id: EndpointResponseId) -> EndpointResponse<'a> {
//         let Slot::Response {
//             status,
//             type_id,
//             docs,
//         } = &self.endpoints.data[id.0 as usize]
//         else {
//             unreachable!("slot at response id should contain a response");
//         };
//
//         let typ = match type_id {
//             EndpointResponseTypeId::Response(id) => {
//                 EndpointResponseType::Response(self.get_response_reference(*id))
//             }
//             EndpointResponseTypeId::InlineType(id) => {
//                 EndpointResponseType::InlineType(self.get_inline_type(*id))
//             }
//         };
//
//         EndpointResponse {
//             status: *status,
//             typ,
//             docs: docs.map(|id| self.strings.resolve(id)),
//         }
//     }
// }

// #[cfg(test)]
// mod test {
//     use super::ResponseStatus;
//
//     use crate::path::PathPart;
//     use crate::spec::Spec;
//     use crate::spec::endpoint::{EndpointData, EndpointResponseTypeId};
//     use crate::spec::typ::{
//         InlineTy, InlineTyId, PrimitiveTy, RootRef, SimpleRecordReference, SimpleRecordReferenceId,
//         TypeDefData,
//     };
//     use crate::text::NameId;
//
//     use http::{Method, StatusCode};
//
//     // #[test]
//     // fn builds_endpoint_arena() {
//     //     Re-enable after the endpoint arena refactor is finished.
//     // }
// }
