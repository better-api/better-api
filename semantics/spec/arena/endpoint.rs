//! Defines semantic representation of endpoints and route groups.
//!
//! ## Querying
//!
//! Querying starts from [`Spec`](crate::spec::Spec). [`Route`] and [`Endpoint`]
//! provide iterators over nested routes, endpoints, and responses.
//!
//! ## Construction
//!
//! Construction is handled by [`Oracle`](crate::Oracle). It builds the internal
//! arenas and performs validation before data is exposed through `SpecContext`.

use crate::path::{Path, PathArena, PathId, PathPart};
use crate::spec::typ::{
    InlineTy, InlineTyId, NamedReference, ResponseReference, ResponseReferenceId,
    SimpleRecordReference, SimpleRecordReferenceId, Type,
};
use crate::spec::value::{self, MimeTypes, ValueContext};
use crate::spec::SpecContext;
use crate::text::{Name, NameId, StringId};

/// Route group representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone)]
pub struct Route<'a> {
    /// Id of the route
    pub(crate) id: RouteId,

    /// Route path at this level of the hierarchy.
    pub path: Path<'a>,

    /// Doc comments
    pub docs: Option<&'a str>,

    #[debug(skip)]
    ctx: SpecContext<'a>,

    /// End index of the route in the arena, used to know where to stop
    /// child iteration.
    end: u32,
}

impl<'a> Route<'a> {
    /// Returns an iterator over endpoints in this route group.
    pub fn endpoints(&self) -> EndpointIterator<'a> {
        EndpointIterator {
            ctx: self.ctx,
            current: self.id.0 + 1,
            end: self.end,
        }
    }

    /// Returns an iterator over routes in this route group.
    pub fn routes(&self) -> RouteIterator<'a> {
        RouteIterator {
            ctx: self.ctx,
            current: self.id.0 + 1,
            end: self.end,
        }
    }

    /// Returns an iterator over responses defined for this route group.
    pub fn responses(&self) -> ResponseIterator<'a> {
        ResponseIterator {
            ctx: self.ctx,
            current: self.id.0 + 1,
            end: self.end,
        }
    }
}

/// Endpoint representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone)]
pub struct Endpoint<'a> {
    /// Id of the endpoint
    pub(crate) id: EndpointId,

    /// Path of the endpoint.
    pub path: Path<'a>,

    /// Doc comments
    pub docs: Option<&'a str>,

    /// HTTP method used by the endpoint.
    pub method: http::Method,

    /// Name assigned to the endpoint.
    pub name: &'a Name,

    /// Path parameter type.
    pub path_param: Option<NamedReference<'a, SimpleRecordReference<'a>>>,
    /// Query parameter type.
    pub query: Option<NamedReference<'a, SimpleRecordReference<'a>>>,
    /// Headers type.
    pub headers: Option<NamedReference<'a, SimpleRecordReference<'a>>>,

    /// MIME types the endpoint accepts for the request body.
    pub accept: Option<MimeTypes<'a>>,

    /// Request body type.
    pub request_body: Option<InlineTy<'a, Type<'a>>>,
    /// Additional documentation for request body.
    pub request_body_docs: Option<&'a str>,

    #[debug(skip)]
    ctx: SpecContext<'a>,

    /// End index of the endpoint in the arena, used to know where to stop
    /// response iteration.
    end: u32,
}

impl<'a> Endpoint<'a> {
    /// Returns an iterator over responses for this endpoint.
    pub fn responses(&self) -> ResponseIterator<'a> {
        ResponseIterator {
            ctx: self.ctx,
            current: self.id.0 + 1,
            end: self.end,
        }
    }
}

/// Iterator over endpoints inside a [`Route`].
pub struct EndpointIterator<'a> {
    ctx: SpecContext<'a>,

    current: u32,
    end: u32,
}

impl<'a> Iterator for EndpointIterator<'a> {
    type Item = Endpoint<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current < self.end {
            match &self.ctx.endpoints.data[self.current as usize] {
                Slot::Route { end, .. } => self.current = *end,
                Slot::Response { .. } => self.current += 1,
                Slot::Endpoint { end, .. } => {
                    let id = EndpointId(self.current);
                    self.current = *end;
                    return Some(self.ctx.get_endpoint(id));
                }
            }
        }

        None
    }
}

/// Iterator over routes inside a [`Route`]
pub struct RouteIterator<'a> {
    ctx: SpecContext<'a>,

    current: u32,
    end: u32,
}

impl<'a> Iterator for RouteIterator<'a> {
    type Item = Route<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current < self.end {
            match &self.ctx.endpoints.data[self.current as usize] {
                Slot::Response { .. } => self.current += 1,
                Slot::Endpoint { end, .. } => self.current = *end,
                Slot::Route { end, .. } => {
                    let id = RouteId(self.current);
                    self.current = *end;
                    return Some(self.ctx.get_route(id));
                }
            }
        }

        None
    }
}

/// Iterator over responses inside a [`Route`] or [`Endpoint`]
pub struct ResponseIterator<'a> {
    ctx: SpecContext<'a>,

    current: u32,
    end: u32,
}

impl<'a> Iterator for ResponseIterator<'a> {
    type Item = EndpointResponse<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.current < self.end {
            match &self.ctx.endpoints.data[self.current as usize] {
                Slot::Endpoint { end, .. } => self.current = *end,
                Slot::Route { end, .. } => self.current = *end,
                Slot::Response { .. } => {
                    let id = EndpointResponseId(self.current);
                    self.current += 1;
                    return Some(self.ctx.get_endpoint_response(id));
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

impl std::fmt::Display for ResponseStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Default => f.write_str("Default"),
            Self::Code(code) => code.as_u16().fmt(f),
        }
    }
}

/// Type used in endpoint response  
#[derive(Debug, Clone)]
pub enum EndpointResponseType<'a> {
    Response(NamedReference<'a, ResponseReference<'a>>),
    InlineType(InlineTy<'a, Type<'a>>),
}

/// Response definition
#[derive(Debug, Clone)]
pub struct EndpointResponse<'a> {
    /// Status code of the response.
    pub status: ResponseStatus,

    /// Response body type.
    pub typ: EndpointResponseType<'a>,

    /// Doc comments
    pub docs: Option<&'a str>,
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
    fn new(parent: Parent<'p>, path: PathPart, data: EndpointData) -> (Self, PathId) {
        let path_id = parent.paths.insert(parent.path_id, path);

        let idx = parent.data.len();
        parent.data.push(Slot::Endpoint {
            path: path_id,
            data,
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
        data: EndpointData,
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

/// Core fields used to describe an endpoint.
#[derive(Clone, Debug)]
pub(crate) struct EndpointData {
    /// Documentation
    pub docs: Option<StringId>,

    /// HTTP method used by the endpoint.
    pub method: http::Method,

    /// Name assigned to the endpoint.
    pub name: NameId,

    /// Path parameter type.
    pub path_param: Option<SimpleRecordReferenceId>,

    /// Query parameter type.
    pub query: Option<SimpleRecordReferenceId>,

    /// Headers type.
    pub headers: Option<SimpleRecordReferenceId>,

    /// MIME types the endpoint accepts for the request body.
    pub accept: Option<value::MimeTypesId>,

    /// Request body type.
    pub request_body: Option<InlineTyId>,
    /// Additional documentation for request body.
    pub request_body_docs: Option<StringId>,
}

/// Id of the type used in [`EndpointResponse`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum EndpointResponseTypeId {
    Response(ResponseReferenceId),
    InlineType(InlineTyId),
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

        data: EndpointData,

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
        fields: EndpointData,
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
}

impl<'a> SpecContext<'a> {
    /// Get [`Endpoint`] by id.
    pub(crate) fn get_endpoint(&self, id: EndpointId) -> Endpoint<'a> {
        let Slot::Endpoint { path, data, end } = &self.endpoints.data[id.0 as usize] else {
            unreachable!("slot at endpoint id should contain an endpoint");
        };

        let accept = data.accept.map(|id| {
            let val_ctx: ValueContext = (*self).into();
            val_ctx.get_mime_types(id)
        });

        Endpoint {
            id,
            path: self.endpoints.paths.get(*path),
            docs: data.docs.map(|id| self.strings.resolve(id)),
            method: data.method.clone(),
            name: self.strings.resolve_name(data.name),
            path_param: data
                .path_param
                .map(|id| self.get_simple_record_reference(id)),
            query: data.query.map(|id| self.get_simple_record_reference(id)),
            headers: data.headers.map(|id| self.get_simple_record_reference(id)),
            accept,
            request_body: data.request_body.map(|id| self.get_inline_type(id)),
            request_body_docs: data.request_body_docs.map(|id| self.strings.resolve(id)),
            ctx: *self,
            end: *end,
        }
    }

    /// Get [`Route`] by id.
    pub(crate) fn get_route(&self, id: RouteId) -> Route<'a> {
        let Slot::Route { path, docs, end } = &self.endpoints.data[id.0 as usize] else {
            unreachable!("slot at route id should contain a route");
        };

        Route {
            id,
            path: self.endpoints.paths.get(*path),
            docs: docs.map(|id| self.strings.resolve(id)),
            ctx: *self,
            end: *end,
        }
    }

    /// Return iterator through root routes.
    ///
    /// Root routes are routes without a parent.
    pub(crate) fn root_routes(&self) -> RouteIterator<'a> {
        RouteIterator {
            ctx: *self,
            current: 0,
            end: self.endpoints.data.len() as u32,
        }
    }

    /// Return iterator through root endpoints.
    ///
    /// Root endpoints are endpoints without a parent.
    pub(crate) fn root_endpoints(&self) -> EndpointIterator<'a> {
        EndpointIterator {
            ctx: *self,
            current: 0,
            end: self.endpoints.data.len() as u32,
        }
    }

    /// Get endpoint or route [`Response`] by id.
    pub(crate) fn get_endpoint_response(&self, id: EndpointResponseId) -> EndpointResponse<'a> {
        let Slot::Response {
            status,
            type_id,
            docs,
        } = &self.endpoints.data[id.0 as usize]
        else {
            unreachable!("slot at response id should contain a response");
        };

        let typ = match type_id {
            EndpointResponseTypeId::Response(id) => {
                EndpointResponseType::Response(self.get_response_reference(*id))
            }
            EndpointResponseTypeId::InlineType(id) => {
                EndpointResponseType::InlineType(self.get_inline_type(*id))
            }
        };

        EndpointResponse {
            status: *status,
            typ,
            docs: docs.map(|id| self.strings.resolve(id)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::ResponseStatus;

    use crate::path::PathPart;
    use crate::spec::endpoint::{EndpointData, EndpointResponseTypeId};
    use crate::spec::typ::{
        InlineTy, InlineTyId, PrimitiveTy, RootRef, SimpleRecordReference, SimpleRecordReferenceId,
        TypeDefData,
    };
    use crate::spec::Spec;
    use crate::text::NameId;

    use http::{Method, StatusCode};

    // #[test]
    // fn builds_endpoint_arena() {
    //     Re-enable after the endpoint arena refactor is finished.
    // }
}
