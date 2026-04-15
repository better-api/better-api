use crate::path::Path;
use crate::spec::Spec;
use crate::spec::arena::endpoint::{
    EndpointCursor, EndpointData, EndpointResponseTypeId, ResponseCursor, ResponseData,
    ResponseStatus, RouteCursor, RouteData,
};
use crate::spec::view::typ::{InlineTyView, NamedRootTypeRefView, NamedTypeRefView};
use crate::text::Name;

/// Route group representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone)]
pub struct RouteView<'a> {
    /// Route path at this level of the hierarchy.
    pub path: Path<'a>,

    /// Doc comments
    pub docs: Option<&'a str>,

    #[debug(skip)]
    spec: &'a Spec,

    route_cursor: RouteCursor,
    endpoint_cursor: EndpointCursor,
    response_cursor: ResponseCursor,
}

impl<'a> RouteView<'a> {
    /// Returns an iterator over endpoints in this route group.
    pub fn endpoints(&self) -> EndpointIter<'a> {
        EndpointIter {
            spec: self.spec,
            cursor: self.endpoint_cursor,
        }
    }

    /// Returns an iterator over routes in this route group.
    pub fn routes(&self) -> RouteIter<'a> {
        RouteIter {
            spec: self.spec,
            cursor: self.route_cursor,
        }
    }

    /// Returns an iterator over responses defined for this route group.
    pub fn responses(&self) -> ResponseIter<'a> {
        ResponseIter {
            spec: self.spec,
            cursor: self.response_cursor,
        }
    }

    fn from_data(spec: &'a Spec, data: RouteData) -> Self {
        Self {
            path: spec.endpoints.get_path(data.path),
            docs: data.docs.map(|id| spec.strings.resolve(id)),
            spec,
            route_cursor: spec.endpoints.route_cursor(data),
            endpoint_cursor: spec.endpoints.endpoint_cursor(data),
            response_cursor: spec.endpoints.route_response_cursor(data),
        }
    }
}

/// Endpoint representation returned by the [`EndpointArena`].
#[derive(derive_more::Debug, Clone)]
pub struct EndpointView<'a> {
    /// Path of the endpoint.
    pub path: Path<'a>,

    /// Doc comments
    pub docs: Option<&'a str>,

    /// HTTP method used by the endpoint.
    pub method: http::Method,

    /// Name assigned to the endpoint.
    pub name: &'a Name,

    /// Path parameter type.
    pub path_param: Option<NamedTypeRefView<'a>>,
    /// Query parameter type.
    pub query: Option<NamedTypeRefView<'a>>,
    /// Headers type.
    pub headers: Option<NamedTypeRefView<'a>>,

    /// MIME types the endpoint accepts for the request body.
    pub accept: Option<()>, // TODO: Mime types

    /// Request body type.
    pub request_body: Option<InlineTyView<'a>>,
    /// Additional documentation for request body.
    pub request_body_docs: Option<&'a str>,

    #[debug(skip)]
    spec: &'a Spec,

    response_cursor: ResponseCursor,
}

impl<'a> EndpointView<'a> {
    /// Returns an iterator over responses for this endpoint.
    pub fn responses(&self) -> ResponseIter<'a> {
        ResponseIter {
            spec: self.spec,
            cursor: self.response_cursor,
        }
    }

    fn from_data(spec: &'a Spec, data: EndpointData) -> Self {
        let response_cursor = spec.endpoints.endpoint_response_cursor(&data);

        Self {
            path: spec.endpoints.get_path(data.path),
            docs: data.fields.docs.map(|id| spec.strings.resolve(id)),
            method: data.fields.method,
            name: spec.strings.resolve_name(data.fields.name),
            path_param: data
                .fields
                .path_param
                .map(|id| spec.get_simple_record_type(id)),
            query: data.fields.query.map(|id| spec.get_simple_record_type(id)),
            headers: data
                .fields
                .headers
                .map(|id| spec.get_simple_record_type(id)),
            accept: data.fields.accept, // TODO: Mime types
            request_body: data.fields.request_body.map(|id| spec.get_inline_type(id)),
            request_body_docs: data
                .fields
                .request_body_docs
                .map(|id| spec.strings.resolve(id)),
            spec,
            response_cursor,
        }
    }
}

/// Iterator over endpoints inside a [`Route`].
pub struct EndpointIter<'a> {
    spec: &'a Spec,
    cursor: EndpointCursor,
}

impl<'a> Iterator for EndpointIter<'a> {
    type Item = EndpointView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (data, next) = self.spec.endpoints.next_endpoint(self.cursor)?;

        self.cursor = next;
        Some(EndpointView::from_data(self.spec, data))
    }
}

/// Iterator over routes inside a [`Route`]
pub struct RouteIter<'a> {
    spec: &'a Spec,
    cursor: RouteCursor,
}

impl<'a> Iterator for RouteIter<'a> {
    type Item = RouteView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (data, next) = self.spec.endpoints.next_route(self.cursor)?;

        self.cursor = next;
        Some(RouteView::from_data(self.spec, data))
    }
}

/// Iterator over responses inside a [`Route`] or [`Endpoint`]
pub struct ResponseIter<'a> {
    spec: &'a Spec,
    cursor: ResponseCursor,
}

impl<'a> Iterator for ResponseIter<'a> {
    type Item = EndpointResponseView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (data, next) = self.spec.endpoints.next_response(self.cursor)?;

        self.cursor = next;
        Some(EndpointResponseView::from_data(self.spec, data))
    }
}

/// Type used in endpoint response  
#[derive(Debug, Clone)]
pub enum EndpointResponseType<'a> {
    Response(NamedRootTypeRefView<'a>),
    InlineType(InlineTyView<'a>),
}

/// Response definition
#[derive(Debug, Clone)]
pub struct EndpointResponseView<'a> {
    /// Status code of the response.
    pub status: ResponseStatus,

    /// Response body type.
    pub typ: EndpointResponseType<'a>,

    /// Doc comments
    pub docs: Option<&'a str>,
}

impl<'a> EndpointResponseView<'a> {
    fn from_data(spec: &'a Spec, data: ResponseData) -> Self {
        let typ = match data.typ {
            EndpointResponseTypeId::Response(id) => {
                EndpointResponseType::Response(spec.get_response_reference_type(id))
            }
            EndpointResponseTypeId::InlineType(id) => {
                EndpointResponseType::InlineType(spec.get_inline_type(id))
            }
        };

        Self {
            status: data.status,
            typ,
            docs: data.docs.map(|id| spec.strings.resolve(id)),
        }
    }
}
