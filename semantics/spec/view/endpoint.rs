//! Views over endpoint and route definitions in the [`Spec`].
//!
//! Endpoints and routes reference types defined in the spec. See [`typ`](crate::spec::view::typ)
//! documentation for more details on how types are structured and represented.

use crate::mime::MimeRange;
use crate::path::Path;
use crate::spec::Spec;
use crate::spec::arena::endpoint::{
    EndpointCursor, EndpointData, EndpointId, EndpointResponseTypeId, ResponseCursor, ResponseData,
    ResponseStatus, RouteCursor, RouteData,
};
use crate::spec::view::typ::{InlineTypeView, NamedRootTypeRefView, NamedTypeRefView};
use crate::text::Name;

/// Semantic view of a route.
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
    /// Returns an iterator over endpoints in this route.
    pub fn endpoints(&self) -> EndpointIter<'a> {
        EndpointIter {
            spec: self.spec,
            cursor: self.endpoint_cursor,
        }
    }

    /// Returns an iterator over nested routes in this route.
    pub fn routes(&self) -> RouteIter<'a> {
        RouteIter {
            spec: self.spec,
            cursor: self.route_cursor,
        }
    }

    /// Returns an iterator over responses defined for this route.
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

/// Semantic view of an endpoint.
#[derive(derive_more::Debug, Clone)]
pub struct EndpointView<'a> {
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
    pub path_param: Option<NamedTypeRefView<'a>>,
    /// Query parameter type.
    pub query: Option<NamedTypeRefView<'a>>,
    /// Headers type.
    pub headers: Option<NamedTypeRefView<'a>>,

    /// MIME types the endpoint accepts for the request body.
    pub accept: Option<MimeRange<'a>>,

    /// Request body type.
    pub request_body: Option<InlineTypeView<'a>>,
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
            id: data.id,
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
            accept: data.fields.accept.map(|id| spec.mimes.get_mime_range(id)),
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

/// Iterator over endpoints inside a [`RouteView`].
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

/// Iterator over routes inside a [`RouteView`].
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

/// Iterator over responses inside a [`RouteView`] or [`EndpointView`].
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

/// Type used in endpoint response.
///
/// It can be either a named reference to a response type, or an inline type.
#[derive(Debug, Clone)]
pub enum EndpointResponseType<'a> {
    /// Reference to a named response type.
    Response(NamedRootTypeRefView<'a>),
    /// Inline response body type.
    InlineType(InlineTypeView<'a>),
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

impl Spec {
    /// Returns an iterator over root-level endpoints.
    pub fn root_endpoints(&self) -> EndpointIter<'_> {
        EndpointIter {
            spec: self,
            cursor: self.endpoints.root_endpoint_cursor(),
        }
    }

    /// Returns an iterator over root-level routes.
    pub fn root_routes(&self) -> RouteIter<'_> {
        RouteIter {
            spec: self,
            cursor: self.endpoints.root_route_cursor(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::EndpointResponseType;
    use crate::path::PathPart;
    use crate::spec::Spec;
    use crate::spec::arena::endpoint::{EndpointFields, EndpointResponseTypeId, ResponseStatus};
    use crate::spec::arena::typ::builder::TypeArenaBuilder;
    use crate::spec::arena::typ::{PrimitiveTy, PrimitiveTy::String};
    use crate::spec::view::typ::InlineTypeView;
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

    fn string_type(spec: &mut Spec) -> crate::spec::arena::typ::id::InlineTypeId {
        let mut builder = TypeArenaBuilder::default();
        let string_id = builder.add_primitive(PrimitiveTy::String);
        spec.types = builder
            .finish(&spec.symbol_table)
            .expect("test type arena should build");
        string_id
    }

    #[test]
    fn iterates_root_and_nested_endpoint_views() {
        let mut spec = Spec::new_test();
        let string_id = string_type(&mut spec);

        let api_docs = spec.strings.get_or_intern("api docs");
        let users_docs = spec.strings.get_or_intern("users docs");

        let health_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("health")) };
        let users_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("users")) };
        let stats_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("stats")) };

        let (mut health, _) = spec.endpoints.add_endpoint(
            PathPart::Segment("/health"),
            endpoint_fields(health_name, Method::GET, None),
        );
        health.add_response(
            ResponseStatus::Code(StatusCode::OK),
            EndpointResponseTypeId::InlineType(string_id),
            None,
        );
        health.finish();

        let (mut api, _) = spec
            .endpoints
            .add_route(PathPart::Segment("/api"), Some(api_docs));
        api.add_response(
            ResponseStatus::Default,
            EndpointResponseTypeId::InlineType(string_id),
            None,
        );

        let (mut users, _) = api.add_endpoint(
            PathPart::Segment("/users"),
            endpoint_fields(users_name, Method::POST, Some(users_docs)),
        );
        users.add_response(
            ResponseStatus::Code(StatusCode::CREATED),
            EndpointResponseTypeId::InlineType(string_id),
            None,
        );
        users.finish();

        let (mut admin, _) = api.add_route(PathPart::Segment("/admin"), None);
        let (mut stats, _) = admin.add_endpoint(
            PathPart::Segment("/stats"),
            endpoint_fields(stats_name, Method::GET, None),
        );
        stats.add_response(
            ResponseStatus::Code(StatusCode::OK),
            EndpointResponseTypeId::InlineType(string_id),
            None,
        );
        stats.finish();
        admin.finish();
        api.finish();

        let mut root_endpoints = spec.root_endpoints();
        let health = root_endpoints.next().expect("root health endpoint");
        assert!(root_endpoints.next().is_none());
        assert_eq!(health.path.segments().as_slice(), &["/health"]);
        assert_eq!(health.method, Method::GET);
        assert_eq!(health.name.as_str(), "health");
        assert!(health.docs.is_none());
        assert!(health.request_body.is_none());

        let health_responses: Vec<_> = health.responses().collect();
        assert_eq!(health_responses.len(), 1);
        assert_eq!(
            health_responses[0].status,
            ResponseStatus::Code(StatusCode::OK)
        );
        assert!(matches!(
            health_responses[0].typ,
            EndpointResponseType::InlineType(InlineTypeView::Primitive(String))
        ));

        let mut root_routes = spec.root_routes();
        let api = root_routes.next().expect("root api route");
        assert!(root_routes.next().is_none());
        assert_eq!(api.path.segments().as_slice(), &["/api"]);
        assert_eq!(api.docs, Some("api docs"));

        let api_responses: Vec<_> = api.responses().collect();
        assert_eq!(api_responses.len(), 1);
        assert_eq!(api_responses[0].status, ResponseStatus::Default);

        let api_endpoints: Vec<_> = api.endpoints().collect();
        assert_eq!(api_endpoints.len(), 1);
        assert_eq!(
            api_endpoints[0].path.segments().as_slice(),
            &["/api", "/users"]
        );
        assert_eq!(api_endpoints[0].name.as_str(), "users");
        assert_eq!(api_endpoints[0].docs, Some("users docs"));

        let admin_routes: Vec<_> = api.routes().collect();
        assert_eq!(admin_routes.len(), 1);
        assert_eq!(
            admin_routes[0].path.segments().as_slice(),
            &["/api", "/admin"]
        );

        let stats_endpoints: Vec<_> = admin_routes[0].endpoints().collect();
        assert_eq!(stats_endpoints.len(), 1);
        assert_eq!(
            stats_endpoints[0].path.segments().as_slice(),
            &["/api", "/admin", "/stats"]
        );
        assert_eq!(stats_endpoints[0].name.as_str(), "stats");
    }
}
