//! Define methods for validating and lowering routes and endpoints.
//!
//! The methods in here use a `std::mem::take` trick. We have to recursively
//! call methods on &mut Oracle, including methods for lowering types and values.
//! This is why we can't split some of the lower endpoint methods into normal functions
//! outside of oracle (like we do for lowering types and values).
//!
//! Instead all of the methods take &mut EndpointParent or &mut ResponseParent. The methods
//! that are exposed to pub(crate) are responsible for doing std::mem::take(&mut self.endpoints)
//! and at the end of the method doing self.endpoints = ...

use better_api_diagnostic::{Label, Report};
use better_api_syntax::ast::{self, AstNode};

use crate::oracle::value::lower_mime_types;
use crate::path::PathPart;
use crate::spec::endpoint::{
    EndpointArena, EndpointBuilder, EndpointData, EndpointId, EndpointResponseId,
    EndpointResponseTypeId, ResponseStatus, RouteBuilder,
};
use crate::string::StringId;
use crate::text;

use super::Oracle;

/// Helper trait to unify endpoint arena and route builder.
trait EndpointParent {
    fn add_endpoint<'a>(&'a mut self, path: PathPart, data: EndpointData) -> EndpointBuilder<'a>;

    fn add_route<'a>(&'a mut self, path: PathPart, docs: Option<StringId>) -> RouteBuilder<'a>;
}

impl EndpointParent for EndpointArena {
    #[inline(always)]
    fn add_endpoint<'a>(&'a mut self, path: PathPart, data: EndpointData) -> EndpointBuilder<'a> {
        self.add_endpoint(path, data)
    }

    #[inline(always)]
    fn add_route<'a>(&'a mut self, path: PathPart, docs: Option<StringId>) -> RouteBuilder<'a> {
        self.add_route(path, docs)
    }
}

impl EndpointParent for RouteBuilder<'_> {
    #[inline(always)]
    fn add_endpoint<'a>(&'a mut self, path: PathPart, data: EndpointData) -> EndpointBuilder<'a> {
        self.add_endpoint(path, data)
    }

    #[inline(always)]
    fn add_route<'a>(&'a mut self, path: PathPart, docs: Option<StringId>) -> RouteBuilder<'a> {
        self.add_route(path, docs)
    }
}

/// Helper trait to unify endpoint and route builder.
trait ResponseParent {
    fn add_response(
        &mut self,
        status: ResponseStatus,
        type_id: EndpointResponseTypeId,
        docs: Option<StringId>,
    ) -> EndpointResponseId;
}

impl ResponseParent for RouteBuilder<'_> {
    #[inline(always)]
    fn add_response(
        &mut self,
        status: ResponseStatus,
        type_id: EndpointResponseTypeId,
        docs: Option<StringId>,
    ) -> EndpointResponseId {
        self.add_response(status, type_id, docs)
    }
}

impl ResponseParent for EndpointBuilder<'_> {
    #[inline(always)]
    fn add_response(
        &mut self,
        status: ResponseStatus,
        type_id: EndpointResponseTypeId,
        docs: Option<StringId>,
    ) -> EndpointResponseId {
        self.add_response(status, type_id, docs)
    }
}

impl<'a> Oracle<'a> {
    /// Lowers routes and endpoints
    pub(crate) fn lower_endpoints(&mut self) {
        let mut endpoint_arena = std::mem::take(&mut self.endpoints);

        for endpoint in self.root.endpoints() {
            self.lower_endpoint(&mut endpoint_arena, &endpoint);
        }

        self.endpoints = endpoint_arena;
    }

    fn lower_endpoint<P: EndpointParent>(
        &mut self,
        parent: &mut P,
        endpoint: &ast::Endpoint,
    ) -> Option<EndpointId> {
        // Missing method is reported by the parser. This is an exception compared to all other
        // endpoint fields.
        let method = endpoint
            .method()
            .map(|method| convert_method(method.value()));

        // Validate name and return early if it doesn't exist or isn't valid.
        // Without a valid name we can't lower path, query, header params and request body.
        // They all require a name to be inlined.
        let name_id = if let Some(name) = endpoint.name() {
            text::lower_name(&name, &mut self.strings, &mut self.reports)?
        } else {
            self.reports.push(
                Report::error("missing endpoint name".to_string())
                    .add_label(Label::primary(
                        "missing endpoint name".to_string(),
                        endpoint.syntax().text_range().into(),
                    ))
                    .with_note("help: endpoint must have a unique name".to_string()),
            );
            return None;
        };

        // Lower headers
        let headers_param = endpoint
            .header_param()
            .and_then(|h| h.typ())
            .and_then(|typ| {
                let name = self.strings.resolve(name_id);
                let name = format!("{name}Headers");
                self.lower_simple_record_param(&typ, true, false, "headers", &name)
            });

        // Lower query
        let query_param = endpoint
            .query_param()
            .and_then(|q| q.typ())
            .and_then(|typ| {
                let name = self.strings.resolve(name_id);
                let name = format!("{name}Query");
                self.lower_simple_record_param(&typ, true, true, "query", &name)
            });

        // Lower query
        let path_param = endpoint.path_param().and_then(|p| p.typ()).and_then(|typ| {
            let name = self.strings.resolve(name_id);
            let name = format!("{name}Path");
            self.lower_simple_record_param(&typ, false, false, "path", &name)
        });

        // Lower accept header
        let accept_id = endpoint.accept().and_then(|a| a.value()).and_then(|v| {
            lower_mime_types(&mut self.values, &mut self.strings, &mut self.reports, &v)
        });

        // Validate and lower request body.
        if let Some(req_body) = endpoint.request_body().and_then(|b| b.typ()) {}

        // TODO: Check req body is valid, lower it and check it matches with accept header.

        let path_token = endpoint.path().map(|p| p.string())?;
        let parsed_str = text::parse_string(&path_token, &mut self.reports);
        let path = PathPart::new(&parsed_str, path_token.text_range(), &mut self.reports);

        let mut endpoint_builder = parent.add_endpoint(
            path,
            EndpointData {
                // TODO: Extract docs
                docs: None,
                method: method?,
                name: name_id,
                path_param,
                query: query_param,
                headers: headers_param,
                accept: accept_id,
                // TODO: Populate requst body
                request_body: None,
                // TODO: Extract docs
                request_body_docs: None,
            },
        );

        for resp in endpoint.responses() {
            self.lower_endpoint_response(&mut endpoint_builder, &resp);
        }

        Some(endpoint_builder.finish())
    }

    fn lower_endpoint_response<P: ResponseParent>(
        &mut self,
        parent: &mut P,
        resp: &ast::EndpointResponse,
    ) {
    }
}

fn convert_method(method: ast::Method) -> http::Method {
    match method {
        ast::Method::Get => http::Method::GET,
        ast::Method::Post => http::Method::POST,
        ast::Method::Put => http::Method::PUT,
        ast::Method::Delete => http::Method::DELETE,
        ast::Method::Patch => http::Method::PATCH,
    }
}
