//! Define methods for validating and lowering routes and endpoints.

use better_api_diagnostic::{Label, Report};
use better_api_syntax::ast::{self, AstNode};

use crate::oracle::Context;
use crate::oracle::typ::lower_simple_record_param;
use crate::oracle::value::lower_mime_types;
use crate::path::PathPart;
use crate::spec::endpoint::{
    EndpointArena, EndpointBuilder, EndpointData, EndpointId, EndpointResponseId,
    EndpointResponseTypeId, ResponseStatus, RouteBuilder,
};
use crate::spec::typ::TypeArena;
use crate::spec::value::ValueArena;
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
        let mut ctx = Context {
            strings: &mut self.strings,
            spec_symbol_table: &mut self.spec_symbol_table,
            symbol_map: &mut self.symbol_map,
            reports: &mut self.reports,
            root: self.root,
        };

        for endpoint in self.root.endpoints() {
            lower_endpoint(
                &mut ctx,
                &mut self.values,
                &mut self.types,
                &mut self.endpoints,
                &endpoint,
            );
        }
    }
}

fn lower_endpoint<P: EndpointParent>(
    ctx: &mut Context,
    values: &mut ValueArena,
    types: &mut TypeArena,
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
        text::lower_name(&name, ctx.strings, ctx.reports)?
    } else {
        ctx.reports.push(
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
            let name = ctx.strings.resolve(name_id);
            let name = format!("{name}Headers");
            lower_simple_record_param(ctx, types, values, &typ, true, false, "headers", &name)
        });

    // Lower query
    let query_param = endpoint
        .query_param()
        .and_then(|q| q.typ())
        .and_then(|typ| {
            let name = ctx.strings.resolve(name_id);
            let name = format!("{name}Query");
            lower_simple_record_param(ctx, types, values, &typ, true, true, "query", &name)
        });

    // Lower query
    let path_param = endpoint.path_param().and_then(|p| p.typ()).and_then(|typ| {
        let name = ctx.strings.resolve(name_id);
        let name = format!("{name}Path");
        lower_simple_record_param(ctx, types, values, &typ, false, false, "path", &name)
    });

    // Lower accept header
    let accept_id = endpoint
        .accept()
        .and_then(|a| a.value())
        .and_then(|v| lower_mime_types(values, ctx.strings, ctx.reports, &v));

    // Validate and lower request body.
    if let Some(req_body) = endpoint.request_body().and_then(|b| b.typ()) {}

    // TODO: Check req body is valid, lower it and check it matches with accept header.

    let path_token = endpoint.path().map(|p| p.string())?;
    let parsed_str = text::parse_string(&path_token, ctx.reports);
    let path = PathPart::new(&parsed_str, path_token.text_range(), ctx.reports);

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
        lower_endpoint_response(ctx, &mut endpoint_builder, &resp);
    }

    Some(endpoint_builder.finish())
}

fn lower_endpoint_response<P: ResponseParent>(
    ctx: &mut Context,
    parent: &mut P,
    resp: &ast::EndpointResponse,
) {
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
