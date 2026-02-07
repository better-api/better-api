//! Define methods for validating and lowering routes and endpoints.

use better_api_diagnostic::{Label, Report};
use better_api_syntax::TextRange;
use better_api_syntax::ast::{self, AstNode};

use crate::oracle::Context;
use crate::oracle::typ::{
    ensure_inline, lower_simple_record_param, lower_type, require_no_file, require_not_response,
    require_with_deref,
};
use crate::oracle::value::lower_mime_types;
use crate::path::PathPart;
use crate::spec::endpoint::{
    EndpointArena, EndpointBuilder, EndpointData, EndpointId, EndpointResponseId,
    EndpointResponseTypeId, ResponseStatus, RouteBuilder,
};
use crate::spec::typ::{InlineTyId, TypeArena};
use crate::spec::value::{ValueArena, ValueContext};
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

/// Validate and lower endpoint and it's responses.
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

    // Does accept parameter requires the request body to be `file`
    let requires_file = accept_id.is_some_and(|id| {
        let val_ctx = ValueContext {
            strings: ctx.strings,
            values,
        };

        val_ctx
            .get_mime_types(id)
            .any(|mime| mime != "application/json")
    });

    // Validate and lower request body.
    let name = ctx.strings.resolve(name_id);
    let req_body = lower_request_body(
        ctx,
        types,
        values,
        requires_file,
        endpoint,
        &format!("{name}RequestBody"),
    )
    .ok()?;

    // Lower path
    let path_token = endpoint.path().map(|p| p.string())?;
    let parsed_str = text::parse_string(&path_token, ctx.reports);
    let path = PathPart::new(&parsed_str, path_token.text_range(), ctx.reports);

    let mut endpoint_builder = parent.add_endpoint(
        path,
        EndpointData {
            method: method?,
            name: name_id,
            path_param,
            query: query_param,
            headers: headers_param,
            accept: accept_id,
            request_body: req_body,

            // TODO: Extract doc comments
            docs: None,
            request_body_docs: None,
        },
    );

    for resp in endpoint.responses() {
        lower_endpoint_response(ctx, &mut endpoint_builder, &resp);
    }

    Some(endpoint_builder.finish())
}

/// Validate and lower request body.
fn lower_request_body(
    ctx: &mut Context,
    types: &mut TypeArena,
    values: &mut ValueArena,
    requires_file: bool,
    endpoint: &ast::Endpoint,
    name: &str,
) -> Result<Option<InlineTyId>, ()> {
    let body = endpoint.request_body().and_then(|b| b.typ());
    let endpoint_range = endpoint.syntax().text_range();
    let accept_range = endpoint.accept().map(|a| a.syntax().text_range());

    let Some(body) = body else {
        if requires_file {
            let mut report = Report::error("endpoint's `requestBody` is required".to_string())
                .add_label(Label::primary(
                    "endpoint's `accept` parameter requires `requestBody` body to be defined"
                        .to_string(),
                    endpoint_range.into(),
                ));

            if let Some(range) = accept_range {
                report = report.add_label(Label::secondary(
                    "`accept` defined here".to_string(),
                    range.into(),
                ))
            }

            ctx.reports.push(report);

            return Err(());
        } else {
            return Ok(None);
        }
    };

    let mut is_valid = true;

    // Check that body isn't a response
    if require_not_response(ctx, &body).is_err() {
        is_valid = false;
    }

    if requires_file {
        // Check that request body is a file
        if require_request_body_is_file(ctx, &body, accept_range).is_err() {
            is_valid = false;
        }
    } else {
        // Check that request body is not a file and it does not contain a file (in case of ie a
        // record).
        let report_builder = |range: TextRange| {
            let mut report = Report::error("invalid endpoint`s request body type".to_string())
                .add_label(Label::primary(
                    "`accept` requires that no `file` is present in request body".to_string(),
                    body.syntax().text_range().into(),
                ))
                .add_label(Label::secondary(
                    "`file` introduced here".to_string(),
                    range.into(),
                ))
                .with_note(
                    "help: `application/json` request body must not use `file` in body".to_string(),
                );

            if let Some(range) = accept_range {
                report = report.add_label(Label::secondary(
                    "`accept` defined here".to_string(),
                    range.into(),
                ));
            }

            report
        };

        if require_no_file(ctx, &body, &report_builder).is_err() {
            is_valid = false;
        }
    }

    // Lower the body. Since there are no more validation steps after this,
    // we can do early return.
    let body_id = lower_type(ctx, types, values, &body).ok_or(())?;

    // If there are any errors during validation, don't build the final type
    if !is_valid {
        return Err(());
    }

    let body_id = ensure_inline(ctx, &body, body_id, name, types).ok_or(())?;

    // Safety: We checked that body is not a response, and that it's inlined.
    let body_id = unsafe { InlineTyId::new_unchecked(body_id) };
    Ok(Some(body_id))
}

/// Requires that request body is a file or a reference to a file.
fn require_request_body_is_file(
    ctx: &mut Context,
    node: &ast::Type,
    accept_range: Option<TextRange>,
) -> Result<(), ()> {
    let check = |derefed: &ast::Type| {
        if matches!(derefed, ast::Type::TypeFile(_)) {
            Ok(())
        } else {
            let mut report = Report::error("invalid endpoint's request body type".to_string())
                .add_label(Label::primary(
                    "endpoint's `accept` parameter requires `file` request body".to_string(),
                    node.syntax().text_range().into(),
                )).with_note("help: none `application/json` endpoint requests must use `file` as request body".to_string());

            if let Some(range) = accept_range {
                report = report.add_label(Label::secondary(
                    "`accept` defined here".to_string(),
                    range.into(),
                ))
            }

            Err(report)
        }
    };

    match require_with_deref(node, ctx.strings, ctx.symbol_map, ctx.root, check) {
        Ok(_) => Ok(()),
        Err(None) => Err(()),
        Err(Some(report)) => {
            ctx.reports.push(report);
            Err(())
        }
    }
}

/// Lower endpoint response
fn lower_endpoint_response<P: ResponseParent>(
    ctx: &mut Context,
    parent: &mut P,
    resp: &ast::EndpointResponse,
) {
    todo!("Implement me")
}

/// Converts AST method to http::Method
fn convert_method(method: ast::Method) -> http::Method {
    match method {
        ast::Method::Get => http::Method::GET,
        ast::Method::Post => http::Method::POST,
        ast::Method::Put => http::Method::PUT,
        ast::Method::Delete => http::Method::DELETE,
        ast::Method::Patch => http::Method::PATCH,
    }
}
