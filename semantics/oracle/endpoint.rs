//! Define methods for validating and lowering routes and endpoints.

use std::collections::HashMap;

use better_api_diagnostic::{Label, Report};
use better_api_syntax::TextRange;
use better_api_syntax::ast::{self, AstNode};

use crate::oracle::symbols::deref;
use crate::oracle::typ::{
    SimpleRecordParamConfig, ensure_inline, lower_response, lower_simple_record_param, lower_type,
    require_no_file, require_not_response, require_with_deref,
};
use crate::oracle::value::lower_mime_types;
use crate::oracle::{Context, RangeMap};
use crate::path::{Path, PathId, PathPart};
use crate::spec::SpecContext;
use crate::spec::endpoint::{
    Endpoint, EndpointArena, EndpointBuilder, EndpointData, EndpointId, EndpointResponseId,
    EndpointResponseTypeId, ResponseStatus, Route, RouteBuilder,
};
use crate::spec::typ::{InlineTyId, ResponseReferenceId, TypeArena};
use crate::spec::value::{ValueArena, ValueContext};
use crate::string::{StringId, StringInterner};
use crate::text;

use super::Oracle;

/// Helper trait to unify endpoint arena and route builder.
trait EndpointParent {
    fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        data: EndpointData,
    ) -> (EndpointBuilder<'a>, PathId);

    fn add_route<'a>(
        &'a mut self,
        path: PathPart,
        docs: Option<StringId>,
    ) -> (RouteBuilder<'a>, PathId);
}

impl EndpointParent for EndpointArena {
    #[inline(always)]
    fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        data: EndpointData,
    ) -> (EndpointBuilder<'a>, PathId) {
        self.add_endpoint(path, data)
    }

    #[inline(always)]
    fn add_route<'a>(
        &'a mut self,
        path: PathPart,
        docs: Option<StringId>,
    ) -> (RouteBuilder<'a>, PathId) {
        self.add_route(path, docs)
    }
}

impl EndpointParent for RouteBuilder<'_> {
    #[inline(always)]
    fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        data: EndpointData,
    ) -> (EndpointBuilder<'a>, PathId) {
        self.add_endpoint(path, data)
    }

    #[inline(always)]
    fn add_route<'a>(
        &'a mut self,
        path: PathPart,
        docs: Option<StringId>,
    ) -> (RouteBuilder<'a>, PathId) {
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

struct EndpointContext<'o, 'a> {
    ctx: Context<'o, 'a>,
    response_statuses: HashMap<ResponseStatus, TextRange>,
}

impl<'a> Oracle<'a> {
    /// Lowers routes and endpoints
    pub(crate) fn lower_endpoints(&mut self) {
        let mut ctx = EndpointContext {
            ctx: Context {
                strings: &mut self.strings,
                spec_symbol_table: &mut self.spec_symbol_table,
                symbol_map: &mut self.symbol_map,
                reports: &mut self.reports,
                range_map: &mut self.range_map,
                root: self.root,
            },
            response_statuses: HashMap::new(),
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

    pub(crate) fn validate_paths(&mut self) {
        let spec_ctx = SpecContext {
            strings: &self.strings,
            symbol_table: &self.spec_symbol_table,
            values: &self.values,
            types: &self.types,
            endpoints: &self.endpoints,
        };

        validate_paths(&self.range_map, &mut self.reports, &spec_ctx);
    }
}

/// Validate and lower endpoint and it's responses.
fn lower_endpoint<P: EndpointParent>(
    ctx: &mut EndpointContext,
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
        text::lower_name(&name, ctx.ctx.strings, ctx.ctx.reports)?
    } else {
        ctx.ctx.reports.push(
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
            let name = ctx.ctx.strings.resolve(name_id);
            let name = format!("{name}Headers");
            lower_simple_record_param(
                &mut ctx.ctx,
                types,
                values,
                &typ,
                SimpleRecordParamConfig {
                    allow_option: true,
                    allow_array: false,
                    typ_name: "headers",
                    ref_name: &name,
                },
            )
        });

    // Lower query
    let query_param = endpoint
        .query_param()
        .and_then(|q| q.typ())
        .and_then(|typ| {
            let name = ctx.ctx.strings.resolve(name_id);
            let name = format!("{name}Query");
            lower_simple_record_param(
                &mut ctx.ctx,
                types,
                values,
                &typ,
                SimpleRecordParamConfig {
                    allow_option: true,
                    allow_array: true,
                    typ_name: "query",
                    ref_name: &name,
                },
            )
        });

    // Lower query
    // TODO: Check that fields match path fields
    let path_param = endpoint.path_param().and_then(|p| p.typ()).and_then(|typ| {
        let name = ctx.ctx.strings.resolve(name_id);
        let name = format!("{name}Path");
        lower_simple_record_param(
            &mut ctx.ctx,
            types,
            values,
            &typ,
            SimpleRecordParamConfig {
                allow_option: false,
                allow_array: false,
                typ_name: "path",
                ref_name: &name,
            },
        )
    });

    // Lower accept header
    let accept_id = endpoint
        .accept()
        .and_then(|a| a.value())
        .and_then(|v| lower_mime_types(values, ctx.ctx.strings, ctx.ctx.reports, &v));

    // Does accept parameter requires the request body to be `file`
    let requires_file = accept_id.is_some_and(|id| {
        let val_ctx = ValueContext {
            strings: ctx.ctx.strings,
            values,
        };

        val_ctx
            .get_mime_types(id)
            .any(|mime| mime != "application/json")
    });

    // Validate and lower request body.
    let name = ctx.ctx.strings.resolve(name_id);
    let name = format!("{name}RequestBody");
    let req_body =
        lower_request_body(&mut ctx.ctx, types, values, requires_file, endpoint, &name).ok()?;

    // Lower path
    let path_token = endpoint.path().map(|p| p.string())?;
    let parsed_str = text::parse_string(&path_token, ctx.ctx.reports);
    let path = PathPart::new(&parsed_str, path_token.text_range(), ctx.ctx.reports);

    let (mut endpoint_builder, path_id) = parent.add_endpoint(
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

    // Insert range mapping for path
    ctx.ctx
        .range_map
        .path
        .insert(path_id, path_token.text_range());

    // Lower responses
    ctx.response_statuses.clear();
    for resp in endpoint.responses() {
        let Some(status) = lower_endpoint_response(
            &mut ctx.ctx,
            values,
            types,
            &mut endpoint_builder,
            &resp,
            name_id,
        ) else {
            continue;
        };

        let range = resp
            .status()
            .map(|s| s.syntax().text_range())
            .unwrap_or_else(|| resp.syntax().text_range());

        if let Some(existing_range) = ctx.response_statuses.get(&status) {
            ctx.ctx.reports.push(
                Report::error(format!("repeated response status {status}"))
                    .add_label(Label::primary(
                        "repeated response status".to_string(),
                        range.into(),
                    ))
                    .add_label(Label::secondary(
                        "response with same status first defined here".to_string(),
                        (*existing_range).into(),
                    )),
            );
        } else {
            ctx.response_statuses.insert(status, range);
        }
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
    values: &mut ValueArena,
    types: &mut TypeArena,
    parent: &mut P,
    resp: &ast::EndpointResponse,
    endpoint_name_id: StringId,
) -> Option<ResponseStatus> {
    // Validate status
    let status = match resp.status() {
        None => None,
        Some(status) => match convert_status(&status) {
            Ok(status) => Some(status),
            Err(report) => {
                ctx.reports.push(report);
                None
            }
        },
    };

    let typ = resp.typ()?;
    let type_id = match &typ {
        ast::Type::TypeResponse(resp) => {
            let resp_name = endpoint_response_name(ctx.strings, endpoint_name_id, status);
            let resp_id = lower_response(ctx, types, values, resp, &resp_name)?;

            let resp_id = ensure_inline(ctx, &typ, resp_id.into(), &resp_name, types)?;

            // Safety: We know it's a response and we have made sure that it's behind a reference.
            let resp_id = unsafe { ResponseReferenceId::new_unchecked(resp_id) };
            EndpointResponseTypeId::Response(resp_id)
        }

        ast::Type::TypeRef(reference) => {
            let reference_id = lower_type(ctx, types, values, &typ)?;
            match deref(ctx.strings, ctx.symbol_map, ctx.root, reference)? {
                ast::Type::TypeResponse(_) => {
                    // Safety: We know it's a reference to a response
                    let id = unsafe { ResponseReferenceId::new_unchecked(reference_id) };
                    EndpointResponseTypeId::Response(id)
                }

                _ => {
                    // Safety: We know it's a reference to a type that isn't a response
                    let id = unsafe { InlineTyId::new_unchecked(reference_id) };
                    EndpointResponseTypeId::InlineType(id)
                }
            }
        }
        _ => {
            let id = lower_type(ctx, types, values, &typ)?;

            let resp_name = endpoint_response_name(ctx.strings, endpoint_name_id, status);
            let id = ensure_inline(ctx, &typ, id, &resp_name, types)?;

            // Safety: We know it's a type and we made sure it's inline
            let id = unsafe { InlineTyId::new_unchecked(id) };
            EndpointResponseTypeId::InlineType(id)
        }
    };

    // TODO: Get docs from doc comments
    let status = status?;
    parent.add_response(status, type_id, None);
    Some(status)
}

/// Validate paths are correct
///
/// Specifically this checks:
/// - paths are unique
/// - params for each path are unique
/// - params match with endpoint path type
fn validate_paths<'a>(range_map: &RangeMap, reports: &mut Vec<Report>, spec_ctx: &'a SpecContext) {
    let mut path_unique: HashMap<Path<'a>, TextRange> = HashMap::new();

    for endpoint in spec_ctx.root_endpoints() {
        validate_endpoint_path_unique(range_map, reports, &endpoint, &mut path_unique);
        validate_path_params_unique(&endpoint.path);
    }

    for route in spec_ctx.root_routes() {
        validate_route_paths_unique(range_map, reports, &route, &mut path_unique);
        validate_route_path_params_unique(&route);
    }

    // TODO: Implement param and endpoint path type match
}

/// Auxiliary method to validate if endpoint path is unique.
fn validate_endpoint_path_unique<'a>(
    range_map: &RangeMap,
    reports: &mut Vec<Report>,
    endpoint: &Endpoint<'a>,
    existing_paths: &mut HashMap<Path<'a>, TextRange>,
) {
    let path_id = endpoint.path.id();
    let path_range = range_map
        .path
        .get(&path_id)
        .expect("endpoint paths should be inserted into range map");

    if let Some(existing_range) = existing_paths.get(&endpoint.path) {
        reports.push(
            Report::error("duplicated endpoint path".to_string())
                .add_label(Label::primary(
                    "duplicated endpoint path".to_string(),
                    (*path_range).into(),
                ))
                .add_label(Label::secondary(
                    "same path first defined here".to_string(),
                    (*existing_range).into(),
                )),
        );
    } else {
        existing_paths.insert(endpoint.path.clone(), *path_range);
    }
}

/// Auxiliary method to validate if all endpoints inside the route have unique paths.
fn validate_route_paths_unique<'a>(
    range_map: &RangeMap,
    reports: &mut Vec<Report>,
    route: &Route<'a>,
    existing_paths: &mut HashMap<Path<'a>, TextRange>,
) {
    for endpoint in route.endpoints() {
        validate_endpoint_path_unique(range_map, reports, &endpoint, existing_paths);
    }

    for route in route.routes() {
        validate_route_paths_unique(range_map, reports, &route, existing_paths);
    }
}

/// Auxiliary method to validate if params in route path are unique.
///
/// This is used to call itself recursively on descendants.
fn validate_route_path_params_unique<'a>(route: &Route<'a>) {
    // Validate path of "self"
    validate_path_params_unique(&route.path);

    // Validate child endpoints.
    for endpoint in route.endpoints() {
        validate_path_params_unique(&endpoint.path);
    }

    // Validate child routes.
    for route in route.routes() {
        validate_route_path_params_unique(&route);
    }
}

/// Validate that path parameters are unique. Used for endpoint and route path validation.
fn validate_path_params_unique<'a>(path: &Path<'a>) {
    todo!()
}

/// Name of the auto generated type for endpoint response
fn endpoint_response_name(
    strings: &StringInterner,
    endpoint_name_id: StringId,
    status: Option<ResponseStatus>,
) -> String {
    let endpoint_name = strings.resolve(endpoint_name_id);
    match status {
        None => format!("{endpoint_name}DefaultResponse"),
        Some(status) => format!("{endpoint_name}{status}Response"),
    }
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

/// Convert AST response status to semantic one
fn convert_status(status: &ast::EndpointResponseStatus) -> Result<ResponseStatus, Report> {
    let build_report = || {
        Report::error("invalid response status".to_string()).add_label(Label::primary(
            "invalid response status".to_string(),
            status.syntax().text_range().into(),
        ))
    };

    let res = match status.value() {
        ast::ResponseStatus::Default => ResponseStatus::Default,
        ast::ResponseStatus::Code(code) => {
            let code = u16::try_from(code).map_err(|_| build_report())?;
            let code = http::StatusCode::from_u16(code).map_err(|_| build_report())?;
            ResponseStatus::Code(code)
        }
    };
    Ok(res)
}
