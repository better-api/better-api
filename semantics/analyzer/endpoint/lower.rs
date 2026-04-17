//! Define methods for validating and lowering routes and endpoints.

use std::collections::HashMap;

use better_api_diagnostic::{Label, Report};
use better_api_syntax::TextRange;
use better_api_syntax::ast::{self, AstNode};

use crate::analyzer::symbols::deref;
use crate::analyzer::typ::{
    InvalidInnerContext, InvalidOuterContext, SimpleRecordParamConfig, TypeClass, ensure_inline,
    lower_response, lower_simple_record_param, lower_type, new_invalid_inner_type, require_no_file,
    require_not_response, require_with_deref,
};
use crate::analyzer::value::lower_mime_types;
use crate::analyzer::{Context, SpecLowerer};
use crate::path::{PathId, PathPart};
use crate::spec::arena::endpoint::{
    EndpointArena, EndpointBuilder, EndpointFields, EndpointId, EndpointResponseId,
    EndpointResponseTypeId, ResponseStatus, RouteBuilder, RouteId,
};
use crate::spec::arena::typ::builder::TypeArenaBuilder;
use crate::spec::arena::typ::id::{InlineTypeId, ResponseReferenceProof};
use crate::spec::arena::value::ValueArena;
use crate::text::{self, NameId, StringId, StringInterner};

/// Helper trait to unify endpoint arena and route builder.
trait EndpointParent {
    fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        data: EndpointFields,
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
        data: EndpointFields,
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
        data: EndpointFields,
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
    endpoint_names: HashMap<NameId, TextRange>,
}

impl<'a> SpecLowerer<'a> {
    /// Lowers routes and endpoints
    pub(crate) fn lower_endpoints_and_routes(&mut self) {
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
            endpoint_names: HashMap::new(),
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

        for route in self.root.routes() {
            lower_route(
                &mut ctx,
                &mut self.values,
                &mut self.types,
                &mut self.endpoints,
                &route,
            );
        }
    }

    // /// Validate route and endpoint paths.
    // ///
    // /// It checks all paths are unique, parameters are unique, and that parameters match
    // /// endpoint's `path` record.
    // pub(crate) fn validate_paths(&mut self) {
    //     let spec_ctx = SpecContext {
    //         strings: &self.strings,
    //         symbol_table: &self.spec_symbol_table,
    //         values: &self.values,
    //         types: &self.types,
    //         endpoints: &self.endpoints,
    //     };
    //
    //     validate_paths(&self.range_map, &mut self.reports, &spec_ctx);
    // }
}

/// Lower a route
fn lower_route<P: EndpointParent>(
    ctx: &mut EndpointContext,
    values: &mut ValueArena,
    types: &mut TypeArenaBuilder,
    parent: &mut P,
    route: &ast::Route,
) -> Option<RouteId> {
    // Lower path and start route builder
    let path_token = route.path().map(|p| p.string());
    let mut route_builder = with_lowered_path(
        ctx,
        route.syntax().text_range(),
        path_token,
        |ctx, path, path_range| {
            // TODO: Get docs from doc comment
            let (builder, path_id) = parent.add_route(path, None);
            ctx.ctx.range_map.path.insert(path_id, path_range);
            builder
        },
    );

    // Lower children routes and endpoints
    for child in route.routes() {
        lower_route(ctx, values, types, &mut route_builder, &child);
    }

    for endpoint in route.endpoints() {
        lower_endpoint(ctx, values, types, &mut route_builder, &endpoint);
    }

    // Lower response
    lower_responses(
        ctx,
        values,
        types,
        &mut route_builder,
        LowerResponseBehavior::Route,
        route.responses(),
    );

    Some(route_builder.finish())
}

/// Validate and lower endpoint and it's responses.
fn lower_endpoint<P: EndpointParent>(
    ctx: &mut EndpointContext,
    values: &mut ValueArena,
    types: &mut TypeArenaBuilder,
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
    let (name_id, name_range) = if let Some(name) = endpoint.name() {
        (
            ctx.ctx.strings.lower_name(&name, Some(ctx.ctx.reports))?,
            name.syntax().text_range(),
        )
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

    // Check that name is unique
    if let Some(existing_range) = ctx.endpoint_names.get(&name_id) {
        let name = ctx.ctx.strings.resolve_name(name_id);
        ctx.ctx.reports.push(
            Report::error(format!("repeated endpoint name '{name}'"))
                .add_label(Label::primary(
                    "repeated endpoint name".to_string(),
                    name_range.into(),
                ))
                .add_label(Label::secondary(
                    "endpoint with same name first defined here".to_string(),
                    (*existing_range).into(),
                )),
        );
    } else {
        ctx.endpoint_names.insert(name_id, name_range);
    }

    // Lower headers
    let headers_param = endpoint
        .header_param()
        .and_then(|h| h.typ())
        .and_then(|typ| {
            let name = ctx.ctx.strings.resolve_name(name_id);
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
            let name = ctx.ctx.strings.resolve_name(name_id);
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

    // Lower path params
    let path_param = endpoint.path_param().and_then(|p| p.typ()).and_then(|typ| {
        let name = ctx.ctx.strings.resolve_name(name_id);
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
    let requires_file = accept_id.is_some_and(|_id| {
        // let val_ctx = ValueContext {
        //     strings: ctx.ctx.strings,
        //     values,
        // };
        //
        // val_ctx
        //     .get_mime_types(id)
        //     .any(|mime| mime != "application/json")
        // TODO: Check mime type is not application/json
        false
    });

    // Validate and lower request body.
    let name = ctx.ctx.strings.resolve_name(name_id);
    let name = format!("{name}RequestBody");
    let req_body =
        lower_request_body(&mut ctx.ctx, types, values, requires_file, endpoint, &name).ok()?;

    // Check if GET method has a request body
    if method == Some(http::Method::GET)
        && let Some(req_body) = endpoint.request_body()
    {
        ctx.ctx.reports.push(
            Report::warning("GET request with body".to_string())
                .add_label(Label::primary(
                    "GET requests should not have a request body".to_string(),
                    endpoint
                        .method()
                        .expect("method should be defined")
                        .syntax()
                        .text_range()
                        .into(),
                ))
                .add_label(Label::secondary(
                    "requestBody defined here".to_string(),
                    req_body.syntax().text_range().into(),
                ))
                .with_note(
                    "help: consider using POST or PUT instead, or remove the request body"
                        .to_string(),
                ),
        );
    }

    // Lower path and start endpoint builder with the lowered path
    let path_token = endpoint.path().map(|p| p.string());
    let endpoint_builder = with_lowered_path(
        ctx,
        endpoint.syntax().text_range(),
        path_token,
        move |ctx, path, path_range| {
            let (builder, path_id) = parent.add_endpoint(
                path,
                EndpointFields {
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
            ctx.ctx.range_map.path.insert(path_id, path_range);
            Some(builder)
        },
    );

    // Unpack the builder, it can be None if some stuff is invalid
    let mut endpoint_builder = endpoint_builder?;

    // Lower responses
    let nr_responses = lower_responses(
        ctx,
        values,
        types,
        &mut endpoint_builder,
        LowerResponseBehavior::Endpoint(name_id),
        endpoint.responses(),
    );

    if nr_responses == 0 {
        ctx.ctx.reports.push(
            Report::error("missing endpoint responses".to_string())
                .add_label(Label::primary(
                    "missing endpoint responses".to_string(),
                    endpoint.syntax().text_range().into(),
                ))
                .with_note("help: endpoint must have at least one response".to_string()),
        );
    }

    let id = endpoint_builder.finish();

    // Add range of path param attribute
    if let Some(param) = endpoint.path_param() {
        ctx.ctx
            .range_map
            .endpoint_path_attribute_name
            .insert(id, param.name_range());
    }

    Some(id)
}

/// Lower route or endpoint responses.
///
/// Returns number of lowered responses.
///
/// **Note:** Number of lowered response can be smaller than number of responses
/// in AST iterator.
fn lower_responses<P: ResponseParent>(
    ctx: &mut EndpointContext,
    values: &mut ValueArena,
    types: &mut TypeArenaBuilder,
    parent: &mut P,
    behavior: LowerResponseBehavior,
    responses: impl Iterator<Item = ast::EndpointResponse>,
) -> usize {
    ctx.response_statuses.clear();
    for resp in responses {
        let Some(status) =
            lower_endpoint_response(&mut ctx.ctx, values, types, parent, &resp, behavior)
        else {
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

    ctx.response_statuses.len()
}

/// Validate and lower request body.
fn lower_request_body(
    ctx: &mut Context,
    types: &mut TypeArenaBuilder,
    values: &mut ValueArena,
    requires_file: bool,
    endpoint: &ast::Endpoint,
    name: &str,
) -> Result<Option<InlineTypeId>, ()> {
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

    // Check that body isn't a response
    if require_not_response(ctx, &body).is_err() {
        return Err(());
    }

    let mut is_valid = true;

    if requires_file {
        // Check that request body is a file
        if require_request_body_is_file(ctx, &body, accept_range).is_err() {
            is_valid = false;
        }
    } else {
        // Check that request body is not a file and it does not contain a file (in case of ie a
        // record).
        let report_builder = |range: TextRange| {
            let mut report = Report::error("invalid endpoint's request body type".to_string())
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
    let body_id = unsafe { InlineTypeId::from_root_type_id(body_id) };
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

/// Defines how response lowering handles non-inlined types
#[derive(Clone, Copy, PartialEq, Eq)]
enum LowerResponseBehavior {
    /// Lower responses for endpoint. If a type is not inlined,
    /// we define a new type and use it as a reference. Name of
    /// the new type is based on the endpoint name.
    Endpoint(NameId),

    /// Lower responses for route. If a type is not inlined,
    /// we report an error.
    Route,
}

/// Lower endpoint response
///
/// Types that are not inlined are handled based on given
/// [behavior](LowerResponseBehavior).
fn lower_endpoint_response<P: ResponseParent>(
    ctx: &mut Context,
    values: &mut ValueArena,
    types: &mut TypeArenaBuilder,
    parent: &mut P,
    resp: &ast::EndpointResponse,
    behavior: LowerResponseBehavior,
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
        ast::Type::TypeResponse(resp) => match behavior {
            LowerResponseBehavior::Endpoint(endpoint_name_id) => {
                let resp_name = endpoint_response_name(ctx.strings, endpoint_name_id, status);
                let resp_id = lower_response(ctx, types, values, resp, &resp_name)?;

                let resp_id = ensure_inline(ctx, &typ, resp_id.into(), &resp_name, types)?;

                // Safety: We know it's a response and we have made sure that it's behind a reference.
                let resp_id = unsafe { ResponseReferenceProof::new(resp_id) };
                Some(EndpointResponseTypeId::Response(resp_id))
            }
            LowerResponseBehavior::Route => {
                ctx.reports.push(new_invalid_inner_type(
                    InvalidInnerContext::Response,
                    InvalidOuterContext::RouteResponse,
                    &typ,
                ));
                None
            }
        },

        ast::Type::TypeRef(reference) => {
            let reference_id = lower_type(ctx, types, values, &typ)?;
            match deref(ctx.strings, ctx.symbol_map, ctx.root, reference)? {
                ast::Type::TypeResponse(_) => {
                    // Safety: We know it's a reference to a response
                    let id = unsafe { ResponseReferenceProof::new(reference_id) };
                    Some(EndpointResponseTypeId::Response(id))
                }

                _ => {
                    // Safety: We know it's a reference to a type that isn't a response
                    let id = unsafe { InlineTypeId::from_root_type_id(reference_id) };
                    Some(EndpointResponseTypeId::InlineType(id))
                }
            }
        }
        _ => match behavior {
            LowerResponseBehavior::Endpoint(endpoint_name_id) => {
                let id = lower_type(ctx, types, values, &typ)?;

                let resp_name = endpoint_response_name(ctx.strings, endpoint_name_id, status);
                let id = ensure_inline(ctx, &typ, id, &resp_name, types)?;

                // Safety: We know it's a type and we made sure it's inline
                let id = unsafe { InlineTypeId::from_root_type_id(id) };
                Some(EndpointResponseTypeId::InlineType(id))
            }
            LowerResponseBehavior::Route => {
                match TypeClass::from(&typ) {
                    TypeClass::Inline => {
                        let id = lower_type(ctx, types, values, &typ)?;

                        // Safety: We know it's inline type
                        let id = unsafe { InlineTypeId::from_root_type_id(id) };
                        Some(EndpointResponseTypeId::InlineType(id))
                    }
                    TypeClass::Enum => {
                        ctx.reports.push(new_invalid_inner_type(
                            InvalidInnerContext::Enum,
                            InvalidOuterContext::RouteResponse,
                            &typ,
                        ));
                        None
                    }
                    TypeClass::Union => {
                        ctx.reports.push(new_invalid_inner_type(
                            InvalidInnerContext::Union,
                            InvalidOuterContext::RouteResponse,
                            &typ,
                        ));
                        None
                    }
                    TypeClass::Record => {
                        ctx.reports.push(new_invalid_inner_type(
                            InvalidInnerContext::Record,
                            InvalidOuterContext::RouteResponse,
                            &typ,
                        ));
                        None
                    }
                    TypeClass::Response => {
                        unreachable!(
                            "response should be handled separately by the outer match statement"
                        )
                    }
                }
            }
        },
    };

    // TODO: Get docs from doc comments
    let status = status?;
    parent.add_response(status, type_id?, None);
    Some(status)
}

/// Name of the auto generated type for endpoint response
fn endpoint_response_name(
    strings: &StringInterner,
    endpoint_name_id: NameId,
    status: Option<ResponseStatus>,
) -> String {
    let endpoint_name = strings.resolve_name(endpoint_name_id);
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

fn with_lowered_path<T, F>(
    ctx: &mut EndpointContext,
    fallback_range: TextRange,
    path_token: Option<ast::StringToken>,
    inner: F,
) -> T
where
    F: FnOnce(&mut EndpointContext, PathPart, TextRange) -> T,
{
    let path_range = path_token
        .as_ref()
        .map_or_else(|| fallback_range, |tk| tk.text_range());
    let parsed_str = path_token
        .as_ref()
        .map(|tk| text::parse_string(tk, Some(ctx.ctx.reports)));
    let path = match &parsed_str {
        Some(parsed_str) => PathPart::new(
            parsed_str,
            path_token
                .as_ref()
                .expect("path token should be defined when path str is defined")
                .text_range(),
            ctx.ctx.reports,
        ),
        None => PathPart::Empty,
    };

    inner(ctx, path, path_range)
}
