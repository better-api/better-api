use std::borrow::Cow;

use better_api_diagnostic::{Label, Report};
use better_api_syntax::ast::{self, AstNode};

use crate::{oracle::value::lower_mime_types, path::PathPart, spec::endpoint::EndpointId, text};

use super::Oracle;

impl<'a> Oracle<'a> {
    /// Lowers routes and endpoints
    pub(crate) fn lower_endpoints(&mut self) {
        for endpoint in self.root.endpoints() {
            self.lower_endpoint(&endpoint);
        }
    }

    fn lower_endpoint(&mut self, node: &ast::Endpoint) -> Option<EndpointId> {
        // Missing method is reported by the parser. This is an exception compared to all other
        // endpoint fields.
        let method = node.method().map(|method| convert_method(method.value()));

        // Validate name and return early if it doesn't exist or isn't valid.
        // Without a valid name we can't lower path, query, header params and request body.
        // They all require a name to be inlined.
        let name_id = if let Some(name) = node.name() {
            text::lower_name(&name, &mut self.strings, &mut self.reports)?
        } else {
            self.reports.push(
                Report::error("missing endpoint name".to_string())
                    .add_label(Label::primary(
                        "missing endpoint name".to_string(),
                        node.syntax().text_range().into(),
                    ))
                    .with_note("help: endpoint must have a unique name".to_string()),
            );
            return None;
        };

        // Lower headers
        let headers_param = node.header_param().and_then(|h| h.typ()).and_then(|typ| {
            let name = self.strings.resolve(name_id);
            let name = format!("{name}Headers");
            self.lower_simple_record_param(&typ, true, false, "headers", &name)
        });

        // Lower query
        let query_param = node.query_param().and_then(|q| q.typ()).and_then(|typ| {
            let name = self.strings.resolve(name_id);
            let name = format!("{name}Query");
            self.lower_simple_record_param(&typ, true, true, "query", &name)
        });

        // Lower query
        let path_param = node.path_param().and_then(|p| p.typ()).and_then(|typ| {
            let name = self.strings.resolve(name_id);
            let name = format!("{name}Path");
            self.lower_simple_record_param(&typ, false, false, "path", &name)
        });

        // Lower accept header
        let accept_id = node.accept().and_then(|a| a.value()).and_then(|v| {
            lower_mime_types(&mut self.values, &mut self.strings, &mut self.reports, &v)
        });

        // Validate and lower request body.
        if let Some(req_body) = node.request_body().and_then(|b| b.typ()) {}

        // TODO: Implement me
        None
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
