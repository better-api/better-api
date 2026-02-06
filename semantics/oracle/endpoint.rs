use better_api_syntax::ast;

use crate::{path::PathPart, text};

use super::Oracle;

impl<'a> Oracle<'a> {
    /// Lowers routes and endpoints
    pub(crate) fn lower_endpoints(&mut self) {
        for endpoint in self.root.endpoints() {
            self.lower_endpoint(&endpoint);
        }
    }

    fn lower_endpoint(&mut self, node: &ast::Endpoint) {}
}
