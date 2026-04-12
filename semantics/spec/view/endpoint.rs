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
