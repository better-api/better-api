// TODO:
// - [ ] Implement getters and iterators
// - [ ] Add comments
// - [ ] Add tests

use crate::{
    path::{Path, PathArena, PathId, PathPart},
    string::StringId,
    typ::TypeId,
};

pub struct Route<'a> {
    pub path: Path<'a>,
}

impl<'a> Route<'a> {
    pub fn endpoints(&self) {
        todo!("implement endpoint iterator")
    }

    pub fn routes(&self) {
        todo!("implement routes iterator")
    }

    pub fn responses(&self) {
        todo!("implement responses iterator")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EndpointFields {
    pub method: http::Method,

    pub name: Option<StringId>,

    pub path: Option<TypeId>,
    pub query: Option<TypeId>,
    pub headers: Option<TypeId>,

    // TODO: Accept should actually be an array of mime types.
    // Where exactly this validation happens (during construction or late) is yet to be decided.
    // It is also yet to be decided how to validate mime type is correct. This probably boils
    // down to using a library, but I haven't looked into it yet.
    pub accept: Option<StringId>,
    pub request_body: Option<TypeId>,
}

pub struct Endpoint<'a> {
    pub path: Path<'a>,
    pub fields: EndpointFields,
}

impl<'a> Endpoint<'a> {
    pub fn responses(&self) {
        todo!("implement responses iterator")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResponseStatus {
    Default,
    Code(http::StatusCode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Response {
    pub status: Option<ResponseStatus>,
    pub type_id: Option<TypeId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EndpointId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RouteId(u32);

struct Parent<'p> {
    data: &'p mut Vec<Slot>,
    paths: &'p mut PathArena,

    /// Path ID of the parent's path
    path_id: Option<PathId>,
}

pub struct EndpointBuilder<'p> {
    parent: Parent<'p>,

    start: EndpointId,

    end: u32,

    finished: bool,
}

impl<'p> EndpointBuilder<'p> {
    fn new(parent: Parent<'p>, path: PathPart, fields: EndpointFields) -> Self {
        let path_id = parent.paths.insert(parent.path_id, path);

        let idx = parent.data.len();
        parent.data.push(Slot::Endpoint {
            path: path_id,
            fields,
            end: 0,
        });

        Self {
            parent,
            start: EndpointId(idx as u32),
            end: 0,
            finished: false,
        }
    }

    pub fn add_response(&mut self, resp: Response) {
        self.parent.data.push(Slot::Response(resp))
    }

    pub fn finish(mut self) -> EndpointId {
        self.finished = true;

        let idx = self.parent.data.len();

        let start = self.start.0 as usize;
        let head = &mut self.parent.data[start];
        match head {
            Slot::Endpoint { end, .. } => *end = idx as u32,
            _ => unreachable!("invalid EndpintBuilder start"),
        }

        self.start
    }
}

impl<'p> Drop for EndpointBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        self.parent.data.truncate(self.start.0 as usize);
    }
}

pub struct RouteBuilder<'p> {
    parent: Parent<'p>,

    path_id: PathId,

    start: RouteId,

    end: u32,

    finished: bool,
}

impl<'p> RouteBuilder<'p> {
    fn new(parent: Parent<'p>, path: PathPart) -> Self {
        let path_id = parent.paths.insert(parent.path_id, path);

        let idx = parent.data.len();
        parent.data.push(Slot::Route {
            path: path_id,
            end: 0,
        });

        Self {
            parent,
            path_id,
            start: RouteId(idx as u32),
            end: 0,
            finished: false,
        }
    }

    pub fn add_response(&mut self, resp: Response) {
        self.parent.data.push(Slot::Response(resp))
    }

    pub fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        fields: EndpointFields,
    ) -> EndpointBuilder<'a> {
        let parent = Parent {
            data: self.parent.data,
            paths: self.parent.paths,
            path_id: Some(self.path_id),
        };

        EndpointBuilder::new(parent, path, fields)
    }

    pub fn finish(mut self) -> RouteId {
        self.finished = true;

        let idx = self.parent.data.len();

        let start = self.start.0 as usize;
        let head = &mut self.parent.data[start];
        match head {
            Slot::Endpoint { end, .. } => *end = idx as u32,
            _ => unreachable!("invalid EndpintBuilder start"),
        }

        self.start
    }
}

impl<'p> Drop for RouteBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        self.parent.data.truncate(self.start.0 as usize);
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Slot {
    Route {
        path: PathId,
        end: u32,
    },
    Endpoint {
        path: PathId,
        fields: EndpointFields,
        end: u32,
    },

    Response(Response),
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct EndpointArena {
    data: Vec<Slot>,
    paths: PathArena,
}

impl EndpointArena {
    pub fn new() -> Self {
        Self::default()
    }

    fn parent<'a>(&'a mut self) -> Parent<'a> {
        Parent {
            data: &mut self.data,
            paths: &mut self.paths,
            path_id: None,
        }
    }

    pub fn add_endpoint<'a>(
        &'a mut self,
        path: PathPart,
        fields: EndpointFields,
    ) -> EndpointBuilder<'a> {
        EndpointBuilder::new(self.parent(), path, fields)
    }

    pub fn add_route<'a>(&'a mut self, path: PathPart) -> RouteBuilder<'a> {
        RouteBuilder::new(self.parent(), path)
    }
}
