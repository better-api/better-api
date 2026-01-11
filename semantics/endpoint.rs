use crate::{
    path::{Path, PathArena, PathId, PathPart},
    typ::TypeId,
};

pub struct Route<'a> {
    pub path: Option<Path<'a>>,
}

impl<'a> Route<'a> {
    pub fn endpoints(&self) {
        todo!("implement endpoint iterator")
    }

    pub fn responses(&self) {
        todo!("implement responses iterator")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EndpointFields {
    // TODO: define them correctly
    pub name: u32,
    pub method: u32,
    pub query: u32,
    pub headers: u32,
    pub accept: u32,
    pub request_body: u32,
}

pub struct Endpoint<'a> {
    pub path: Option<Path<'a>>,
    pub fields: EndpointFields,
}

impl<'a> Endpoint<'a> {
    pub fn responses(&self) {
        todo!("implement responses iterator")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EndpointId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RouteId(u32);

struct Parent<'p> {
    data: &'p mut Vec<Slot>,
    paths: &'p mut PathArena,

    path_id: Option<PathId>,
}

pub struct EndpointBuilder<'p> {
    parent: Parent<'p>,

    start: EndpointId,

    end: u32,

    finished: bool,
}

impl<'p> EndpointBuilder<'p> {
    fn new(parent: Parent<'p>, path: Option<&PathPart>, fields: EndpointFields) -> Self {
        // TODO: Implement correctly
        // let path_id = path.map(|p| parent.paths.insert(parent.path_id, p));
        let path_id = None;

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

    pub fn add_response(&mut self, status: Option<u32>, type_id: Option<TypeId>) {
        self.parent.data.push(Slot::Response { status, type_id })
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

    path_id: Option<PathId>,

    start: RouteId,

    end: u32,

    finished: bool,
}

impl<'p> RouteBuilder<'p> {
    fn new(parent: Parent<'p>, path: Option<&PathPart>) -> Self {
        // TODO: Implement correctly
        // let path_id = path.map(|p| parent.paths.insert(parent.path_id, p));
        let path_id = None;

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

    pub fn add_response(&mut self, status: Option<u32>, type_id: Option<TypeId>) {
        self.parent.data.push(Slot::Response { status, type_id })
    }

    pub fn add_endpoint<'a>(
        &'a mut self,
        path: Option<&PathPart>,
        fields: EndpointFields,
    ) -> EndpointBuilder<'a> {
        let parent = Parent {
            data: self.parent.data,
            paths: self.parent.paths,
            path_id: self.path_id,
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
        path: Option<PathId>,
        end: u32,
    },
    Endpoint {
        path: Option<PathId>,
        fields: EndpointFields,
        end: u32,
    },

    Response {
        // TODO: This u32 is not enough, we need to also represent default
        // ALso we need to validate this. TBD if we do it here, or later on
        status: Option<u32>,
        type_id: Option<TypeId>,
    },
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
        path: Option<&PathPart>,
        fields: EndpointFields,
    ) -> EndpointBuilder<'a> {
        EndpointBuilder::new(self.parent(), path, fields)
    }

    pub fn add_route<'a>(&'a mut self, path: Option<&PathPart>) -> RouteBuilder<'a> {
        RouteBuilder::new(self.parent(), path)
    }
}
