use crate::path::{Path, PathId};

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
}
