//! Defines semantic representation of types

use crate::name::NameBuf;

pub enum Type {
    Reference(NameBuf),
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    Date,
    Timestamp,
    Bool,
    String,
    File,
    Option(Box<Type>),
    Array(Box<Type>),
    Record(Vec<TypeField>),
    Union(Union),
    Enum(Enum),
    Response(Box<Response>),
}

pub struct TypeField {
    pub name: NameBuf,
    pub typ: Type,
}

pub struct Union {
    pub disriminator: String,
    pub fields: Vec<TypeField>,
}

pub enum Enum {
    String(Vec<String>),
    I32(Vec<i32>),
    I64(Vec<i64>),
    U32(Vec<u32>),
    U64(Vec<u64>),
}

pub struct Response {
    pub body: Type,
    pub headers: Option<Type>,
    pub content_type: Option<String>,
}
