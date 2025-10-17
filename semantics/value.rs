//! Defines semantic representation of values.

use crate::name::NameBuf;

pub enum Value {
    String(String),
    Bool(bool),
    Integer(i128), // i128 is large enough for i64 and u64
    Float(f64),
    Object(Vec<ObjectField>),
    Array(Vec<Value>),
}

pub struct ObjectField {
    pub name: NameBuf,
    pub value: Value,
}
