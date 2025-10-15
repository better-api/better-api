//! Defines semantic representation of values.

pub enum Type {
    String(String),
    Bool(bool),
    Integer(i128), // i128 is large enough for i64 and u64
    Float(f64),
    Object(Object),
}

pub struct Object {}
