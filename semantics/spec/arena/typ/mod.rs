//! Defines semantic representation of types.
//!
//! ## Querying
//!
//! Types are queried from [`Spec`](crate::spec::Spec) through the spec query
//! context. This module exposes [`Type`], [`RootType`], [`ResponseTy`], and
//! related reference wrappers used during traversal.
//!
//! Within this module, [`Type`] represents the named/composite variants you
//! typically work with. The full representation, including responses and other
//! root-only variants, is [`RootType`].
//!
//! ## Construction
//!
//! Construction is handled by [`Oracle`](crate::Oracle). It builds the internal
//! arenas and performs validation before data is exposed through `SpecContext`.

use crate::spec::arena::typ::id::RootTypeId;
use crate::text::StringId;

pub(crate) mod arena;
pub(crate) mod builder;
pub(crate) mod id;

mod slot;

#[cfg(test)]
mod test;

pub(crate) use arena::TypeArena;

/// Primitive types.
#[derive(Debug, Clone, Copy, PartialEq, derive_more::Display)]
pub enum PrimitiveTy {
    #[display("`i32`")]
    I32,
    #[display("`i64`")]
    I64,
    #[display("`u32`")]
    U32,
    #[display("`u64`")]
    U64,
    #[display("`f32`")]
    F32,
    #[display("`f64`")]
    F64,
    #[display("`date`")]
    Date,
    #[display("`timestamp`")]
    Timestamp,
    #[display("`bool`")]
    Bool,
    #[display("`string`")]
    String,
    #[display("`file`")]
    File,
}

/// Valid enum types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum EnumTy {
    #[display("`i32`")]
    I32,
    #[display("`i64`")]
    I64,
    #[display("`u32`")]
    U32,
    #[display("`u64`")]
    U64,
    #[display("`string`")]
    String,
}

/// Type definition.
///
/// Stores the doc comment for a named type definition.
#[derive(Debug, Clone)]
pub(crate) struct TypeDefData {
    #[expect(
        dead_code,
        reason = "Stored for future semantic queries and diagnostics"
    )]
    pub docs: Option<StringId>,
    pub typ: RootTypeId,
    #[expect(
        dead_code,
        reason = "Stored for future semantic queries and diagnostics"
    )]
    pub name: StringId,
}
