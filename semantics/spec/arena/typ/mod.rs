//! Defines semantic representation of types.
//!
//! ## Querying
//!
//! Types are stored in [`TypeArena`] and queried through arena ids such as
//! [`InlineTypeId`](id::InlineTypeId), [`TypeId`](id::TypeId),
//! [`ResponseTypeId`](id::ResponseTypeId), and [`RootTypeId`](id::RootTypeId).
//! The higher-level wrappers used during traversal live in
//! [`crate::spec::view::typ`].
//!
//! Within this module, inline ids represent primitive, option, array, and
//! reference nodes. Type and root ids extend that representation with enums,
//! records, unions, and responses.
//!
//! ## Construction
//!
//! Construction is handled by [`Analyzer`](crate::analyzer::Analyzer). It builds the internal
//! arenas and performs validation before data is exposed through
//! [`Spec`](crate::spec::Spec) views.

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
        reason = "stored for future semantic queries and diagnostics"
    )]
    pub docs: Option<StringId>,
    pub typ: RootTypeId,
    #[expect(
        dead_code,
        reason = "stored for future semantic queries and diagnostics"
    )]
    pub name: StringId,
}
