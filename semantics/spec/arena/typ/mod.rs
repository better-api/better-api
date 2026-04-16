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
    /// 32-bit signed integer.
    #[display("`i32`")]
    I32,

    /// 64-bit signed integer.
    #[display("`i64`")]
    I64,

    /// 32-bit unsigned integer.
    #[display("`u32`")]
    U32,

    /// 64-bit unsigned integer.
    #[display("`u64`")]
    U64,

    /// 32-bit floating point number.
    #[display("`f32`")]
    F32,

    /// 64-bit floating point number.
    #[display("`f64`")]
    F64,

    /// String representation of an ISO date `YYYY-MM-DD`.
    #[display("`date`")]
    Date,

    /// String representation of a RFC 3339 timestamp.
    #[display("`timestamp`")]
    Timestamp,

    /// Boolean value.
    #[display("`bool`")]
    Bool,

    /// String value.
    #[display("`string`")]
    String,

    /// Binary data.
    #[display("`file`")]
    File,
}

/// Valid enum types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum EnumTy {
    /// 32-bit signed integer.
    #[display("`i32`")]
    I32,

    /// 64-bit signed integer.
    #[display("`i64`")]
    I64,

    /// 32-bit unsigned integer.
    #[display("`u32`")]
    U32,

    /// 64-bit unsigned integer.
    #[display("`u64`")]
    U64,

    /// String value.
    #[display("`string`")]
    String,
}

/// Type definition.
///
/// Stores the metadata for a named type definition.
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
