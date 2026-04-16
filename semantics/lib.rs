//! Semantic analysis and representation of a Better API Specification.
//!
//! <div class="warning">TODO: Add overview of how to use this</div>

#![warn(missing_docs)]
#![warn(rustdoc::missing_crate_level_docs)]

pub mod path;
pub mod spec;
pub mod text;

mod analyzer;

pub use analyzer::{AnalyzeResult, analyze};
