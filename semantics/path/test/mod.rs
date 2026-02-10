use better_api_syntax::{TextRange, TextSize};

mod part;
mod param;
mod path;

/// Helper function for constructing mock text range that belongs to a string.
fn text_range_for_str(s: &str) -> TextRange {
    // In real world range contains start and end `"` of a path string, which is why range has
    // to be +2
    TextRange::new(TextSize::new(0), TextSize::new(s.len() as u32 + 2))
}
