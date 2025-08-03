/// Represents a span of text.
/// Start and end are byte (utf-8) offset, starting at 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start < end);

        Self { start, end }
    }
}

/// Severity of the diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    /// Warning is shown to the user when you want to suggest to them a
    /// better or more correct way of doing things. It doesn't stop the building.
    Warning,

    /// Error is shown when you can't proceed with parsing/building/code generation.
    Error,
}

/// Label describes a region of code connected to a `Report`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    /// Message displayed to the user.
    pub message: String,

    /// Span of the code the label is associated with.
    pub span: Span,
}

impl Label {
    pub fn new(message: String, span: Span) -> Self {
        Self { message, span }
    }
}

/// A single diagnostic message that can provide information like errors and warnings
/// to the user.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Report {
    /// Severity of the diagnostic
    pub severity: Severity,

    /// Report title shown to the user. This should contain the
    /// main message you want to convey to the user.
    pub title: String,

    /// Label connected to the report. This should contain a detailed message
    /// of the report, connected to the location from where the diagnostic report originates.
    pub label: Option<Label>,

    /// Optional note displayed at the end of the report. Usually it contains a tip or a help
    /// message.
    pub note: Option<String>,
}

impl Report {
    pub fn new(severity: Severity, title: String) -> Self {
        Self {
            severity,
            title,
            label: None,
            note: None,
        }
    }

    pub fn warning(title: String) -> Self {
        Self::new(Severity::Warning, title)
    }

    pub fn error(title: String) -> Self {
        Self::new(Severity::Error, title)
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.label = Some(label);
        self
    }

    pub fn with_note(mut self, note: String) -> Self {
        self.note = Some(note);
        self
    }
}

// TODO: Implement conversion to https://github.com/brendanzab/codespan?tab=readme-ov-file
