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

impl From<rowan::TextRange> for Span {
    fn from(range: rowan::TextRange) -> Self {
        Self::new(range.start().into(), range.end().into())
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

/// Style of the label.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

/// Label describes a region of code connected to a `Report`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    /// Style of the label.
    pub style: LabelStyle,

    /// Message displayed to the user.
    pub message: String,

    /// Span of the code the label is associated with.
    pub span: Span,
}

impl Label {
    /// Creates a new primary label
    pub fn primary(message: String, span: Span) -> Self {
        Self {
            style: LabelStyle::Primary,
            message,
            span,
        }
    }

    /// Creates a new secondary label
    pub fn secondary(message: String, span: Span) -> Self {
        Self {
            style: LabelStyle::Secondary,
            message,
            span,
        }
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
    pub labels: Vec<Label>,

    /// Optional note displayed at the end of the report. Usually it contains a tip or a help
    /// message.
    pub note: Option<String>,
}

impl Report {
    pub fn new(severity: Severity, title: String) -> Self {
        Self {
            severity,
            title,
            labels: Default::default(),
            note: None,
        }
    }

    pub fn warning(title: String) -> Self {
        Self::new(Severity::Warning, title)
    }

    pub fn error(title: String) -> Self {
        Self::new(Severity::Error, title)
    }

    pub fn add_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_note(mut self, note: String) -> Self {
        self.note = Some(note);
        self
    }
}

// TODO: Implement conversion to https://github.com/brendanzab/codespan?tab=readme-ov-file
