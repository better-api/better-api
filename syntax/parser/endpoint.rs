use better_api_diagnostic::{Label, Report, Span};

use super::Parser;
use super::prologue::Prologue;
use crate::Kind::{self, *};
use crate::Token;

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    pub fn parse_endpoint(&mut self, prologue: Option<Prologue>) {}
}
