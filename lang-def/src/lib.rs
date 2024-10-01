use parser::CharParserDesc;

pub mod mem_serializable;
pub mod parser;
pub mod parser_tools;

/// A trait that represents a (programming, markup, domain-specific, ...) language, for the purpose
/// of implementing reusable tooling.
///
/// A [`LanguageDefinition`] implementation is roughly equivalent to an LSP (Language Server
/// Protocol) server, and in fact the main use case is a generic LSP implementation that takes a
/// [`LanguageDefinition`] as input and takes care of all internal state handling (e.g. related to
/// text edits).
///
/// Currently, many LSP features are missing, but users are encouraged to add support for them as
/// desired.
pub trait LanguageDefinition {
    type ParserDesc: CharParserDesc;
}
