pub mod mem_serializable;
pub mod parser;
mod util;

use parser::{CharParserInput, Parser, ParserInterface};

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
    /// The output event type of the (single-pass part of the) parser. Parsing a document produces
    /// a stream of such events.
    type Out;

    /// A user-defined type that implements a parser for the language, for the given input/output
    /// interface.
    type Parser<'a, IF: ParserInterface<In = char, Out = Self::Out,
                                        Pos: 'a, Input: CharParserInput<'a>>>:
        Parser<IF> + 'a;

    /// Creates a new parser with the given interface.
    fn parser<
        'a,
        IF: ParserInterface<In = char, Out = Self::Out, Pos: 'a, Input: CharParserInput<'a>>,
    >(
        &self,
    ) -> Self::Parser<'a, IF>;
}
