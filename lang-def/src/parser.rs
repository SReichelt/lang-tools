use std::{
    borrow::Cow,
    fmt::{self, Debug, Display},
    iter::FusedIterator,
    ops::{Deref, DerefMut, Range},
};

use crate::{impl_mem_serializable_self, mem_serializable::MemSerializable};

/// This trait defines a parser that performs the initial analysis of a text document; as much as is
/// possible in a single pass (currently without access to other documents).
///
/// At the most basic level, a parser simply consumes a stream of input events and produces a stream
/// of output events (as well as diagnostics) while maintaining some internal state. By reporting
/// its internal state, the parser can allow its client to avoid reparsing the input if only part of
/// it has changed.
///
/// More concretely, whenever the client finds itself at a position where
/// * the reported state of the parser matches a previously reported state at an analogous position,
/// * one or more input events are the same as before at the analogous position, and
/// * serialized state exists after those input events,
///
/// then the client can reuse the previous output (and diagnostics) received while processing those
/// input events, and advance the parser to the next serialized state.
///
/// Parsers with matching output and input types can be composed. A typical use case is the
/// composition of a tokenizer with a parser that operates on tokens. In a language definition, the
/// input events of the parser are required to be characters, but this parser may be composed from
/// smaller parsers with different input event types.
pub trait Parser<IF: ParserInterface>: MemSerializable<IF::Pos> {
    /// Parses all or part of the given input. If [`Parser::parse`] returns `true`, the client will
    /// not call it again. Otherwise the client may or may not do so depending on current state, and
    /// it may serialize the state (via [`MemSerializable`]) and later use it to construct a new
    /// parser.
    ///
    /// Ideally, [`Parser::parse`] should always process as much of the input as necessary to reach
    /// an efficiently serializable state, and then return. However, it is valid (but inefficient)
    /// to always consume all input and not provide any state serialization.
    fn parse(&mut self, interface: &mut IF) -> bool;
}

/// A user-implemented trait that provides a character-based parser given a matching parser
/// interface.
pub trait CharParserDesc {
    type Config<'a>;
    type Out<'a, Pos: Position>: MemSerializable<Pos>;

    type Parser<
        'a,
        Pos: Position,
        IF: ParserInterface<
            Config = Self::Config<'a>,
            In = char,
            Out = Self::Out<'a, Pos>,
            Pos = Pos,
            Input: CharParserInput<'a>,
        >
    >: Parser<IF>;

    fn parser<
        'a,
        Pos: Position,
        IF: ParserInterface<
            Config = Self::Config<'a>,
            In = char,
            Out = Self::Out<'a, Pos>,
            Pos = Pos,
            Input: CharParserInput<'a>,
        >,
    >(
        interface: &IF,
    ) -> Self::Parser<'a, Pos, IF>;
}

pub trait ParserInterfaceBase {
    type Config;
    type Pos: Position;

    fn config(&self) -> &Self::Config;
    fn modify_config<R>(&mut self, f: impl FnOnce(&mut Self::Config) -> R) -> R;
}

pub trait ParserInputInterface: ParserInterfaceBase {
    type In;

    type Input: ParserInput<In = Self::In, Pos = Self::Pos>;
    type Diag: ParserDiagnostics<Pos = Self::Pos>;

    fn input(&mut self) -> &mut Self::Input;
    fn diagnostics(&mut self) -> &mut Self::Diag;

    fn diag(&mut self, span: impl Spanned<Pos = Self::Pos>, diag: Diagnostic<Self::Pos>) {
        self.diagnostics().diag(span, diag)
    }

    fn error(&mut self, span: impl Spanned<Pos = Self::Pos>, kind: Option<ErrorKind>, msg: String) {
        self.diag(span, Diagnostic::new(DiagnosticSeverity::Error(kind), msg))
    }

    fn warning(
        &mut self,
        span: impl Spanned<Pos = Self::Pos>,
        kind: Option<WarningKind>,
        msg: String,
    ) {
        self.diag(
            span,
            Diagnostic::new(DiagnosticSeverity::Warning(kind), msg),
        )
    }

    fn info(&mut self, span: impl Spanned<Pos = Self::Pos>, msg: String) {
        self.diag(span, Diagnostic::new(DiagnosticSeverity::Info, msg))
    }

    fn span_desc(&mut self, span: impl Spanned<Pos = Self::Pos>, desc: SpanDesc) {
        self.diagnostics().span_desc(span, desc)
    }
}

pub trait ParserOutputInterface: ParserInterfaceBase {
    type Out;

    type Output: ParserOutput<Out = Self::Out, Pos = Self::Pos>;

    fn output(&mut self) -> &mut Self::Output;

    fn out(&mut self, span: impl Spanned<Pos = Self::Pos>, out: Self::Out) {
        self.output().out(span, out)
    }
}

pub trait ParserInterface: ParserInputInterface + ParserOutputInterface {
    fn out_with_desc(
        &mut self,
        span: impl Spanned<Pos = Self::Pos>,
        out: Self::Out,
        desc: SpanDesc,
    ) {
        let span = span.span();
        self.span_desc(span.clone(), desc);
        self.out(span, out)
    }
}

pub struct StandardParserInterface<Input, Output, Diag, Config> {
    pub input: Input,
    pub output: Output,
    pub diag: Diag,
    pub config: Config,
}

impl<Pos: Position, Input, Output, Diag: ParserDiagnostics<Pos = Pos>, Config> ParserInterfaceBase
    for StandardParserInterface<Input, Output, Diag, Config>
{
    type Config = Config;
    type Pos = Pos;

    fn config(&self) -> &Self::Config {
        &self.config
    }

    fn modify_config<R>(&mut self, f: impl FnOnce(&mut Self::Config) -> R) -> R {
        f(&mut self.config)
    }
}

impl<
        In,
        Pos: Position,
        Input: ParserInput<In = In, Pos = Pos>,
        Output,
        Diag: ParserDiagnostics<Pos = Pos>,
        Config,
    > ParserInputInterface for StandardParserInterface<Input, Output, Diag, Config>
{
    type In = In;

    type Input = Input;
    type Diag = Diag;

    fn input(&mut self) -> &mut Self::Input {
        &mut self.input
    }

    fn diagnostics(&mut self) -> &mut Self::Diag {
        &mut self.diag
    }
}

impl<
        Out,
        Pos: Position,
        Input,
        Output: ParserOutput<Out = Out, Pos = Pos>,
        Diag: ParserDiagnostics<Pos = Pos>,
        Config,
    > ParserOutputInterface for StandardParserInterface<Input, Output, Diag, Config>
{
    type Out = Out;

    type Output = Output;

    fn output(&mut self) -> &mut Self::Output {
        &mut self.output
    }
}

impl<
        In,
        Out,
        Pos: Position,
        Input: ParserInput<In = In, Pos = Pos>,
        Output: ParserOutput<Out = Out, Pos = Pos>,
        Diag: ParserDiagnostics<Pos = Pos>,
        Config,
    > ParserInterface for StandardParserInterface<Input, Output, Diag, Config>
{
}

pub trait Position: Clone + PartialEq + Debug + MemSerializable<Self> + 'static {}

impl Position for () {}

pub trait Spanned {
    type Pos: Position;

    fn span(self) -> Range<Self::Pos>;
}

impl<Pos: Position> Spanned for Pos {
    type Pos = Pos;

    fn span(self) -> Range<Self::Pos> {
        self.clone()..self
    }
}

impl<T: Spanned> Spanned for Range<T> {
    type Pos = T::Pos;

    fn span(self) -> Range<Self::Pos> {
        self.start.span().start..self.end.span().end
    }
}

#[derive(Clone, PartialEq)]
pub struct WithSpan<T, Pos: Position> {
    inner: T,
    span: Range<Pos>,
}

impl<T, Pos: Position> WithSpan<T, Pos> {
    pub fn new(inner: T, span: impl Spanned<Pos = Pos>) -> Self {
        WithSpan {
            inner,
            span: span.span(),
        }
    }

    pub fn into_inner(self) -> T {
        self.inner
    }

    pub fn as_ref(&self) -> WithSpan<&T, Pos> {
        WithSpan::new(&self.inner, self)
    }
}

impl<T, Pos: Position> Deref for WithSpan<T, Pos> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T, Pos: Position> DerefMut for WithSpan<T, Pos> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T, Pos: Position> AsRef<T> for WithSpan<T, Pos> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T, Pos: Position> AsMut<T> for WithSpan<T, Pos> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

impl<T, Pos: Position> Spanned for &WithSpan<T, Pos> {
    type Pos = Pos;

    fn span(self) -> Range<Self::Pos> {
        self.span.clone()
    }
}

impl<T: Debug, Pos: Position> Debug for WithSpan<T, Pos> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)?;
        f.write_str(" [")?;
        self.span.fmt(f)?;
        f.write_str("]")?;
        Ok(())
    }
}

impl<T: Display, Pos: Position> Display for WithSpan<T, Pos> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T: MemSerializable<Pos>, Pos: Position> MemSerializable<Pos> for WithSpan<T, Pos> {
    type Serialized = (T::Serialized, Range<Pos::Serialized>);

    fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
        (
            self.inner.serialize(relative_to),
            self.span.serialize(relative_to),
        )
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
        WithSpan {
            inner: T::deserialize(&serialized.0, relative_to),
            span: <_>::deserialize(&serialized.1, relative_to),
        }
    }
}

pub trait ParserInput:
    FusedIterator<Item = WithSpan<Self::In, Self::Pos>>
    + LookAhead<LookAheadItem = Self::In, ConsumedItem = Self::Item, NextConsumedItems = Self::Item>
{
    type In;
    type Pos: Position;

    fn pos(&mut self) -> Self::Pos;
}

pub trait CharParserInput<'a>: ParserInput<In = char> {
    /// Returns a string slice of the given range that was previously iterated over, either owned or
    /// borrowed with a lifetime that ensures it can be stored within the parser.
    ///
    /// # Panics
    /// May panic if the given span starts before the input position at the beginning of the current
    /// call to [`Parser::parse`].
    fn span_str(&self, span: impl Spanned<Pos = Self::Pos>) -> Cow<'a, str>;
}

pub trait LookAhead {
    type LookAheadItem;
    type ConsumedItem;
    type NextConsumedItems;

    type LookAhead<'l>: Deref<Target = Self::LookAheadItem>
        + Consumable<ConsumedItems = Self::NextConsumedItems>
        + LookAhead<
            LookAheadItem = Self::LookAheadItem,
            ConsumedItem = Self::ConsumedItem,
            NextConsumedItems = (Self::NextConsumedItems, Self::ConsumedItem),
        >
    where
        Self: 'l;

    fn look_ahead(&mut self) -> Option<Self::LookAhead<'_>>;

    fn next_if(
        &mut self,
        pred: impl FnOnce(&Self::LookAheadItem) -> bool,
    ) -> Option<Self::NextConsumedItems> {
        let item = self.look_ahead()?;
        if pred(&*item) {
            Some(item.consume())
        } else {
            None
        }
    }

    fn check_next(&mut self, pred: impl FnOnce(&Self::LookAheadItem) -> bool) -> bool {
        if let Some(item) = self.look_ahead() {
            pred(&*item)
        } else {
            false
        }
    }

    fn has_next(&mut self) -> bool {
        self.look_ahead().is_some()
    }

    fn look_ahead_unbounded<R>(
        &mut self,
        f: impl FnMut(&Self::LookAheadItem) -> Option<R>,
    ) -> Option<R>;
}

pub trait Consumable {
    type ConsumedItems;

    fn consume(self) -> Self::ConsumedItems;
}

pub trait ParserOutput {
    type Out;
    type Pos: Position;

    fn out(&mut self, span: impl Spanned<Pos = Self::Pos>, out: Self::Out);
}

pub trait ParserDiagnostics {
    type Pos: Position;

    fn diag(&mut self, span: impl Spanned<Pos = Self::Pos>, diag: Diagnostic<Self::Pos>);

    fn span_desc(&mut self, span: impl Spanned<Pos = Self::Pos>, desc: SpanDesc);
}

#[derive(Clone, PartialEq, Debug)]
pub struct Diagnostic<Pos: Position> {
    pub severity: DiagnosticSeverity,
    pub message: DiagnosticMessage<Pos>,
}

impl<Pos: Position> Diagnostic<Pos> {
    pub fn new(severity: DiagnosticSeverity, msg: String) -> Self {
        Diagnostic {
            severity,
            message: DiagnosticMessage::new(msg),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum DiagnosticSeverity {
    Error(Option<ErrorKind>),
    Warning(Option<WarningKind>),
    Info,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ErrorKind {
    SyntaxError,
    ResourceNotFound,
    IdentifierNotFound,
    TypeMismatch,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum WarningKind {
    SyntaxWarning,
    UnusedObject,
    UnreachableCode,
    Deprecated,
}

#[derive(Clone, PartialEq, Debug)]
pub struct DiagnosticMessage<Pos: Position> {
    pub msg: String,
    pub hints: Vec<WithSpan<DiagnosticMessage<Pos>, Pos>>,
}

impl<Pos: Position> DiagnosticMessage<Pos> {
    pub fn new(msg: String) -> Self {
        DiagnosticMessage {
            msg,
            hints: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum SpanDesc {
    ParenStart, // Must always be matched by `ParenEnd` later.
    ParenEnd,
    Comment,
    Keyword,
    Number,
    String,
    NameDef(NameScopeDesc, Option<NameKindDesc>),
    NameRef(NameScopeDesc, Option<NameKindDesc>),
}

impl_mem_serializable_self!(SpanDesc);

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum NameScopeDesc {
    Global,
    Instance, // corresponds to VSCode "enumMember"
    Field,    // corresponds to VSCode "property"
    Param,
    Local,
}

impl_mem_serializable_self!(NameScopeDesc);

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum NameKindDesc {
    Value,
    Function,
    Type,
    GenericType,
}

impl_mem_serializable_self!(NameKindDesc);

pub mod helpers {
    use std::mem::take;

    use super::*;

    pub trait IterLookAheadParent {
        type Pos: Position;
        type Item;
        type Iter: Clone;
        type NextConsumedItems;

        fn cur_iter(&self) -> &Self::Iter;

        fn iter_next(&mut self, iter: &mut Self::Iter) -> Option<WithSpan<Self::Item, Self::Pos>>;

        fn advance_to(
            &mut self,
            iter: Self::Iter,
            additional_item: WithSpan<Self::Item, Self::Pos>,
        ) -> Self::NextConsumedItems;
    }

    pub struct IterLookAhead<'l, Parent: IterLookAheadParent> {
        parent: &'l mut Parent,
        iter: Parent::Iter,
        item: Option<WithSpan<Parent::Item, Parent::Pos>>,
    }

    impl<'l, Parent: IterLookAheadParent> IterLookAhead<'l, Parent> {
        pub fn try_new(parent: &'l mut Parent) -> Option<Self> {
            let mut iter = parent.cur_iter().clone();
            let item = parent.iter_next(&mut iter);
            if item.is_some() {
                Some(IterLookAhead { iter, parent, item })
            } else {
                None
            }
        }

        pub fn iterate<R>(
            parent: &'l mut Parent,
            mut f: impl FnMut(&Parent::Item) -> Option<R>,
        ) -> Option<R> {
            let mut iter = parent.cur_iter().clone();
            while let Some(item) = parent.iter_next(&mut iter) {
                let result = f(&item);
                if result.is_some() {
                    return result;
                }
            }
            None
        }
    }

    impl<'l, Parent: IterLookAheadParent> Deref for IterLookAhead<'l, Parent> {
        type Target = Parent::Item;

        fn deref(&self) -> &Self::Target {
            self.item.as_ref().unwrap()
        }
    }

    impl<'l, Parent: IterLookAheadParent> LookAhead for IterLookAhead<'l, Parent> {
        type LookAheadItem = Parent::Item;
        type ConsumedItem = WithSpan<Parent::Item, Parent::Pos>;
        type NextConsumedItems = (Parent::NextConsumedItems, Self::ConsumedItem);

        type LookAhead<'m> = IterLookAhead<'m, Self>
        where
            Self: 'm;

        fn look_ahead(&mut self) -> Option<Self::LookAhead<'_>> {
            IterLookAhead::try_new(self)
        }

        fn look_ahead_unbounded<R>(
            &mut self,
            f: impl FnMut(&Self::LookAheadItem) -> Option<R>,
        ) -> Option<R> {
            IterLookAhead::iterate(self, f)
        }
    }

    impl<'l, Parent: IterLookAheadParent> Consumable for IterLookAhead<'l, Parent> {
        type ConsumedItems = Parent::NextConsumedItems;

        fn consume(self) -> Self::ConsumedItems {
            self.parent.advance_to(self.iter, self.item.unwrap())
        }
    }

    impl<'l, Parent: IterLookAheadParent> IterLookAheadParent for IterLookAhead<'l, Parent> {
        type Pos = Parent::Pos;
        type Item = Parent::Item;
        type Iter = Parent::Iter;
        type NextConsumedItems = (
            Parent::NextConsumedItems,
            WithSpan<Parent::Item, Parent::Pos>,
        );

        fn cur_iter(&self) -> &Self::Iter {
            &self.iter
        }

        fn iter_next(&mut self, iter: &mut Self::Iter) -> Option<WithSpan<Self::Item, Self::Pos>> {
            self.parent.iter_next(iter)
        }

        fn advance_to(
            &mut self,
            iter: Self::Iter,
            additional_item: WithSpan<Self::Item, Self::Pos>,
        ) -> Self::NextConsumedItems {
            self.iter = iter;
            let parent_items = self
                .parent
                .advance_to(self.iter.clone(), take(&mut self.item).unwrap());
            (parent_items, additional_item)
        }
    }
}

pub mod buffer {
    use std::{collections::VecDeque, mem::take};

    use temp_inst::{TempInstMut, TempRefMut, TempRepr, TempReprMut, TempReprMutChk};

    use super::*;

    pub struct ParserOutputBuffer<Out, Pos: Position>(VecDeque<WithSpan<Out, Pos>>);

    impl<Out, Pos: Position> ParserOutputBuffer<Out, Pos> {
        fn new() -> Self {
            ParserOutputBuffer(VecDeque::new())
        }
    }

    impl<Out: MemSerializable<Pos>, Pos: Position> MemSerializable<Pos>
        for ParserOutputBuffer<Out, Pos>
    {
        type Serialized = <VecDeque<WithSpan<Out, Pos>> as MemSerializable<Pos>>::Serialized;

        fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
            self.0.serialize(relative_to)
        }

        fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
            ParserOutputBuffer(<_>::deserialize(serialized, relative_to))
        }
    }

    impl<Out, Pos: Position> ParserOutput for ParserOutputBuffer<Out, Pos> {
        type Out = Out;
        type Pos = Pos;

        fn out(&mut self, span: impl Spanned<Pos = Self::Pos>, out: Self::Out) {
            self.0.push_back(WithSpan::new(out, span))
        }
    }

    #[derive(TempRepr, TempReprMut, TempReprMutChk)]
    pub struct BufferedParserInterface<IF: ParserInterfaceBase, Out> {
        interface: TempRefMut<IF>,
        buffer: TempRefMut<ParserOutputBuffer<Out, IF::Pos>>,
    }

    impl<Config1, Config2: 'static, IF: ParserInterfaceBase<Config = (Config1, Config2)>, Out>
        ParserInterfaceBase for BufferedParserInterface<IF, Out>
    {
        type Pos = IF::Pos;
        type Config = Config1;

        fn config(&self) -> &Self::Config {
            &self.interface.config().0
        }

        fn modify_config<R>(&mut self, f: impl FnOnce(&mut Self::Config) -> R) -> R {
            self.interface
                .get_mut()
                .modify_config(|(config1, _)| f(config1))
        }
    }

    impl<Config1, Config2: 'static, IF: ParserInputInterface<Config = (Config1, Config2)>, Out>
        ParserInputInterface for BufferedParserInterface<IF, Out>
    {
        type In = IF::In;

        type Input = IF::Input;
        type Diag = IF::Diag;

        fn input(&mut self) -> &mut Self::Input {
            self.interface.get_mut().input()
        }

        fn diagnostics(&mut self) -> &mut Self::Diag {
            self.interface.get_mut().diagnostics()
        }
    }

    impl<Config1, Config2: 'static, IF: ParserInterfaceBase<Config = (Config1, Config2)>, Out>
        ParserOutputInterface for BufferedParserInterface<IF, Out>
    {
        type Out = Out;

        type Output = ParserOutputBuffer<Out, IF::Pos>;

        fn output(&mut self) -> &mut Self::Output {
            self.buffer.get_mut()
        }
    }

    impl<Config1, Config2: 'static, IF: ParserInputInterface<Config = (Config1, Config2)>, Out>
        ParserInterface for BufferedParserInterface<IF, Out>
    {
    }

    pub struct BufferedParser<IF: ParserInterfaceBase, Out, P> {
        parser: Option<P>,
        buffer: ParserOutputBuffer<Out, IF::Pos>,
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInputInterface<Config = (Config1, Config2)>,
            Out,
            P: Parser<BufferedParserInterface<IF, Out>>,
        > BufferedParser<IF, Out, P>
    {
        pub fn new(parser: P) -> Self {
            BufferedParser {
                parser: Some(parser),
                buffer: ParserOutputBuffer::new(),
            }
        }

        pub fn iterate<R>(
            &mut self,
            interface: &mut IF,
            f: impl FnOnce(&mut ParserOutputIter<IF, Out, P>) -> R,
        ) -> R {
            TempInstMut::call_with((interface, self), f)
        }

        fn try_parse_next(&mut self, interface: &mut IF) -> bool {
            let Some(parser) = &mut self.parser else {
                return false;
            };
            if TempInstMut::call_with((interface, &mut self.buffer), |parser_interface| {
                parser.parse(parser_interface)
            }) {
                self.parser = None;
            }
            true
        }

        pub(crate) fn finish(&mut self, interface: &mut IF) {
            self.buffer.0.clear();
            if let Some(parser) = &mut self.parser {
                TempInstMut::call_with((interface, &mut self.buffer), |parser_interface| {
                    while !parser.parse(parser_interface) {
                        let buffer = parser_interface.buffer.get_mut();
                        buffer.0.clear();
                    }
                });
                self.parser = None;
            }
        }
    }

    impl<IF: ParserInterfaceBase, Out: MemSerializable<IF::Pos>, P: MemSerializable<IF::Pos>>
        MemSerializable<IF::Pos> for BufferedParser<IF, Out, P>
    {
        type Serialized = (
            Option<P::Serialized>,
            <ParserOutputBuffer<Out, IF::Pos> as MemSerializable<IF::Pos>>::Serialized,
        );

        fn serialize(&self, relative_to: &IF::Pos) -> Self::Serialized {
            (
                self.parser.serialize(relative_to),
                self.buffer.serialize(relative_to),
            )
        }

        fn deserialize(serialized: &Self::Serialized, relative_to: &IF::Pos) -> Self {
            BufferedParser {
                parser: <_>::deserialize(&serialized.0, relative_to),
                buffer: <_>::deserialize(&serialized.1, relative_to),
            }
        }
    }

    #[derive(TempRepr, TempReprMut, TempReprMutChk)]
    pub struct ParserOutputIter<IF: ParserInterfaceBase, Out, P> {
        pub(crate) interface: TempRefMut<IF>,
        pub(crate) buffered_parser: TempRefMut<BufferedParser<IF, Out, P>>,
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInputInterface<Config = (Config1, Config2)>,
            Out,
            P: Parser<BufferedParserInterface<IF, Out>>,
        > Iterator for ParserOutputIter<IF, Out, P>
    {
        type Item = WithSpan<Out, IF::Pos>;

        fn next(&mut self) -> Option<Self::Item> {
            let interface = self.interface.get_mut();
            let bp = self.buffered_parser.get_mut();
            loop {
                let item = bp.buffer.0.pop_front();
                if item.is_some() {
                    return item;
                }
                if !bp.try_parse_next(interface) {
                    return None;
                }
            }
        }
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInputInterface<Config = (Config1, Config2)>,
            Out,
            P: Parser<BufferedParserInterface<IF, Out>>,
        > FusedIterator for ParserOutputIter<IF, Out, P>
    {
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInputInterface<Config = (Config1, Config2)>,
            Out,
            P: Parser<BufferedParserInterface<IF, Out>>,
        > ParserInput for ParserOutputIter<IF, Out, P>
    {
        type In = Out;
        type Pos = IF::Pos;

        fn pos(&mut self) -> Self::Pos {
            if let Some(item) = self.buffered_parser.buffer.0.front() {
                item.span.start.clone()
            } else {
                self.interface.input().pos()
            }
        }
    }

    pub trait LookAheadParent {
        type In;
        type Pos: Position;
        type NextConsumedItems;

        fn next_item(
            &mut self,
            pred: impl FnOnce(&Self::In) -> bool,
        ) -> Option<WithSpan<Self::In, Self::Pos>>;

        fn return_item(&mut self, item: WithSpan<Self::In, Self::Pos>);

        fn consume_items(
            &mut self,
            additional_item: WithSpan<Self::In, Self::Pos>,
        ) -> Self::NextConsumedItems;

        fn peek<R>(&mut self, f: impl FnOnce(&Self::In) -> R) -> Option<R>;

        fn iterate<R>(&mut self, f: impl FnMut(&Self::In) -> Option<R>) -> Option<R>;
    }

    impl<T: LookAheadParent> LookAhead for T {
        type LookAheadItem = T::In;
        type ConsumedItem = WithSpan<T::In, T::Pos>;
        type NextConsumedItems = T::NextConsumedItems;

        type LookAhead<'l> = LookAheadItem<'l, Self>
        where
            Self: 'l;

        fn look_ahead(&mut self) -> Option<Self::LookAhead<'_>> {
            LookAheadItem::try_new(self)
        }

        fn next_if(
            &mut self,
            pred: impl FnOnce(&Self::LookAheadItem) -> bool,
        ) -> Option<Self::NextConsumedItems> {
            let item = self.next_item(pred)?;
            Some(self.consume_items(item))
        }

        fn check_next(&mut self, pred: impl FnOnce(&Self::LookAheadItem) -> bool) -> bool {
            self.peek(pred).unwrap_or(false)
        }

        fn has_next(&mut self) -> bool {
            self.check_next(|_| true)
        }

        fn look_ahead_unbounded<R>(
            &mut self,
            f: impl FnMut(&Self::LookAheadItem) -> Option<R>,
        ) -> Option<R> {
            self.iterate(f)
        }
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInputInterface<Config = (Config1, Config2)>,
            Out,
            P: Parser<BufferedParserInterface<IF, Out>>,
        > LookAheadParent for ParserOutputIter<IF, Out, P>
    {
        type In = Out;
        type Pos = IF::Pos;
        type NextConsumedItems = WithSpan<Self::In, Self::Pos>;

        fn next_item(
            &mut self,
            pred: impl FnOnce(&Self::In) -> bool,
        ) -> Option<WithSpan<Self::In, Self::Pos>> {
            let interface = self.interface.get_mut();
            let bp = self.buffered_parser.get_mut();
            loop {
                if let Some(item) = bp.buffer.0.front() {
                    if pred(item) {
                        return bp.buffer.0.pop_front();
                    } else {
                        return None;
                    }
                }
                if !bp.try_parse_next(interface) {
                    return None;
                }
            }
        }

        fn return_item(&mut self, item: WithSpan<Self::In, Self::Pos>) {
            self.buffered_parser.buffer.0.push_front(item);
        }

        fn consume_items(
            &mut self,
            additional_item: WithSpan<Self::In, Self::Pos>,
        ) -> Self::NextConsumedItems {
            additional_item
        }

        fn peek<R>(&mut self, f: impl FnOnce(&Self::In) -> R) -> Option<R> {
            let interface = self.interface.get_mut();
            let bp = self.buffered_parser.get_mut();
            loop {
                if let Some(item) = bp.buffer.0.front() {
                    return Some(f(item));
                }
                if !bp.try_parse_next(interface) {
                    return None;
                }
            }
        }

        fn iterate<R>(&mut self, mut f: impl FnMut(&Self::In) -> Option<R>) -> Option<R> {
            let interface = self.interface.get_mut();
            let bp = self.buffered_parser.get_mut();
            let mut pos = 0;
            loop {
                let mut iter = bp.buffer.0.iter().skip(pos);
                while let Some(item) = iter.next() {
                    let result = f(&*item);
                    if result.is_some() {
                        return result;
                    }
                }
                pos = bp.buffer.0.len();
                if !bp.try_parse_next(interface) {
                    return None;
                }
            }
        }
    }

    pub struct LookAheadItem<'l, Parent: LookAheadParent> {
        parent: &'l mut Parent,
        item: Option<WithSpan<Parent::In, Parent::Pos>>,
    }

    impl<'l, Parent: LookAheadParent> LookAheadItem<'l, Parent> {
        fn try_new(parent: &'l mut Parent) -> Option<Self> {
            let item = parent.next_item(|_| true);
            if item.is_some() {
                Some(LookAheadItem { parent, item })
            } else {
                None
            }
        }
    }

    impl<'l, Parent: LookAheadParent> Drop for LookAheadItem<'l, Parent> {
        fn drop(&mut self) {
            if let Some(item) = take(&mut self.item) {
                self.parent.return_item(item);
            }
        }
    }

    impl<'l, Parent: LookAheadParent> Deref for LookAheadItem<'l, Parent> {
        type Target = Parent::In;

        fn deref(&self) -> &Self::Target {
            self.item.as_ref().unwrap()
        }
    }

    impl<'l, Parent: LookAheadParent> Consumable for LookAheadItem<'l, Parent> {
        type ConsumedItems = Parent::NextConsumedItems;

        fn consume(mut self) -> Self::ConsumedItems {
            self.parent.consume_items(take(&mut self.item).unwrap())
        }
    }

    impl<'l, Parent: LookAheadParent> LookAheadParent for LookAheadItem<'l, Parent> {
        type In = Parent::In;
        type Pos = Parent::Pos;
        type NextConsumedItems = (Parent::NextConsumedItems, WithSpan<Self::In, Self::Pos>);

        fn next_item(
            &mut self,
            pred: impl FnOnce(&Self::In) -> bool,
        ) -> Option<WithSpan<Self::In, Self::Pos>> {
            self.parent.next_item(pred)
        }

        fn return_item(&mut self, item: WithSpan<Self::In, Self::Pos>) {
            self.parent.return_item(item)
        }

        fn consume_items(
            &mut self,
            additional_item: WithSpan<Self::In, Self::Pos>,
        ) -> Self::NextConsumedItems {
            (
                self.parent.consume_items(take(&mut self.item).unwrap()),
                additional_item,
            )
        }

        fn peek<R>(&mut self, f: impl FnOnce(&Self::In) -> R) -> Option<R> {
            self.parent.peek(f)
        }

        fn iterate<R>(&mut self, f: impl FnMut(&Self::In) -> Option<R>) -> Option<R> {
            self.parent.iterate(f)
        }
    }
}

pub mod compose {
    use std::collections::VecDeque;

    use temp_inst::TempReprMut;

    use super::{buffer::*, *};

    impl<Config1, Config2, IF: ParserInterfaceBase<Config = (Config1, Config2)>, Mid, P1>
        ParserInterfaceBase for ParserOutputIter<IF, Mid, P1>
    {
        type Pos = IF::Pos;
        type Config = (Config1, Config2);

        fn config(&self) -> &Self::Config {
            self.interface.config()
        }

        fn modify_config<R>(&mut self, f: impl FnOnce(&mut Self::Config) -> R) -> R {
            self.interface.get_mut().modify_config(f)
        }
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInputInterface<Config = (Config1, Config2)>,
            Mid,
            P1: Parser<BufferedParserInterface<IF, Mid>>,
        > ParserInputInterface for ParserOutputIter<IF, Mid, P1>
    {
        type In = Mid;

        type Input = Self;
        type Diag = IF::Diag;

        fn input(&mut self) -> &mut Self::Input {
            self
        }

        fn diagnostics(&mut self) -> &mut Self::Diag {
            self.interface.get_mut().diagnostics()
        }
    }

    impl<Config1, Config2, IF: ParserInterface<Config = (Config1, Config2)>, Mid, P1>
        ParserOutputInterface for ParserOutputIter<IF, Mid, P1>
    {
        type Out = IF::Out;

        type Output = IF::Output;

        fn output(&mut self) -> &mut Self::Output {
            self.interface.get_mut().output()
        }
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInterface<Config = (Config1, Config2)>,
            Mid,
            P1: Parser<BufferedParserInterface<IF, Mid>>,
        > ParserInterface for ParserOutputIter<IF, Mid, P1>
    {
    }

    pub struct ComposedParser<IF: ParserInterfaceBase, Mid, P1, P2> {
        p1: BufferedParser<IF, Mid, P1>,
        p2: P2,
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInterface<Config = (Config1, Config2)>,
            Mid: MemSerializable<IF::Pos>,
            P1: Parser<BufferedParserInterface<IF, Mid>>,
            P2: Parser<ParserOutputIter<IF, Mid, P1>>,
        > ComposedParser<IF, Mid, P1, P2>
    {
        pub fn new(p1: P1, p2: P2) -> Self {
            ComposedParser {
                p1: BufferedParser::new(p1),
                p2,
            }
        }
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInterface<Config = (Config1, Config2)>,
            Mid: MemSerializable<IF::Pos>,
            P1: Parser<BufferedParserInterface<IF, Mid>>,
            P2: Parser<ParserOutputIter<IF, Mid, P1>>,
        > MemSerializable<IF::Pos> for ComposedParser<IF, Mid, P1, P2>
    {
        type Serialized = (
            <(Option<P1>, VecDeque<WithSpan<Mid, IF::Pos>>) as MemSerializable<IF::Pos>>::Serialized,
            P2::Serialized,
        );

        fn serialize(&self, relative_to: &IF::Pos) -> Self::Serialized {
            (
                self.p1.serialize(relative_to),
                self.p2.serialize(relative_to),
            )
        }

        fn deserialize(serialized: &Self::Serialized, relative_to: &IF::Pos) -> Self {
            ComposedParser {
                p1: <_>::deserialize(&serialized.0, relative_to),
                p2: <_>::deserialize(&serialized.1, relative_to),
            }
        }
    }

    impl<
            Config1,
            Config2: 'static,
            IF: ParserInterface<Config = (Config1, Config2)>,
            Mid: MemSerializable<IF::Pos>,
            P1: Parser<BufferedParserInterface<IF, Mid>>,
            P2: Parser<ParserOutputIter<IF, Mid, P1>>,
        > Parser<IF> for ComposedParser<IF, Mid, P1, P2>
    {
        fn parse(&mut self, interface: &mut IF) -> bool {
            let result = self
                .p1
                .iterate(interface, |p2_interface| self.p2.parse(p2_interface));
            if result {
                // If the second parser finishes early, make sure that the first parser is run
                // to completion nevertheless, so that we don't lose any diagnostics.
                self.p1.finish(interface);
            }
            result
        }
    }
}

pub mod str {
    use std::str::Chars;

    use nonminmax::NonMaxUsize;

    use super::{helpers::*, *};

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct StrPosition(NonMaxUsize);

    impl StrPosition {
        pub fn from_non_max_usize(pos: NonMaxUsize) -> Self {
            StrPosition(pos)
        }

        pub fn from_usize(pos: usize) -> Self {
            StrPosition(NonMaxUsize::new(pos).unwrap())
        }

        pub fn to_non_max_usize(self) -> NonMaxUsize {
            self.0
        }

        pub fn to_usize(self) -> usize {
            self.0.get()
        }

        pub fn span_from_range(range: Range<usize>) -> Range<Self> {
            Self::from_usize(range.start)..Self::from_usize(range.end)
        }

        pub fn span_to_range(span: Range<Self>) -> Range<usize> {
            span.start.to_usize()..span.end.to_usize()
        }
    }

    impl Debug for StrPosition {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            Debug::fmt(&self.to_usize(), f)
        }
    }

    impl MemSerializable<StrPosition> for StrPosition {
        type Serialized = NonMaxUsize;

        fn serialize(&self, relative_to: &StrPosition) -> Self::Serialized {
            NonMaxUsize::new(relative_to.to_usize() - self.to_usize()).unwrap()
        }

        fn deserialize(serialized: &Self::Serialized, relative_to: &StrPosition) -> Self {
            StrPosition::from_usize(relative_to.to_usize() - serialized.get())
        }
    }

    impl Position for StrPosition {}

    #[derive(Clone)]
    pub struct StrIter<'a> {
        iter: Chars<'a>,
        pos: usize,
    }

    impl<'a> StrIter<'a> {
        pub fn new(s: &'a str) -> Self {
            StrIter {
                iter: s.chars(),
                pos: 0,
            }
        }

        pub fn with_start_pos(s: &'a str, pos: usize) -> Self {
            StrIter {
                iter: s[pos..].chars(),
                pos,
            }
        }

        pub fn next(&mut self, seen: &mut usize) -> Option<WithSpan<char, StrPosition>> {
            // Unfortunately, we need to duplicate some code from `CharIndices` here until
            // `CharIndices::offset` is stabilized.
            let pre_pos = self.pos;
            let pre_len = self.iter.as_str().len();
            let ch = self.iter.next()?;
            let len = self.iter.as_str().len();
            self.pos += pre_len - len;
            if *seen < self.pos {
                *seen = self.pos;
            }
            let span = StrPosition::span_from_range(pre_pos..self.pos);
            Some(WithSpan::new(ch, span))
        }

        pub fn pos(&self) -> usize {
            self.pos
        }

        pub fn pos_as_str_position(&self) -> StrPosition {
            StrPosition::from_usize(self.pos)
        }
    }

    pub struct StrInput<'a> {
        s: &'a str,
        iter: StrIter<'a>,
        seen: usize,
    }

    impl<'a> StrInput<'a> {
        pub fn new(s: &'a str) -> Self {
            StrInput {
                s,
                iter: StrIter::new(s),
                seen: 0,
            }
        }

        pub fn with_start_pos(s: &'a str, pos: usize, seen: usize) -> Self {
            assert!(seen >= pos);
            StrInput {
                s,
                iter: StrIter::with_start_pos(s, pos),
                seen,
            }
        }

        pub fn iter_pos(&self) -> usize {
            self.iter.pos()
        }

        pub fn seen(&self) -> usize {
            self.seen
        }
    }

    impl<'a> Iterator for StrInput<'a> {
        type Item = WithSpan<char, StrPosition>;

        fn next(&mut self) -> Option<Self::Item> {
            self.iter.next(&mut self.seen)
        }

        fn count(self) -> usize {
            self.iter.iter.count()
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            self.iter.iter.size_hint()
        }
    }

    impl<'a> FusedIterator for StrInput<'a> {}

    impl<'a> LookAhead for StrInput<'a> {
        type LookAheadItem = char;
        type ConsumedItem = WithSpan<char, StrPosition>;
        type NextConsumedItems = Self::ConsumedItem;

        type LookAhead<'l> = IterLookAhead<'l, Self>
        where
            Self: 'l;

        fn look_ahead(&mut self) -> Option<Self::LookAhead<'_>> {
            IterLookAhead::try_new(self)
        }

        fn look_ahead_unbounded<R>(
            &mut self,
            f: impl FnMut(&Self::LookAheadItem) -> Option<R>,
        ) -> Option<R> {
            IterLookAhead::iterate(self, f)
        }
    }

    impl<'a> IterLookAheadParent for StrInput<'a> {
        type Pos = StrPosition;
        type Item = char;
        type Iter = StrIter<'a>;
        type NextConsumedItems = WithSpan<char, StrPosition>;

        fn cur_iter(&self) -> &Self::Iter {
            &self.iter
        }

        fn iter_next(&mut self, iter: &mut Self::Iter) -> Option<WithSpan<Self::Item, Self::Pos>> {
            iter.next(&mut self.seen)
        }

        fn advance_to(
            &mut self,
            iter: Self::Iter,
            additional_item: WithSpan<Self::Item, Self::Pos>,
        ) -> Self::NextConsumedItems {
            self.iter = iter;
            additional_item
        }
    }

    impl<'a> ParserInput for StrInput<'a> {
        type In = char;
        type Pos = StrPosition;

        fn pos(&mut self) -> Self::Pos {
            self.iter.pos_as_str_position()
        }
    }

    impl<'a> CharParserInput<'a> for StrInput<'a> {
        fn span_str(&self, span: impl Spanned<Pos = Self::Pos>) -> Cow<'a, str> {
            Cow::Borrowed(&self.s[StrPosition::span_to_range(span.span())])
        }
    }
}

pub mod testing {
    use super::*;

    #[derive(Clone, PartialEq, Debug)]
    pub struct TestOutput<Out, Pos: Position> {
        pub output: Vec<WithSpan<Out, Pos>>,
    }

    impl<Out, Pos: Position> TestOutput<Out, Pos> {
        pub fn new() -> Self {
            TestOutput { output: Vec::new() }
        }
    }

    impl<Out, Pos: Position> ParserOutput for TestOutput<Out, Pos> {
        type Out = Out;
        type Pos = Pos;

        fn out(&mut self, span: impl Spanned<Pos = Self::Pos>, out: Self::Out) {
            self.output.push(WithSpan::new(out, span))
        }
    }

    pub struct TestDiagnostics<Pos: Position> {
        pub diag: Vec<WithSpan<Diagnostic<Pos>, Pos>>,
        pub desc: Vec<WithSpan<SpanDesc, Pos>>,
    }

    impl<Pos: Position> TestDiagnostics<Pos> {
        pub fn new() -> Self {
            TestDiagnostics {
                diag: Vec::new(),
                desc: Vec::new(),
            }
        }
    }

    impl<Pos: Position> ParserDiagnostics for TestDiagnostics<Pos> {
        type Pos = Pos;

        fn diag(&mut self, span: impl Spanned<Pos = Self::Pos>, diag: Diagnostic<Self::Pos>) {
            self.diag.push(WithSpan::new(diag, span))
        }

        fn span_desc(&mut self, span: impl Spanned<Pos = Self::Pos>, desc: SpanDesc) {
            self.desc.push(WithSpan::new(desc, span))
        }
    }

    pub mod str {
        use super::{super::str::*, *};

        pub type TestParserInterface<'a, Out, Config> = StandardParserInterface<
            StrInput<'a>,
            TestOutput<Out, StrPosition>,
            TestDiagnostics<StrPosition>,
            Config,
        >;

        impl<'a, Out, Config> TestParserInterface<'a, Out, Config> {
            pub fn new(input: &'a str, config: Config) -> Self {
                TestParserInterface {
                    input: StrInput::new(input),
                    output: TestOutput::new(),
                    diag: TestDiagnostics::new(),
                    config,
                }
            }
        }

        pub fn parse_all<'a, Desc: CharParserDesc>(
            input: &'a str,
            config: Desc::Config<'a>,
        ) -> (
            TestOutput<Desc::Out<'a, StrPosition>, StrPosition>,
            TestDiagnostics<StrPosition>,
        ) {
            let mut interface = TestParserInterface::new(input, config);
            let mut parser = Desc::parser(&interface);
            while !parser.parse(&mut interface) {}
            (interface.output, interface.diag)
        }
    }

    /// Some simple example parsers for testing and demonstration purposes.
    pub mod test_parsers {
        use std::mem::replace;

        use super::{compose::*, *};

        pub type Token<'a> = Cow<'a, str>;

        /// A tokenizer that splits an input string into tokens containing only ascii non-whitespace
        /// characters.
        /// Non-ascii characters are reported as errors, and multiple consecutive whitespace
        /// characters are reported as warnings.
        /// This parser makes use of single-character lookahead.
        #[derive(Clone, PartialEq, Debug)]
        pub struct Tokenizer;

        impl Tokenizer {
            pub fn new() -> Self {
                Tokenizer
            }
        }

        impl_mem_serializable_self!(Tokenizer);

        impl<'a, IF: ParserInterface<In = char, Out = Token<'a>, Input: CharParserInput<'a>>>
            Parser<IF> for Tokenizer
        {
            fn parse(&mut self, interface: &mut IF) -> bool {
                let input = interface.input();
                let Some(ch) = input.next() else {
                    return true;
                };

                if !ch.is_ascii() {
                    interface.error(
                        &ch,
                        Some(ErrorKind::SyntaxError),
                        format!("unexpected non-ascii character `{ch}`"),
                    );
                } else if ch.is_ascii_whitespace() {
                    let mut span = ch.span();
                    let mut len: usize = 1;
                    while let Some(ch) = input.look_ahead() {
                        if !ch.is_ascii() || !ch.is_ascii_whitespace() {
                            break;
                        }
                        let ch = ch.consume();
                        span.end = ch.span().end;
                        len += 1;
                    }
                    if len > 1 {
                        interface.warning(
                            span,
                            Some(WarningKind::SyntaxWarning),
                            format!("excessive whitespace length {len}"),
                        );
                    }
                } else {
                    let mut span = ch.span();
                    while let Some(ch) = input.look_ahead() {
                        if !ch.is_ascii() || ch.is_ascii_whitespace() {
                            break;
                        }
                        let ch = ch.consume();
                        span.end = ch.span().end;
                    }
                    let token = input.span_str(span.clone());
                    interface.out_with_desc(span, token, SpanDesc::String);
                }

                false
            }
        }

        #[derive(Clone)]
        pub struct TokenizerConfig;

        impl CharParserDesc for TokenizerConfig {
            type Out<'a, Pos: Position> = Token<'a>;
            type Config<'a> = Self;

            type Parser<
                'a,
                Pos: Position,
                IF: ParserInterface<
                    Config = Self::Config<'a>,
                    In = char,
                    Out = Self::Out<'a, Pos>,
                    Pos = Pos,
                    Input: CharParserInput<'a>,
                >,
            > = Tokenizer;

            fn parser<
                'a,
                Pos: Position,
                IF: ParserInterface<
                    Config = Self::Config<'a>,
                    In = char,
                    Out = Self::Out<'a, Pos>,
                    Pos = Pos,
                    Input: CharParserInput<'a>,
                >,
            >(
                _interface: &IF,
            ) -> Self::Parser<'a, Pos, IF> {
                Tokenizer::new()
            }
        }

        /// A parser that takes arbitrary input events (e.g. tokens) and outputs all pairs of
        /// consecutive inputs (i.e. with overlap).
        /// If no tokens are present, an info is reported.
        /// This parser makes use of single-token lookahead.
        #[derive(Clone, PartialEq)]
        pub struct PairMaker<In, Pos: Position> {
            prev_token: Option<WithSpan<In, Pos>>,
        }

        impl<In, Pos: Position> PairMaker<In, Pos> {
            pub fn new() -> Self {
                PairMaker { prev_token: None }
            }
        }

        impl<In: MemSerializable<Pos>, Pos: Position> MemSerializable<Pos> for PairMaker<In, Pos> {
            type Serialized = <Option<WithSpan<In, Pos>> as MemSerializable<Pos>>::Serialized;

            fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
                self.prev_token.serialize(relative_to)
            }

            fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
                PairMaker {
                    prev_token: <_>::deserialize(serialized, relative_to),
                }
            }
        }

        impl<
                In: MemSerializable<Pos> + Clone,
                Pos: Position,
                IF: ParserInterface<In = In, Out = (In, In), Pos = Pos>,
            > Parser<IF> for PairMaker<In, Pos>
        {
            fn parse(&mut self, interface: &mut IF) -> bool {
                let Some(token) = interface.input().next() else {
                    if self.prev_token.is_none() {
                        let pos = interface.input().pos();
                        interface.info(pos, "no tokens present".into());
                    }
                    return true;
                };
                if let Some(prev_token) = replace(&mut self.prev_token, Some(token.clone())) {
                    let span = &prev_token..&token;
                    interface.out(span.span(), (prev_token.into_inner(), token.into_inner()));
                }
                false
            }
        }

        #[derive(Clone)]
        pub struct PairMakerConfig;

        impl CharParserDesc for PairMakerConfig {
            type Out<'a, Pos: Position> = (Token<'a>, Token<'a>);
            type Config<'a> = (TokenizerConfig, Self);

            type Parser<
                'a,
                Pos: Position,
                IF: ParserInterface<
                    Config = Self::Config<'a>,
                    In = char,
                    Out = Self::Out<'a, Pos>,
                    Pos = Pos,
                    Input: CharParserInput<'a>,
                >,
            > = ComposedParser<IF, Token<'a>, Tokenizer, PairMaker<Token<'a>, Pos>>;

            fn parser<
                'a,
                Pos: Position,
                IF: ParserInterface<
                    Config = Self::Config<'a>,
                    In = char,
                    Out = Self::Out<'a, Pos>,
                    Pos = Pos,
                    Input: CharParserInput<'a>,
                >,
            >(
                _interface: &IF,
            ) -> Self::Parser<'a, Pos, IF> {
                ComposedParser::new(Tokenizer::new(), PairMaker::new())
            }
        }

        /// A parser that inputs and outputs tokens, returning only those output tokens that do not
        /// have any longer tokens to their left or right.
        /// This parser makes conditional use of single-token lookahead.
        #[derive(Clone, PartialEq, Debug)]
        pub struct PeakFinder {
            prev_token_len: Option<usize>,
        }

        impl PeakFinder {
            pub fn new() -> Self {
                PeakFinder {
                    prev_token_len: None,
                }
            }
        }

        impl_mem_serializable_self!(PeakFinder);

        impl<'a, IF: ParserInterface<In = Token<'a>, Out = Token<'a>>> Parser<IF> for PeakFinder {
            fn parse(&mut self, interface: &mut IF) -> bool {
                let input = interface.input();
                let Some(token) = input.next() else {
                    return true;
                };
                let token_len = token.len();
                let ge_prev_token = if let Some(prev_token_len) = self.prev_token_len {
                    token_len >= prev_token_len
                } else {
                    true
                };
                if ge_prev_token {
                    let ge_next_token = if let Some(next_token) = input.look_ahead() {
                        token_len >= next_token.len()
                    } else {
                        true
                    };
                    if ge_next_token {
                        interface.out(token.span(), token.into_inner());
                    }
                }
                self.prev_token_len = Some(token_len);
                false
            }
        }

        #[derive(Clone)]
        pub struct PeakFinderConfig;

        impl CharParserDesc for PeakFinderConfig {
            type Out<'a, Pos: Position> = Token<'a>;
            type Config<'a> = (TokenizerConfig, Self);

            type Parser<
                'a,
                Pos: Position,
                IF: ParserInterface<
                    Config = Self::Config<'a>,
                    In = char,
                    Out = Self::Out<'a, Pos>,
                    Pos = Pos,
                    Input: CharParserInput<'a>,
                >,
            > = ComposedParser<IF, Token<'a>, Tokenizer, PeakFinder>;

            fn parser<
                'a,
                Pos: Position,
                IF: ParserInterface<
                    Config = Self::Config<'a>,
                    In = char,
                    Out = Self::Out<'a, Pos>,
                    Pos = Pos,
                    Input: CharParserInput<'a>,
                >,
            >(
                _interface: &IF,
            ) -> Self::Parser<'a, Pos, IF> {
                ComposedParser::new(Tokenizer::new(), PeakFinder::new())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        buffer::*,
        str::*,
        testing::{str::*, test_parsers::*, TestDiagnostics, TestOutput},
        *,
    };

    #[test]
    fn can_collect_tokens() {
        let input = "abc de  fghi j  kl";
        let (output, diag) = parse_all::<TokenizerConfig>(input, TokenizerConfig);
        assert_eq!(
            output.output,
            [
                WithSpan::new(Cow::Borrowed("abc"), StrPosition::span_from_range(0..3)),
                WithSpan::new(Cow::Borrowed("de"), StrPosition::span_from_range(4..6)),
                WithSpan::new(Cow::Borrowed("fghi"), StrPosition::span_from_range(8..12)),
                WithSpan::new(Cow::Borrowed("j"), StrPosition::span_from_range(13..14)),
                WithSpan::new(Cow::Borrowed("kl"), StrPosition::span_from_range(16..18)),
            ]
        );
        assert_eq!(
            diag.diag,
            [
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(6..8)
                ),
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(14..16)
                )
            ]
        );
        assert_eq!(
            diag.desc,
            [
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(0..3)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(4..6)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(8..12)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(13..14)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(16..18)),
            ]
        );
    }

    #[test]
    fn can_iterate_over_tokens() {
        let mut interface = StandardParserInterface {
            input: StrInput::new("abc de  fghi j  kl"),
            output: (),
            diag: TestDiagnostics::new(),
            config: ((), ()),
        };
        let mut tokenizer = BufferedParser::new(Tokenizer::new());
        let tokens: Vec<WithSpan<Token, StrPosition>> =
            tokenizer.iterate(&mut interface, |iter| iter.collect());
        assert_eq!(
            tokens,
            [
                WithSpan::new(Cow::Borrowed("abc"), StrPosition::span_from_range(0..3)),
                WithSpan::new(Cow::Borrowed("de"), StrPosition::span_from_range(4..6)),
                WithSpan::new(Cow::Borrowed("fghi"), StrPosition::span_from_range(8..12)),
                WithSpan::new(Cow::Borrowed("j"), StrPosition::span_from_range(13..14)),
                WithSpan::new(Cow::Borrowed("kl"), StrPosition::span_from_range(16..18)),
            ]
        );
        assert_eq!(
            interface.diag.diag,
            [
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(6..8)
                ),
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(14..16)
                )
            ]
        );
        assert_eq!(
            interface.diag.desc,
            [
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(0..3)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(4..6)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(8..12)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(13..14)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(16..18)),
            ]
        );
    }

    #[test]
    fn can_iterate_over_tokens_with_lookahead() {
        let mut interface = StandardParserInterface {
            input: StrInput::new("abc de  fghi j  kl"),
            output: (),
            diag: TestDiagnostics::new(),
            config: ((), ()),
        };
        let mut tokenizer = BufferedParser::new(Tokenizer::new());

        tokenizer.iterate(&mut interface, |iter| {
            let token = iter.next().unwrap();
            assert_eq!(
                token,
                WithSpan::new(Cow::Borrowed("abc"), StrPosition::span_from_range(0..3))
            );

            {
                let mut lookahead = iter.look_ahead().unwrap();
                assert_eq!(*lookahead, "de");
                let mut lookahead = lookahead.look_ahead().unwrap();
                assert_eq!(*lookahead, "fghi");

                {
                    let lookahead2 = lookahead.look_ahead().unwrap();
                    assert_eq!(*lookahead2, "j");
                }

                let mut lookahead = lookahead.look_ahead().unwrap();
                assert_eq!(*lookahead, "j");
                let mut lookahead = lookahead.look_ahead().unwrap();
                assert_eq!(*lookahead, "kl");
                assert!(lookahead.look_ahead().is_none());
            }

            let token = iter.next().unwrap();
            assert_eq!(
                token,
                WithSpan::new(Cow::Borrowed("de"), StrPosition::span_from_range(4..6))
            );

            {
                let mut lookahead = iter.look_ahead().unwrap();
                assert_eq!(*lookahead, "fghi");
                let lookahead = lookahead.look_ahead().unwrap();
                assert_eq!(*lookahead, "j");
                let consumed = lookahead.consume();
                assert_eq!(
                    consumed,
                    (
                        WithSpan::new(Cow::Borrowed("fghi"), StrPosition::span_from_range(8..12)),
                        WithSpan::new(Cow::Borrowed("j"), StrPosition::span_from_range(13..14))
                    )
                );
            }

            let token = iter.next().unwrap();
            assert_eq!(
                token,
                WithSpan::new(Cow::Borrowed("kl"), StrPosition::span_from_range(16..18))
            );
        });

        assert_eq!(
            interface.diag.diag,
            [
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(6..8)
                ),
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(14..16)
                )
            ]
        );
        assert_eq!(
            interface.diag.desc,
            [
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(0..3)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(4..6)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(8..12)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(13..14)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(16..18)),
            ]
        );
    }

    #[test]
    fn can_iterate_over_tokens_with_unbounded_lookahead() {
        let mut interface = StandardParserInterface {
            input: StrInput::new("abc de  fghi j  kl"),
            output: (),
            diag: TestDiagnostics::new(),
            config: ((), ()),
        };
        let mut tokenizer = BufferedParser::new(Tokenizer::new());

        tokenizer.iterate(&mut interface, |iter| {
            let token = iter.next().unwrap();
            assert_eq!(
                token,
                WithSpan::new(Cow::Borrowed("abc"), StrPosition::span_from_range(0..3))
            );

            let j_len = iter.look_ahead_unbounded(|token| {
                if *token == "j" {
                    Some(token.len())
                } else {
                    None
                }
            });
            assert_eq!(j_len, Some(1));

            let token = iter.next().unwrap();
            assert_eq!(
                token,
                WithSpan::new(Cow::Borrowed("de"), StrPosition::span_from_range(4..6))
            );

            let abc_len = iter.look_ahead_unbounded(|token| {
                if *token == "abc" {
                    Some(token.len())
                } else {
                    None
                }
            });
            assert_eq!(abc_len, None);

            let kl_len = iter.look_ahead_unbounded(|token| {
                if *token == "kl" {
                    Some(token.len())
                } else {
                    None
                }
            });
            assert_eq!(kl_len, Some(2));

            let token = iter.next().unwrap();
            assert_eq!(
                token,
                WithSpan::new(Cow::Borrowed("fghi"), StrPosition::span_from_range(8..12))
            );
        });

        assert_eq!(
            interface.diag.diag,
            [
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(6..8)
                ),
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(14..16)
                )
            ]
        );
        assert_eq!(
            interface.diag.desc,
            [
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(0..3)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(4..6)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(8..12)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(13..14)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(16..18)),
            ]
        );
    }

    #[test]
    fn can_collect_token_pairs() {
        let input = "abc de  fghi j  kl";
        let (output, diag) =
            parse_all::<PairMakerConfig>(input, (TokenizerConfig, PairMakerConfig));
        assert_eq!(
            output.output,
            [
                WithSpan::new(
                    (Cow::Borrowed("abc"), Cow::Borrowed("de")),
                    StrPosition::span_from_range(0..6)
                ),
                WithSpan::new(
                    (Cow::Borrowed("de"), Cow::Borrowed("fghi")),
                    StrPosition::span_from_range(4..12)
                ),
                WithSpan::new(
                    (Cow::Borrowed("fghi"), Cow::Borrowed("j")),
                    StrPosition::span_from_range(8..14)
                ),
                WithSpan::new(
                    (Cow::Borrowed("j"), Cow::Borrowed("kl")),
                    StrPosition::span_from_range(13..18)
                ),
            ]
        );
        assert_eq!(
            diag.diag,
            [
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(6..8)
                ),
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 2".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(14..16)
                )
            ]
        );
        assert_eq!(
            diag.desc,
            [
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(0..3)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(4..6)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(8..12)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(13..14)),
                WithSpan::new(SpanDesc::String, StrPosition::span_from_range(16..18)),
            ]
        );
    }

    #[test]
    fn can_combine_diagnostics_for_token_pairs() {
        let input = "   ";
        let (output, diag) =
            parse_all::<PairMakerConfig>(input, (TokenizerConfig, PairMakerConfig));
        assert_eq!(output.output, []);
        assert_eq!(
            diag.diag,
            [
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        message: DiagnosticMessage {
                            msg: "excessive whitespace length 3".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(0..3)
                ),
                WithSpan::new(
                    Diagnostic {
                        severity: DiagnosticSeverity::Info,
                        message: DiagnosticMessage {
                            msg: "no tokens present".into(),
                            hints: Vec::new()
                        }
                    },
                    StrPosition::span_from_range(3..3)
                ),
            ]
        );
        assert_eq!(diag.desc, []);
    }

    #[derive(Clone, PartialEq, Debug)]
    struct ParserCacheSpan<State, Out> {
        len: usize,
        lookahead_len: usize,
        end_state: Option<State>,
        output: Option<Vec<Out>>,
    }

    type ParserCache<State, Out> = Vec<ParserCacheSpan<State, Out>>;

    struct StringWithParserCache<State, Out> {
        s: String,
        cache: ParserCache<State, Out>,
    }

    impl<State, Out> StringWithParserCache<State, Out> {
        fn new(s: &str) -> Self {
            StringWithParserCache {
                s: s.into(),
                cache: ParserCache::new(),
            }
        }

        fn replace_range(&mut self, range: Range<usize>, s: &str) {
            self.s.replace_range(range.clone(), s);

            let mut cur_pos = 0;
            let mut span_idx = 0;
            'outer: while span_idx < self.cache.len() {
                let mut span = &mut self.cache[span_idx];
                if range.start == cur_pos && range.end == cur_pos && span.output.is_some() {
                    self.cache.insert(
                        span_idx,
                        ParserCacheSpan {
                            len: s.len(),
                            lookahead_len: 0,
                            end_state: None,
                            output: None,
                        },
                    );
                    break;
                }
                let mut span_len = span.len;
                let mut end_pos = cur_pos + span_len;
                if range.start < end_pos {
                    while range.end > end_pos {
                        println!("removing cache entry from {cur_pos} to {end_pos}");
                        self.cache.remove(span_idx);
                        if span_idx < self.cache.len() {
                            span = &mut self.cache[span_idx];
                            span.len += span_len;
                            span.output = None;
                            span_len = span.len;
                            end_pos = cur_pos + span_len;
                        } else {
                            break 'outer;
                        }
                    }
                    span.len -= range.end - range.start;
                    span.len += s.len();
                    span.output = None;
                    break;
                }
                cur_pos = end_pos;
                span_idx += 1;
            }
        }

        #[allow(dead_code)]
        fn remove_range(&mut self, range: Range<usize>) {
            self.replace_range(range, "")
        }

        fn insert_str(&mut self, pos: usize, s: &str) {
            self.replace_range(pos..pos, s)
        }
    }

    // This function can be regarded as a blueprint for state handling within an LSP server (which
    // should be based on a text rope instead of a contiguous string).
    fn parse_all_with_state<'a, Desc: CharParserDesc>(
        input: &'a str,
        config: Desc::Config<'a>,
        cache: &mut ParserCache<
            <Desc::Parser<
                'a,
                StrPosition,
                TestParserInterface<'a, Desc::Out<'a, StrPosition>, Desc::Config<'a>>,
            > as MemSerializable<StrPosition>>::Serialized,
            <WithSpan<Desc::Out<'a, StrPosition>, StrPosition> as MemSerializable<StrPosition>>::Serialized,
        >,
    ) -> (
        TestOutput<Desc::Out<'a, StrPosition>, StrPosition>,
        TestDiagnostics<StrPosition>,
        usize,
    ) {
        // TODO: A real implementation will need to remember which spans called `modify_config`.
        // TODO: Diagnostics need to be treated equivalently to output.
        let mut interface = TestParserInterface::new(input, config);
        let mut steps = 0;

        let mut cur_pos = 0;
        let mut span_idx = 0;
        'outer: loop {
            loop {
                if span_idx >= cache.len() {
                    if cur_pos == input.len() {
                        break 'outer;
                    }
                    println!("starting to parse due to end of store at {cur_pos}");
                    break;
                }
                let span = &cache[span_idx];
                let end_pos = cur_pos + span.len;
                if span.end_state.is_none() {
                    println!(
                        "starting to parse at {cur_pos} due to lack of end state at {end_pos}"
                    );
                    break;
                }
                let Some(span_output) = &span.output else {
                    println!(
                        "starting to parse at {cur_pos} due to lack of output until {end_pos}"
                    );
                    break;
                };
                println!("using cached output from {cur_pos} to {end_pos}");
                span_idx += 1;
                cur_pos = end_pos;
                let relative_to = StrPosition::from_usize(cur_pos);
                interface.output.output.extend(
                    span_output
                        .iter()
                        .map(|item| <_>::deserialize(item, &relative_to)),
                );
            }

            let lookahead_len: usize;
            let mut parser: Desc::Parser<
                'a,
                StrPosition,
                TestParserInterface<'a, Desc::Out<'a, StrPosition>, Desc::Config<'a>>,
            >;
            if span_idx > 0 {
                let span = &cache[span_idx - 1];
                lookahead_len = span.lookahead_len;
                parser = <_>::deserialize(
                    span.end_state.as_ref().unwrap(),
                    &StrPosition::from_usize(cur_pos),
                );
            } else {
                parser = Desc::parser(&interface);
                lookahead_len = 0;
            }
            interface.input = StrInput::with_start_pos(input, cur_pos - lookahead_len, cur_pos);
            let mut output_len = interface.output.output.len();
            let mut parsing_required = true;
            while parsing_required {
                steps += 1;
                let finished = parser.parse(&mut interface);
                let pos = interface.input.seen();
                let len = pos - cur_pos;
                let new_output = &interface.output.output[output_len..];
                let relative_to = StrPosition::from_usize(pos);
                let new_span = ParserCacheSpan {
                    len,
                    lookahead_len: pos - interface.input.iter_pos(),
                    end_state: Some(parser.serialize(&relative_to)),
                    output: Some(
                        new_output
                            .iter()
                            .map(|item| item.serialize(&relative_to))
                            .collect(),
                    ),
                };
                let mut span_len_to_remove = len;
                loop {
                    if span_idx < cache.len() {
                        let span = &mut cache[span_idx];
                        let start_pos = pos - span_len_to_remove;
                        let end_pos = start_pos + span.len;
                        if span.len < span_len_to_remove {
                            println!("removing span from {start_pos} to {end_pos}");
                            span_len_to_remove -= span.len;
                            cache.remove(span_idx);
                            continue;
                        } else {
                            span.len -= span_len_to_remove;
                            span.output = None;
                            if span.len == 0 {
                                println!(
                                    "found span of equal length from {start_pos} to {end_pos}"
                                );
                                if span.end_state == new_span.end_state {
                                    println!(
                                        "span end state matches; continuing with cached output"
                                    );
                                    parsing_required = false;
                                } else {
                                    println!(
                                        "span end state does not match; cached: {:?}, new: {:?}",
                                        span.end_state, new_span.end_state
                                    );
                                }
                                *span = new_span;
                                span_idx += 1;
                                break;
                            } else {
                                let new_start_pos = end_pos - span.len;
                                println!("truncating span from {start_pos} to {end_pos}, now starting at {new_start_pos}");
                            }
                        }
                    }
                    println!("inserting span from {cur_pos} to {pos}");
                    cache.insert(span_idx, new_span);
                    span_idx += 1;
                    break;
                }
                cur_pos = pos;
                output_len = interface.output.output.len();
                if finished {
                    cache.truncate(span_idx);
                    break 'outer;
                }
            }
        }

        println!("parsing finished");
        (interface.output, interface.diag, steps)
    }

    #[test]
    fn can_reuse_intermediate_state() {
        let mut input = StringWithParserCache::new("abc de  fghi j  kl");

        let (output1, _, steps1) = parse_all_with_state::<PairMakerConfig>(
            &input.s,
            (TokenizerConfig, PairMakerConfig),
            &mut input.cache,
        );
        assert!(!output1.output.is_empty());
        assert_eq!(steps1, 6);
        assert!(!input.cache.is_empty());

        let (output2, _, steps2) = parse_all_with_state::<PairMakerConfig>(
            &input.s,
            (TokenizerConfig, PairMakerConfig),
            &mut input.cache,
        );
        assert_eq!(output2, output1);
        assert_eq!(steps2, 0);

        input.insert_str(0, " xy z");
        let (output3, _, steps3) = parse_all_with_state::<PairMakerConfig>(
            &input.s,
            (TokenizerConfig, PairMakerConfig),
            &mut input.cache,
        );
        assert_eq!(
            output3.output,
            [
                WithSpan::new(
                    (Cow::Borrowed("xy"), Cow::Borrowed("zabc")),
                    StrPosition::span_from_range(1..8)
                ),
                WithSpan::new(
                    (Cow::Borrowed("zabc"), Cow::Borrowed("de")),
                    StrPosition::span_from_range(4..11)
                ),
                WithSpan::new(
                    (Cow::Borrowed("de"), Cow::Borrowed("fghi")),
                    StrPosition::span_from_range(9..17)
                ),
                WithSpan::new(
                    (Cow::Borrowed("fghi"), Cow::Borrowed("j")),
                    StrPosition::span_from_range(13..19)
                ),
                WithSpan::new(
                    (Cow::Borrowed("j"), Cow::Borrowed("kl")),
                    StrPosition::span_from_range(18..23)
                ),
            ]
        );
        assert_eq!(steps3, 3);

        input.replace_range(14..20, " mn o");
        let (output4, _, steps4) = parse_all_with_state::<PairMakerConfig>(
            &input.s,
            (TokenizerConfig, PairMakerConfig),
            &mut input.cache,
        );
        assert_eq!(
            output4.output,
            [
                WithSpan::new(
                    (Cow::Borrowed("xy"), Cow::Borrowed("zabc")),
                    StrPosition::span_from_range(1..8)
                ),
                WithSpan::new(
                    (Cow::Borrowed("zabc"), Cow::Borrowed("de")),
                    StrPosition::span_from_range(4..11)
                ),
                WithSpan::new(
                    (Cow::Borrowed("de"), Cow::Borrowed("f")),
                    StrPosition::span_from_range(9..14)
                ),
                WithSpan::new(
                    (Cow::Borrowed("f"), Cow::Borrowed("mn")),
                    StrPosition::span_from_range(13..17)
                ),
                WithSpan::new(
                    (Cow::Borrowed("mn"), Cow::Borrowed("o")),
                    StrPosition::span_from_range(15..19)
                ),
                WithSpan::new(
                    (Cow::Borrowed("o"), Cow::Borrowed("kl")),
                    StrPosition::span_from_range(18..22)
                ),
            ]
        );
        assert_eq!(steps4, 4);
    }

    #[test]
    fn can_collect_peaks_with_lookahead() {
        let input = "abc de  f ghi j  kl";
        let (output, _) = parse_all::<PeakFinderConfig>(input, (TokenizerConfig, PeakFinderConfig));
        assert_eq!(
            output.output,
            [
                WithSpan::new(Cow::Borrowed("abc"), StrPosition::span_from_range(0..3)),
                WithSpan::new(Cow::Borrowed("ghi"), StrPosition::span_from_range(10..13)),
                WithSpan::new(Cow::Borrowed("kl"), StrPosition::span_from_range(17..19)),
            ]
        );
    }

    #[test]
    fn can_reuse_intermediate_state_with_lookahead() {
        let mut input = StringWithParserCache::new("abc de  f ghi j  kl");

        let (output1, _, steps1) = parse_all_with_state::<PeakFinderConfig>(
            &input.s,
            (TokenizerConfig, PeakFinderConfig),
            &mut input.cache,
        );
        assert!(!output1.output.is_empty());
        assert_eq!(steps1, 7);
        assert!(!input.cache.is_empty());

        let (output2, _, steps2) = parse_all_with_state::<PeakFinderConfig>(
            &input.s,
            (TokenizerConfig, PeakFinderConfig),
            &mut input.cache,
        );
        assert_eq!(output2, output1);
        assert_eq!(steps2, 0);

        let to_insert = " xy z";
        input.insert_str(0, to_insert);
        let (output3, _, steps3) = parse_all_with_state::<PeakFinderConfig>(
            &input.s,
            (TokenizerConfig, PeakFinderConfig),
            &mut input.cache,
        );
        assert_eq!(
            output3.output,
            [
                WithSpan::new(Cow::Borrowed("zabc"), StrPosition::span_from_range(4..8)),
                WithSpan::new(Cow::Borrowed("ghi"), StrPosition::span_from_range(15..18)),
                WithSpan::new(Cow::Borrowed("kl"), StrPosition::span_from_range(22..24)),
            ]
        );
        assert_eq!(steps3, 3);

        input.replace_range(17..21, " mn o");
        let (output4, _, steps4) = parse_all_with_state::<PeakFinderConfig>(
            &input.s,
            (TokenizerConfig, PeakFinderConfig),
            &mut input.cache,
        );
        assert_eq!(
            output4.output,
            [
                WithSpan::new(Cow::Borrowed("zabc"), StrPosition::span_from_range(4..8)),
                WithSpan::new(Cow::Borrowed("gh"), StrPosition::span_from_range(15..17)),
                WithSpan::new(Cow::Borrowed("mn"), StrPosition::span_from_range(18..20)),
                WithSpan::new(Cow::Borrowed("kl"), StrPosition::span_from_range(23..25)),
            ]
        );
        assert_eq!(steps4, 4);
    }
}
