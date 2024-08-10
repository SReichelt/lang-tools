use std::fmt::Debug;

use lang_def::parser::*;

use internal::*;

pub type ExpectedFragment<'a, Out> = (ExpectedFragmentContent<'a>, Option<&'a Out>);

pub enum ExpectedFragmentContent<'a> {
    Input(&'a str),
    WithDiag(&'a ExpectedFragmentContent<'a>, ExpectedDiagnostic<'a>),
    WithDesc(&'a ExpectedFragmentContent<'a>, SpanDesc),
    Nested(&'a [ExpectedFragmentContent<'a>]),
}

pub type ExpectedDiagnostic<'a> = (DiagnosticSeverity, &'a str);

pub fn assert_parser_output<
    'a,
    Out: PartialEq + Debug,
    P: for<'b> Parser<FragmentParserInterface<'a, 'b, Out>>,
>(
    mut parser: P,
    expected_fragments: &[ExpectedFragment<'a, Out>],
) {
    let mut flattened_fragments = Vec::new();
    flatten_fragments(expected_fragments, &mut flattened_fragments);
    let mut interface = FragmentParserInterface::new(&flattened_fragments);
    while !parser.parse(&mut interface) {}
    interface.check_empty();
}

fn flatten_fragments<'a, Out>(
    fragments: &[ExpectedFragment<'a, Out>],
    flattened_fragments: &mut Vec<FlattenedFragment<'a, Out>>,
) {
    for (content, output) in fragments {
        let start_idx = flattened_fragments.len();
        flatten_content(content, flattened_fragments);
        if let Some(output) = output {
            let start = FragmentPosition::fragment_start(start_idx);
            let end = FragmentPosition::fragment_start(flattened_fragments.len());
            flattened_fragments.last_mut().unwrap().output =
                Some(WithSpan::new(output, start..end));
        }
    }
}

fn flatten_content<'a, Out>(
    content: &ExpectedFragmentContent<'a>,
    flattened_fragments: &mut Vec<FlattenedFragment<'a, Out>>,
) {
    match *content {
        ExpectedFragmentContent::Input(input) => {
            flattened_fragments.push(FlattenedFragment {
                input,
                output: None,
                diag: Vec::new(),
                desc: Vec::new(),
            });
        }
        ExpectedFragmentContent::WithDiag(inner, diag) => {
            let start_idx = flattened_fragments.len();
            flatten_content(inner, flattened_fragments);
            let start = FragmentPosition::fragment_start(start_idx);
            let end = FragmentPosition::fragment_start(flattened_fragments.len());
            flattened_fragments
                .last_mut()
                .unwrap()
                .diag
                .push(WithSpan::new(diag, start..end));
        }
        ExpectedFragmentContent::WithDesc(inner, desc) => {
            let start_idx = flattened_fragments.len();
            flatten_content(inner, flattened_fragments);
            let start = FragmentPosition::fragment_start(start_idx);
            let end = FragmentPosition::fragment_start(flattened_fragments.len());
            flattened_fragments
                .last_mut()
                .unwrap()
                .desc
                .push(WithSpan::new(desc, start..end));
        }
        ExpectedFragmentContent::Nested(inners) => {
            for inner in inners {
                flatten_content(inner, flattened_fragments);
            }
        }
    }
}

pub mod internal {
    use std::{borrow::Cow, fmt, iter::FusedIterator, ops::Range};

    use lang_def::{
        mem_serializable::*,
        parser::{helpers::*, str::*},
    };

    use super::*;

    pub struct FlattenedFragment<'a, Out> {
        pub input: &'a str,
        pub output: Option<WithSpan<&'a Out, FragmentPosition>>,
        pub diag: Vec<WithSpan<ExpectedDiagnostic<'a>, FragmentPosition>>,
        pub desc: Vec<WithSpan<SpanDesc, FragmentPosition>>,
    }

    #[derive(Default, Clone, Copy, PartialEq)]
    pub struct FragmentPosition {
        pub fragment_idx: usize,
        pub pos: StrPosition,
    }

    impl FragmentPosition {
        pub fn fragment_start(fragment_idx: usize) -> Self {
            FragmentPosition {
                fragment_idx,
                pos: StrPosition::default(),
            }
        }
    }

    impl MemSerializable<FragmentPosition> for FragmentPosition {
        type Serialized = (
            usize,
            <StrPosition as MemSerializable<StrPosition>>::Serialized,
        );

        fn serialize(&self, relative_to: &FragmentPosition) -> Self::Serialized {
            if self.fragment_idx == relative_to.fragment_idx {
                (0, self.pos.serialize(&relative_to.pos))
            } else {
                (
                    relative_to.fragment_idx - self.fragment_idx,
                    self.pos.to_non_max_usize(),
                )
            }
        }

        fn deserialize(serialized: &Self::Serialized, relative_to: &FragmentPosition) -> Self {
            if serialized.0 == 0 {
                FragmentPosition {
                    fragment_idx: relative_to.fragment_idx,
                    pos: StrPosition::deserialize(&serialized.1, &relative_to.pos),
                }
            } else {
                FragmentPosition {
                    fragment_idx: relative_to.fragment_idx - serialized.0,
                    pos: StrPosition::from_non_max_usize(serialized.1),
                }
            }
        }
    }

    impl Position for FragmentPosition {}

    impl Debug for FragmentPosition {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            f.write_fmt(format_args!("({:?},{:?})", self.fragment_idx, self.pos))
        }
    }

    pub struct FragmentParserInterface<'a, 'b, Out> {
        iter: FragmentIter<'a, 'b, Out>,
        expected_outputs: ExpectedOutputs<'a, Out>,
    }

    impl<'a, 'b, Out> FragmentParserInterface<'a, 'b, Out> {
        pub fn new(fragments: &'b [FlattenedFragment<'a, Out>]) -> Self {
            let mut expected_outputs = ExpectedOutputs::new();
            let iter = FragmentIter::new(fragments, Some(&mut expected_outputs));
            FragmentParserInterface {
                iter,
                expected_outputs,
            }
        }

        pub fn check_empty(&self)
        where
            Out: PartialEq + Debug,
        {
            self.expected_outputs.check_empty()
        }
    }

    impl<'a, 'b, Out> Iterator for FragmentParserInterface<'a, 'b, Out> {
        type Item = WithSpan<char, FragmentPosition>;

        fn next(&mut self) -> Option<Self::Item> {
            self.iter.next(Some(&mut self.expected_outputs))
        }
    }

    impl<'a, 'b, Out> FusedIterator for FragmentParserInterface<'a, 'b, Out> {}

    impl<'a, 'b, Out> LookAhead for FragmentParserInterface<'a, 'b, Out> {
        type LookAheadItem = char;
        type ConsumedItem = WithSpan<char, FragmentPosition>;
        type NextConsumedItems = Self::ConsumedItem;

        type LookAhead<'l> = IterLookAhead<'l, Self>
        where
            Self: 'l;

        fn look_ahead(&mut self) -> Option<Self::LookAhead<'_>> {
            IterLookAhead::try_new(self)
        }
    }

    impl<'a, 'b, Out> IterLookAheadParent for FragmentParserInterface<'a, 'b, Out> {
        type Pos = FragmentPosition;
        type Item = char;
        type Iter = FragmentIter<'a, 'b, Out>;
        type NextConsumedItems = WithSpan<char, FragmentPosition>;

        fn cur_iter(&self) -> &Self::Iter {
            &self.iter
        }

        fn iter_next(&mut self, iter: &mut Self::Iter) -> Option<WithSpan<Self::Item, Self::Pos>> {
            iter.next(None)
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

    impl<'a, 'b, Out> ParserInput for FragmentParserInterface<'a, 'b, Out> {
        type In = char;
        type Pos = FragmentPosition;
    }

    impl<'a, 'b, Out> CharParserInput<'a> for FragmentParserInterface<'a, 'b, Out> {
        fn span_str(&self, span: impl Spanned<Pos = Self::Pos>) -> Cow<'a, str> {
            let fragments = self.iter.fragments;
            let Range { start, end } = span.span();
            if start.fragment_idx >= fragments.len() {
                Cow::Borrowed("")
            } else if start.fragment_idx == end.fragment_idx {
                let fragment = &fragments[start.fragment_idx];
                Cow::Borrowed(&fragment.input[start.pos.to_usize()..end.pos.to_usize()])
            } else if start.fragment_idx + 1 == end.fragment_idx && end.pos.to_usize() == 0 {
                let fragment = &fragments[start.fragment_idx];
                Cow::Borrowed(&fragment.input[start.pos.to_usize()..])
            } else {
                let mut fragment_idx = start.fragment_idx;
                let mut s = fragments[fragment_idx].input[start.pos.to_usize()..].to_owned();
                fragment_idx += 1;
                while fragment_idx < end.fragment_idx {
                    s += fragments[fragment_idx].input;
                    fragment_idx += 1;
                }
                if end.fragment_idx < fragments.len() {
                    s += &fragments[fragment_idx].input[..end.pos.to_usize()];
                }
                Cow::Owned(s)
            }
        }
    }

    impl<'a, 'b, Out: PartialEq + Debug> ParserOutput for FragmentParserInterface<'a, 'b, Out> {
        type Out = Out;
        type Pos = FragmentPosition;

        fn out(&mut self, span: impl Spanned<Pos = Self::Pos>, out: Self::Out) {
            let out = WithSpan::new(&out, span);
            let expected_outputs = &mut self.expected_outputs.expected_outputs;
            for idx in 0..expected_outputs.len() {
                if expected_outputs[idx] == out {
                    expected_outputs.remove(idx);
                    return;
                }
            }
            panic!("unexpected output {out:?}, expected any of {expected_outputs:?}");
        }
    }

    impl<'a, 'b, Out> ParserDiagnostics for FragmentParserInterface<'a, 'b, Out> {
        type Pos = FragmentPosition;

        fn diag(&mut self, span: impl Spanned<Pos = Self::Pos>, diag: Diagnostic<Self::Pos>) {
            let diag = WithSpan::new((diag.severity, diag.message.msg.as_str()), span);
            let expected_diags = &mut self.expected_outputs.expected_diags;
            for idx in 0..expected_diags.len() {
                if expected_diags[idx] == diag {
                    expected_diags.remove(idx);
                    return;
                }
            }
            panic!("unexpected diagnostic {diag:?}, expected any of {expected_diags:?}");
        }

        fn span_desc(&mut self, span: impl Spanned<Pos = Self::Pos>, desc: SpanDesc) {
            let desc = WithSpan::new(desc, span);
            let expected_descs = &mut self.expected_outputs.expected_descs;
            for idx in 0..expected_descs.len() {
                if expected_descs[idx] == desc {
                    expected_descs.remove(idx);
                    return;
                }
            }
            panic!("unexpected span description {desc:?}, expected any of {expected_descs:?}");
        }
    }

    impl<'a, 'b, Out> ParserInterfaceBase for FragmentParserInterface<'a, 'b, Out> {
        type Pos = FragmentPosition;
    }

    impl<'a, 'b, Out> ParserInputInterface for FragmentParserInterface<'a, 'b, Out> {
        type In = char;

        type Input = Self;
        type Diag = Self;

        fn input(&mut self) -> &mut Self::Input {
            self
        }

        fn diagnostics(&mut self) -> &mut Self::Diag {
            self
        }
    }

    impl<'a, 'b, Out: PartialEq + Debug> ParserOutputInterface
        for FragmentParserInterface<'a, 'b, Out>
    {
        type Out = Out;

        type Output = Self;

        fn output(&mut self) -> &mut Self::Output {
            self
        }
    }

    impl<'a, 'b, Out: PartialEq + Debug> ParserInterface for FragmentParserInterface<'a, 'b, Out> {
        fn intermediate_state_wanted(&self) -> bool {
            false
        }
    }

    pub struct FragmentIter<'a, 'b, Out> {
        fragments: &'b [FlattenedFragment<'a, Out>],
        fragment_idx: usize,
        input_iter: StrIter<'a>,
    }

    impl<'a, 'b, Out> FragmentIter<'a, 'b, Out> {
        fn new(
            fragments: &'b [FlattenedFragment<'a, Out>],
            expected_outputs: Option<&mut ExpectedOutputs<'a, Out>>,
        ) -> Self {
            let mut iter = FragmentIter {
                fragments,
                fragment_idx: 0,
                input_iter: StrIter::new(""),
            };
            iter.init_fragment(expected_outputs);
            iter
        }

        fn init_fragment(&mut self, expected_outputs: Option<&mut ExpectedOutputs<'a, Out>>) {
            let mut s = "";
            if self.fragment_idx < self.fragments.len() {
                let fragment = &self.fragments[self.fragment_idx];
                s = fragment.input;
                if let Some(expected_outputs) = expected_outputs {
                    expected_outputs.add_fragment(fragment);
                }
            }
            self.input_iter = StrIter::new(s);
        }

        fn next(
            &mut self,
            mut expected_outputs: Option<&mut ExpectedOutputs<'a, Out>>,
        ) -> Option<WithSpan<char, FragmentPosition>> {
            while self.fragment_idx < self.fragments.len() {
                if let Some(next) = self.input_iter.next(&mut 0) {
                    return Some(self.map_char_with_span(next));
                }
                self.fragment_idx += 1;
                if let Some(expected_outputs) = &mut expected_outputs {
                    self.init_fragment(Some(expected_outputs));
                } else {
                    self.init_fragment(None);
                }
            }
            None
        }

        fn map_span(&self, span: Range<StrPosition>) -> Range<FragmentPosition> {
            let start = FragmentPosition {
                fragment_idx: self.fragment_idx,
                pos: span.start,
            };
            let end = if span.end.to_usize() == self.fragments[self.fragment_idx].input.len() {
                FragmentPosition::fragment_start(self.fragment_idx + 1)
            } else {
                FragmentPosition {
                    fragment_idx: self.fragment_idx,
                    pos: span.end,
                }
            };
            start..end
        }

        fn map_char_with_span(
            &self,
            ch: WithSpan<char, StrPosition>,
        ) -> WithSpan<char, FragmentPosition> {
            WithSpan::new(*ch, self.map_span(ch.span()))
        }
    }

    impl<'a, 'b, Out> Clone for FragmentIter<'a, 'b, Out> {
        fn clone(&self) -> Self {
            Self {
                fragments: self.fragments,
                fragment_idx: self.fragment_idx,
                input_iter: self.input_iter.clone(),
            }
        }
    }

    type ExpectedOutput<T> = Vec<WithSpan<T, FragmentPosition>>;

    struct ExpectedOutputs<'a, Out> {
        expected_outputs: ExpectedOutput<&'a Out>,
        expected_diags: ExpectedOutput<ExpectedDiagnostic<'a>>,
        expected_descs: ExpectedOutput<SpanDesc>,
    }

    impl<'a, Out> ExpectedOutputs<'a, Out> {
        fn new() -> Self {
            ExpectedOutputs {
                expected_outputs: Vec::new(),
                expected_diags: Vec::new(),
                expected_descs: Vec::new(),
            }
        }

        fn add_fragment(&mut self, fragment: &FlattenedFragment<'a, Out>) {
            if let Some(output) = &fragment.output {
                self.expected_outputs.push(output.clone());
            }
            self.expected_diags.extend(fragment.diag.iter().cloned());
            self.expected_descs.extend(fragment.desc.iter().cloned());
        }

        fn check_empty(&self)
        where
            Out: PartialEq + Debug,
        {
            // If this assertion fails, the given expected outputs did not occur.
            assert_eq!(self.expected_outputs, &[]);

            // If this assertion fails, the given expected diagnostics did not occur.
            assert_eq!(self.expected_diags, &[]);

            // If this assertion fails, the given expected span descriptions did not occur.
            assert_eq!(self.expected_descs, &[]);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use lang_def::parser::testing::test_parsers::*;

    use super::*;

    #[test]
    fn tokenizer_empty() {
        assert_parser_output(Tokenizer::new(), &[]);
    }

    #[test]
    fn tokenizer_space() {
        assert_parser_output(
            Tokenizer::new(),
            &[(ExpectedFragmentContent::Input(" "), None)],
        );
    }

    #[test]
    fn tokenizer_single() {
        assert_parser_output(
            Tokenizer::new(),
            &[(
                ExpectedFragmentContent::WithDesc(
                    &ExpectedFragmentContent::Input("a"),
                    SpanDesc::String,
                ),
                Some(&Cow::Borrowed("a")),
            )],
        );
    }

    #[test]
    fn tokenizer_many() {
        assert_parser_output(
            Tokenizer::new(),
            &[
                (
                    ExpectedFragmentContent::WithDiag(
                        &ExpectedFragmentContent::Input("  "),
                        (
                            DiagnosticSeverity::Warning(None),
                            "excessive whitespace length 2",
                        ),
                    ),
                    None,
                ),
                (
                    ExpectedFragmentContent::WithDesc(
                        &ExpectedFragmentContent::Input("ab"),
                        SpanDesc::String,
                    ),
                    Some(&Cow::Borrowed("ab")),
                ),
                (ExpectedFragmentContent::Input(" "), None),
                (
                    ExpectedFragmentContent::WithDesc(
                        &ExpectedFragmentContent::Input("c"),
                        SpanDesc::String,
                    ),
                    Some(&Cow::Borrowed("c")),
                ),
                (
                    ExpectedFragmentContent::WithDiag(
                        &ExpectedFragmentContent::Input("   "),
                        (
                            DiagnosticSeverity::Warning(None),
                            "excessive whitespace length 3",
                        ),
                    ),
                    None,
                ),
                (
                    ExpectedFragmentContent::WithDiag(
                        &ExpectedFragmentContent::Input("ä"),
                        (
                            DiagnosticSeverity::Error(Some(ErrorKind::ParseError)),
                            "unexpected non-ascii character `ä`",
                        ),
                    ),
                    None,
                ),
                (
                    ExpectedFragmentContent::WithDesc(
                        &ExpectedFragmentContent::Input("def"),
                        SpanDesc::String,
                    ),
                    Some(&Cow::Borrowed("def")),
                ),
                (
                    ExpectedFragmentContent::WithDiag(
                        &ExpectedFragmentContent::Input("ö"),
                        (
                            DiagnosticSeverity::Error(Some(ErrorKind::ParseError)),
                            "unexpected non-ascii character `ö`",
                        ),
                    ),
                    None,
                ),
                (
                    ExpectedFragmentContent::WithDesc(
                        &ExpectedFragmentContent::Input("g"),
                        SpanDesc::String,
                    ),
                    Some(&Cow::Borrowed("g")),
                ),
                (
                    ExpectedFragmentContent::WithDiag(
                        &ExpectedFragmentContent::Input("  "),
                        (
                            DiagnosticSeverity::Warning(None),
                            "excessive whitespace length 2",
                        ),
                    ),
                    None,
                ),
            ],
        );
    }

    #[test]
    #[should_panic]
    fn tokenizer_unexpected_output() {
        assert_parser_output(
            Tokenizer::new(),
            &[(
                ExpectedFragmentContent::WithDesc(
                    &ExpectedFragmentContent::Input("unexpected"),
                    SpanDesc::String,
                ),
                None,
            )],
        );
    }

    #[test]
    #[should_panic]
    fn tokenizer_missing_output() {
        assert_parser_output(
            Tokenizer::new(),
            &[(
                ExpectedFragmentContent::Input(" "),
                Some(&Cow::Borrowed("missing")),
            )],
        );
    }

    #[test]
    #[should_panic]
    fn tokenizer_unexpected_diag() {
        assert_parser_output(
            Tokenizer::new(),
            &[(ExpectedFragmentContent::Input("ä"), None)],
        );
    }

    #[test]
    #[should_panic]
    fn tokenizer_missing_diag() {
        assert_parser_output(
            Tokenizer::new(),
            &[(
                ExpectedFragmentContent::WithDiag(
                    &ExpectedFragmentContent::Input(" "),
                    (DiagnosticSeverity::Error(None), "missing"),
                ),
                None,
            )],
        );
    }

    #[test]
    #[should_panic]
    fn tokenizer_unexpected_desc() {
        assert_parser_output(
            Tokenizer::new(),
            &[(
                ExpectedFragmentContent::Input("a"),
                Some(&Cow::Borrowed("a")),
            )],
        );
    }

    #[test]
    #[should_panic]
    fn tokenizer_missing_desc() {
        assert_parser_output(
            Tokenizer::new(),
            &[(
                ExpectedFragmentContent::WithDesc(
                    &ExpectedFragmentContent::Input(" "),
                    SpanDesc::Keyword,
                ),
                None,
            )],
        );
    }
}
