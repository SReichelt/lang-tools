use std::{cmp::min_by, fmt::Debug, iter::Peekable, ops::Range, slice};

use lang_def::parser::{str::*, testing::str::*, *};

use internal::*;

pub type ExpectedFragment<'a, Out> = (ExpectedFragmentContent<'a>, Option<Out>);

#[derive(Debug)]
pub enum ExpectedFragmentContent<'a> {
    Empty,
    Input(&'a str),
    WithDiag(Box<ExpectedFragmentContent<'a>>, ExpectedDiagnostic),
    WithDesc(Box<ExpectedFragmentContent<'a>>, SpanDesc),
    Seq(Vec<ExpectedFragmentContent<'a>>),
}

pub type ExpectedDiagnostic = (DiagnosticSeverity, String);

/// Checks that a parser produces a given output (including diagnostics and span descriptions) for
/// a given input. The combination of input and expected output is declared as a list of
/// [`ExpectedFragment`].
///
/// For easy construction of the arguments to this function, use [`construct_parser_output`] or
/// [`print_parser_output`].
pub fn assert_parser_output<
    'a,
    Desc: CharParserDesc<Out<'a, StrPosition>: PartialEq + Debug> + 'a,
>(
    expected_fragments: Vec<ExpectedFragment<'a, Desc::Out<'a, StrPosition>>>,
    mut config: Desc::Config<'a>,
) {
    let mut pos = 0;
    let mut pre_fragment = FlattenedFragment::new(pos, "");
    let mut flattened_fragments = Vec::new();
    flatten_fragments(
        expected_fragments,
        &mut pos,
        &mut pre_fragment,
        &mut flattened_fragments,
    );
    let mut interface =
        FragmentParserInterface::new(pos, &pre_fragment, &flattened_fragments, &mut config);
    let mut parser = Desc::parser(&interface);
    while !parser.parse(&mut interface) {}
    interface.check_empty();
}

fn flatten_fragments<'a, Out>(
    fragments: Vec<ExpectedFragment<'a, Out>>,
    pos: &mut usize,
    pre_fragment: &mut FlattenedFragment<'a, Out>,
    flattened_fragments: &mut Vec<FlattenedFragment<'a, Out>>,
) {
    let mut paren_level = 0;
    print!("parsing: ");
    for (content, output) in fragments {
        let start_pos = *pos;
        flatten_content(
            content,
            pos,
            pre_fragment,
            flattened_fragments,
            &mut paren_level,
        );
        if let Some(output) = output {
            flattened_fragments
                .last_mut()
                .unwrap_or(pre_fragment)
                .output
                .push(WithSpan::new(
                    output,
                    StrPosition::span_from_range(start_pos..*pos),
                ));
        }
    }
    println!("");
    assert!(
        paren_level == 0,
        "unmatched opening parenthesis in expected span description"
    );
}

fn flatten_content<'a, Out>(
    content: ExpectedFragmentContent<'a>,
    pos: &mut usize,
    pre_fragment: &mut FlattenedFragment<'a, Out>,
    flattened_fragments: &mut Vec<FlattenedFragment<'a, Out>>,
    paren_level: &mut usize,
) {
    match content {
        ExpectedFragmentContent::Empty => {}
        ExpectedFragmentContent::Input(input) => {
            if !input.is_empty() {
                flattened_fragments.push(FlattenedFragment::new(*pos, input));
                *pos += input.len();
                print!("{input}");
            }
        }
        ExpectedFragmentContent::WithDiag(inner, diag) => {
            let start_pos = *pos;
            flatten_content(*inner, pos, pre_fragment, flattened_fragments, paren_level);
            flattened_fragments
                .last_mut()
                .unwrap_or(pre_fragment)
                .diag
                .push(WithSpan::new(
                    diag,
                    StrPosition::span_from_range(start_pos..*pos),
                ));
        }
        ExpectedFragmentContent::WithDesc(inner, desc) => {
            let start_pos = *pos;
            flatten_content(*inner, pos, pre_fragment, flattened_fragments, paren_level);
            match &desc {
                SpanDesc::ParenStart => {
                    *paren_level += 1;
                }
                SpanDesc::ParenEnd => {
                    assert!(
                        *paren_level > 0,
                        "unmatched closing parenthesis in expected span description"
                    );
                    *paren_level -= 1;
                }
                _ => {}
            }
            flattened_fragments
                .last_mut()
                .unwrap_or(pre_fragment)
                .desc
                .push(WithSpan::new(
                    desc,
                    StrPosition::span_from_range(start_pos..*pos),
                ));
        }
        ExpectedFragmentContent::Seq(inners) => {
            for inner in inners {
                flatten_content(inner, pos, pre_fragment, flattened_fragments, paren_level);
            }
        }
    }
}

/// Constructs the required input for [`assert_parser_output`] to match the current output produced
/// by the parser for the given input.
pub fn construct_parser_output<'a, Desc: CharParserDesc>(
    input: &'a str,
    config: Desc::Config<'a>,
) -> Vec<ExpectedFragment<'a, Desc::Out<'a, StrPosition>>> {
    println!("parsing: {input}");

    let (mut output, mut diag) = parse_all::<Desc>(input, config);

    sort_by_spans(&mut output.output);
    sort_by_spans(&mut diag.diag);
    sort_by_spans(&mut diag.desc);

    let mut fragments = Vec::new();
    let mut pos = StrPosition::from_usize(0);
    let mut diag_iter = diag.diag.iter().peekable();
    let mut desc_iter = diag.desc.iter().peekable();
    for out in output.output {
        let span = out.span();
        if pos < span.start {
            append_fragment_content::<Desc>(
                input,
                pos..span.start,
                &mut diag_iter,
                &mut desc_iter,
                &mut fragments,
            );
            pos = span.start;
        }
        assert!(
            pos <= span.start,
            "overlapping output spans are not supported ({span:?} after {pos:?})"
        );
        fragments.push((
            construct_fragment_content(input, span.clone(), &mut diag_iter, &mut desc_iter),
            Some(out.into_inner()),
        ));
        pos = span.end;
    }
    let len = StrPosition::from_usize(input.len());
    if pos < len || diag_iter.peek().is_some() || desc_iter.peek().is_some() {
        append_fragment_content::<Desc>(
            input,
            pos..len,
            &mut diag_iter,
            &mut desc_iter,
            &mut fragments,
        );
    }
    fragments
}

fn append_fragment_content<'a, Desc: CharParserDesc>(
    input: &'a str,
    span: Range<StrPosition>,
    diag_iter: &mut Peekable<slice::Iter<WithSpan<Diagnostic<StrPosition>, StrPosition>>>,
    desc_iter: &mut Peekable<slice::Iter<WithSpan<SpanDesc, StrPosition>>>,
    fragments: &mut Vec<ExpectedFragment<'a, Desc::Out<'a, StrPosition>>>,
) {
    match construct_fragment_content(input, span, diag_iter, desc_iter) {
        ExpectedFragmentContent::Empty => {}
        ExpectedFragmentContent::Seq(inners) => {
            fragments.extend(inners.into_iter().map(|inner| (inner, None)))
        }
        content => fragments.push((content, None)),
    }
}

fn construct_fragment_content<'a>(
    input: &'a str,
    span: Range<StrPosition>,
    diag_iter: &mut Peekable<slice::Iter<WithSpan<Diagnostic<StrPosition>, StrPosition>>>,
    desc_iter: &mut Peekable<slice::Iter<WithSpan<SpanDesc, StrPosition>>>,
) -> ExpectedFragmentContent<'a> {
    let next_diag_span = diag_iter.peek().map(|diag| diag.span());
    if next_diag_span == Some(span.clone()) {
        let diag = diag_iter.next().unwrap();
        return ExpectedFragmentContent::WithDiag(
            Box::new(construct_fragment_content(
                input, span, diag_iter, desc_iter,
            )),
            (diag.severity, diag.message.msg.clone()),
        );
    }
    let next_desc_span = desc_iter.peek().map(|desc| desc.span());
    if next_desc_span == Some(span.clone()) {
        let desc = desc_iter.next().unwrap();
        return ExpectedFragmentContent::WithDesc(
            Box::new(construct_fragment_content(
                input, span, diag_iter, desc_iter,
            )),
            **desc,
        );
    }
    let next_span = if let Some(next_diag_span) = next_diag_span {
        if let Some(next_desc_span) = next_desc_span {
            Some(min_by(next_diag_span, next_desc_span, cmp_spans))
        } else {
            Some(next_diag_span)
        }
    } else {
        next_desc_span
    };
    if let Some(next_span) = next_span {
        if next_span.end <= span.end {
            assert!(
                next_span.start >= span.start,
                "partially overlapping spans are not supported ({span:?} and {next_span:?})"
            );
            let mut fragments = Vec::new();
            if next_span.start > span.start {
                fragments.push(ExpectedFragmentContent::Input(
                    &input[StrPosition::span_to_range(span.start..next_span.start)],
                ));
            }
            fragments.push(construct_fragment_content(
                input,
                next_span.clone(),
                diag_iter,
                desc_iter,
            ));
            match construct_fragment_content(input, next_span.end..span.end, diag_iter, desc_iter) {
                ExpectedFragmentContent::Empty => {}
                ExpectedFragmentContent::Seq(inners) => fragments.extend(inners),
                single => fragments.push(single),
            }
            return ExpectedFragmentContent::Seq(fragments);
        }
    }
    if span.is_empty() {
        ExpectedFragmentContent::Empty
    } else {
        ExpectedFragmentContent::Input(&input[StrPosition::span_to_range(span)])
    }
}

/// Prints the result of [`construct_parser_output`] on the console.
pub fn print_parser_output<'a, Desc: CharParserDesc<Out<'a, StrPosition>: Debug>>(
    input: &'a str,
    config: Desc::Config<'a>,
) {
    let fragments = construct_parser_output::<Desc>(input, config);
    println!("{fragments:#?}");
}

mod internal {
    use std::{borrow::Cow, cmp::Ordering, iter::FusedIterator, ops::Range};

    use lang_def::parser::helpers::*;

    use super::*;

    pub struct FlattenedFragment<'a, Out> {
        pub start_pos: usize,
        pub input: &'a str,
        pub output: Vec<WithSpan<Out, StrPosition>>,
        pub diag: Vec<WithSpan<ExpectedDiagnostic, StrPosition>>,
        pub desc: Vec<WithSpan<SpanDesc, StrPosition>>,
    }

    impl<'a, Out> FlattenedFragment<'a, Out> {
        pub fn new(start_pos: usize, input: &'a str) -> Self {
            FlattenedFragment {
                start_pos,
                input,
                output: Vec::new(),
                diag: Vec::new(),
                desc: Vec::new(),
            }
        }
    }

    pub struct FragmentParserInterface<'a, 'b, Out, Config> {
        config: &'b mut Config,
        iter: FragmentIter<'a, 'b, Out>,
        expected_outputs: ExpectedOutputs<'b, Out>,
    }

    impl<'a, 'b, Out, Config> FragmentParserInterface<'a, 'b, Out, Config> {
        pub fn new(
            input_len: usize,
            pre_fragment: &'b FlattenedFragment<'a, Out>,
            fragments: &'b [FlattenedFragment<'a, Out>],
            config: &'b mut Config,
        ) -> Self {
            let mut expected_outputs = ExpectedOutputs::new();
            expected_outputs.add_fragment(pre_fragment);
            let iter = FragmentIter::new(input_len, fragments, Some(&mut expected_outputs));
            FragmentParserInterface {
                config,
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

    impl<'a, 'b, Out, Config> Iterator for FragmentParserInterface<'a, 'b, Out, Config> {
        type Item = WithSpan<char, StrPosition>;

        fn next(&mut self) -> Option<Self::Item> {
            self.iter.next(Some(&mut self.expected_outputs))
        }
    }

    impl<'a, 'b, Out, Config> FusedIterator for FragmentParserInterface<'a, 'b, Out, Config> {}

    impl<'a, 'b, Out, Config> LookAhead for FragmentParserInterface<'a, 'b, Out, Config> {
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

    impl<'a, 'b, Out, Config> IterLookAheadParent for FragmentParserInterface<'a, 'b, Out, Config> {
        type Pos = StrPosition;
        type Item = char;
        type Iter = FragmentIter<'a, 'b, Out>;
        type NextConsumedItems = WithSpan<char, StrPosition>;

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
            self.iter.advance_to(iter, Some(&mut self.expected_outputs));
            additional_item
        }
    }

    impl<'a, 'b, Out, Config> ParserInput for FragmentParserInterface<'a, 'b, Out, Config> {
        type In = char;
        type Pos = StrPosition;

        fn pos(&mut self) -> Self::Pos {
            self.iter.pos_as_str_position()
        }
    }

    impl<'a, 'b, Out, Config> CharParserInput<'a> for FragmentParserInterface<'a, 'b, Out, Config> {
        fn span_str(&self, span: impl Spanned<Pos = Self::Pos>) -> Cow<'a, str> {
            let mut range = StrPosition::span_to_range(span.span());
            let fragments = self.iter.fragments;
            let mut fragment_idx = fragments.partition_point(|fragment| {
                fragment.start_pos + fragment.input.len() <= range.start
            });
            if fragment_idx >= fragments.len() {
                assert_eq!(range.start, self.iter.input_len);
                assert_eq!(range.end, self.iter.input_len);
                return Cow::Borrowed("");
            }
            let fragment = &fragments[fragment_idx];
            range.start -= fragment.start_pos;
            range.end -= fragment.start_pos;
            let mut fragment_len = fragment.input.len();
            assert!(range.start < fragment_len);
            if range.end <= fragment_len {
                return Cow::Borrowed(&fragment.input[range]);
            }
            let mut s = fragment.input[range.start..].to_owned();
            loop {
                fragment_idx += 1;
                range.end -= fragment_len;
                let fragment = &fragments[fragment_idx];
                fragment_len = fragment.input.len();
                if range.end <= fragment_len {
                    s += &fragment.input[..range.end];
                    return Cow::Owned(s);
                }
                s += fragment.input;
            }
        }
    }

    impl<'a, 'b, Out: PartialEq + Debug, Config> ParserOutput
        for FragmentParserInterface<'a, 'b, Out, Config>
    {
        type Out = Out;
        type Pos = StrPosition;

        fn out(&mut self, span: impl Spanned<Pos = Self::Pos>, out: Self::Out) {
            let out = WithSpan::new(out, span);
            let s = self.span_str(&out).into_owned();
            let expected_outputs = &mut self.expected_outputs.expected_outputs;
            for idx in 0..expected_outputs.len() {
                if expected_outputs[idx] == &out {
                    expected_outputs.remove(idx);
                    return;
                }
            }
            panic!("unexpected output {out:?} for {s:?}, expected any of {expected_outputs:?}");
        }
    }

    impl<'a, 'b, Out, Config> ParserDiagnostics for FragmentParserInterface<'a, 'b, Out, Config> {
        type Pos = StrPosition;

        fn diag(&mut self, span: impl Spanned<Pos = Self::Pos>, diag: Diagnostic<Self::Pos>) {
            let diag = WithSpan::new((diag.severity, diag.message.msg), span);
            let s = self.span_str(&diag).into_owned();
            let expected_diags = &mut self.expected_outputs.expected_diags;
            for idx in 0..expected_diags.len() {
                if expected_diags[idx] == &diag {
                    expected_diags.remove(idx);
                    return;
                }
            }
            panic!("unexpected diagnostic {diag:?} for {s:?}, expected any of {expected_diags:?}");
        }

        fn span_desc(&mut self, span: impl Spanned<Pos = Self::Pos>, desc: SpanDesc) {
            let desc = WithSpan::new(desc, span);
            let s = self.span_str(&desc).into_owned();
            let expected_descs = &mut self.expected_outputs.expected_descs;
            for idx in 0..expected_descs.len() {
                if expected_descs[idx] == &desc {
                    expected_descs.remove(idx);
                    return;
                }
            }
            panic!("unexpected span description {desc:?} for {s:?}, expected any of {expected_descs:?}");
        }
    }

    impl<'a, 'b, Out, Config> ParserInterfaceBase for FragmentParserInterface<'a, 'b, Out, Config> {
        type Config = Config;
        type Pos = StrPosition;

        fn config(&self) -> &Self::Config {
            self.config
        }

        fn modify_config<R>(&mut self, f: impl FnOnce(&mut Self::Config) -> R) -> R {
            f(self.config)
        }
    }

    impl<'a, 'b, Out, Config> ParserInputInterface for FragmentParserInterface<'a, 'b, Out, Config> {
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

    impl<'a, 'b, Out: PartialEq + Debug, Config> ParserOutputInterface
        for FragmentParserInterface<'a, 'b, Out, Config>
    {
        type Out = Out;

        type Output = Self;

        fn output(&mut self) -> &mut Self::Output {
            self
        }
    }

    impl<'a, 'b, Out: PartialEq + Debug, Config> ParserInterface
        for FragmentParserInterface<'a, 'b, Out, Config>
    {
    }

    pub struct FragmentIter<'a, 'b, Out> {
        input_len: usize,
        fragments: &'b [FlattenedFragment<'a, Out>],
        fragment_idx: usize,
        input_iter: StrIter<'a>,
    }

    impl<'a, 'b, Out> FragmentIter<'a, 'b, Out> {
        fn new(
            input_len: usize,
            fragments: &'b [FlattenedFragment<'a, Out>],
            expected_outputs: Option<&mut ExpectedOutputs<'b, Out>>,
        ) -> Self {
            let mut iter = FragmentIter {
                input_len,
                fragments,
                fragment_idx: 0,
                input_iter: StrIter::new(""),
            };
            iter.init_fragment(expected_outputs);
            iter
        }

        fn init_fragment(&mut self, expected_outputs: Option<&mut ExpectedOutputs<'b, Out>>) {
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
            mut expected_outputs: Option<&mut ExpectedOutputs<'b, Out>>,
        ) -> Option<WithSpan<char, StrPosition>> {
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

        fn advance_to(
            &mut self,
            iter: Self,
            mut expected_outputs: Option<&mut ExpectedOutputs<'b, Out>>,
        ) {
            while self.fragment_idx < iter.fragment_idx {
                self.fragment_idx += 1;
                if let Some(expected_outputs) = &mut expected_outputs {
                    self.init_fragment(Some(expected_outputs));
                } else {
                    self.init_fragment(None);
                }
            }
            while self.input_iter.pos() < iter.input_iter.pos() {
                self.input_iter.next(&mut 0);
            }
        }

        fn map_pos(&self, pos: StrPosition) -> StrPosition {
            let start_pos = if self.fragment_idx < self.fragments.len() {
                self.fragments[self.fragment_idx].start_pos
            } else {
                self.input_len
            };
            StrPosition::from_usize(start_pos + pos.to_usize())
        }

        fn map_span(&self, span: Range<StrPosition>) -> Range<StrPosition> {
            let start = self.map_pos(span.start);
            let end = self.map_pos(span.end);
            start..end
        }

        fn map_char_with_span(
            &self,
            ch: WithSpan<char, StrPosition>,
        ) -> WithSpan<char, StrPosition> {
            WithSpan::new(*ch, self.map_span(ch.span()))
        }

        fn pos_as_str_position(&mut self) -> StrPosition {
            self.map_pos(self.input_iter.pos_as_str_position())
        }
    }

    impl<'a, 'b, Out> Clone for FragmentIter<'a, 'b, Out> {
        fn clone(&self) -> Self {
            FragmentIter {
                input_len: self.input_len,
                fragments: self.fragments,
                fragment_idx: self.fragment_idx,
                input_iter: self.input_iter.clone(),
            }
        }
    }

    type ExpectedOutput<'b, T> = Vec<&'b WithSpan<T, StrPosition>>;

    struct ExpectedOutputs<'b, Out> {
        expected_outputs: ExpectedOutput<'b, Out>,
        expected_diags: ExpectedOutput<'b, ExpectedDiagnostic>,
        expected_descs: ExpectedOutput<'b, SpanDesc>,
    }

    impl<'b, Out> ExpectedOutputs<'b, Out> {
        fn new() -> Self {
            ExpectedOutputs {
                expected_outputs: Vec::new(),
                expected_diags: Vec::new(),
                expected_descs: Vec::new(),
            }
        }

        fn add_fragment(&mut self, fragment: &'b FlattenedFragment<Out>) {
            self.expected_outputs.extend(fragment.output.iter());
            self.expected_diags.extend(fragment.diag.iter());
            self.expected_descs.extend(fragment.desc.iter());
        }

        fn check_empty(&self)
        where
            Out: PartialEq + Debug,
        {
            // If this assertion fails, the given expected outputs did not occur.
            let expected_outputs = &self.expected_outputs;
            assert!(
                expected_outputs.is_empty(),
                "missing output(s): {expected_outputs:?}"
            );

            // If this assertion fails, the given expected diagnostics did not occur.
            let expected_diags = &self.expected_diags;
            assert!(
                expected_diags.is_empty(),
                "missing diagnostic(s): {expected_diags:?}"
            );

            // If this assertion fails, the given expected span descriptions did not occur.
            let expected_descs = &self.expected_descs;
            assert!(
                expected_descs.is_empty(),
                "missing span description(s): {expected_descs:?}"
            );
        }
    }

    pub fn cmp_spans(l: &Range<StrPosition>, r: &Range<StrPosition>) -> Ordering {
        let start_order = l.start.cmp(&r.start);
        if start_order != Ordering::Equal {
            return start_order;
        }
        r.end.cmp(&l.end)
    }

    pub fn sort_by_spans<T>(items: &mut [WithSpan<T, StrPosition>]) {
        items.sort_by(|l, r| cmp_spans(&l.span(), &r.span()))
    }
}

#[cfg(test)]
mod tests {
    use lang_def::parser::testing::test_parsers::*;

    use super::*;

    fn assert_tokenizer_output(expected_fragments: Vec<ExpectedFragment<Token>>) {
        assert_parser_output::<TokenizerConfig>(expected_fragments, TokenizerConfig)
    }

    #[test]
    fn tokenizer_empty() {
        assert_tokenizer_output(Vec::new());
    }

    #[test]
    fn tokenizer_space() {
        assert_tokenizer_output(vec![(ExpectedFragmentContent::Input(" "), None)]);
    }

    #[test]
    fn tokenizer_single() {
        assert_tokenizer_output(vec![(
            ExpectedFragmentContent::WithDesc(
                Box::new(ExpectedFragmentContent::Input("a")),
                SpanDesc::String,
            ),
            Some("a".into()),
        )]);
    }

    #[test]
    fn tokenizer_many() {
        assert_tokenizer_output(vec![
            (
                ExpectedFragmentContent::WithDiag(
                    Box::new(ExpectedFragmentContent::Input("  ")),
                    (
                        DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        "excessive whitespace length 2".into(),
                    ),
                ),
                None,
            ),
            (
                ExpectedFragmentContent::WithDesc(
                    Box::new(ExpectedFragmentContent::Input("ab")),
                    SpanDesc::String,
                ),
                Some("ab".into()),
            ),
            (ExpectedFragmentContent::Input(" "), None),
            (
                ExpectedFragmentContent::WithDesc(
                    Box::new(ExpectedFragmentContent::Input("c")),
                    SpanDesc::String,
                ),
                Some("c".into()),
            ),
            (
                ExpectedFragmentContent::WithDiag(
                    Box::new(ExpectedFragmentContent::Input("   ")),
                    (
                        DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        "excessive whitespace length 3".into(),
                    ),
                ),
                None,
            ),
            (
                ExpectedFragmentContent::WithDiag(
                    Box::new(ExpectedFragmentContent::Input("ä")),
                    (
                        DiagnosticSeverity::Error(Some(ErrorKind::SyntaxError)),
                        "unexpected non-ascii character `ä`".into(),
                    ),
                ),
                None,
            ),
            (
                ExpectedFragmentContent::WithDesc(
                    Box::new(ExpectedFragmentContent::Input("def")),
                    SpanDesc::String,
                ),
                Some("def".into()),
            ),
            (
                ExpectedFragmentContent::WithDiag(
                    Box::new(ExpectedFragmentContent::Input("ö")),
                    (
                        DiagnosticSeverity::Error(Some(ErrorKind::SyntaxError)),
                        "unexpected non-ascii character `ö`".into(),
                    ),
                ),
                None,
            ),
            (
                ExpectedFragmentContent::WithDesc(
                    Box::new(ExpectedFragmentContent::Input("g")),
                    SpanDesc::String,
                ),
                Some("g".into()),
            ),
            (
                ExpectedFragmentContent::WithDiag(
                    Box::new(ExpectedFragmentContent::Input("  ")),
                    (
                        DiagnosticSeverity::Warning(Some(WarningKind::SyntaxWarning)),
                        "excessive whitespace length 2".into(),
                    ),
                ),
                None,
            ),
        ]);
    }

    #[test]
    #[should_panic]
    fn tokenizer_unexpected_output() {
        assert_tokenizer_output(vec![(
            ExpectedFragmentContent::WithDesc(
                Box::new(ExpectedFragmentContent::Input("unexpected")),
                SpanDesc::String,
            ),
            None,
        )]);
    }

    #[test]
    #[should_panic]
    fn tokenizer_missing_output() {
        assert_tokenizer_output(vec![(
            ExpectedFragmentContent::Input(" "),
            Some("missing".into()),
        )]);
    }

    #[test]
    #[should_panic]
    fn tokenizer_unexpected_diag() {
        assert_tokenizer_output(vec![(ExpectedFragmentContent::Input("ä"), None)]);
    }

    #[test]
    #[should_panic]
    fn tokenizer_missing_diag() {
        assert_tokenizer_output(vec![(
            ExpectedFragmentContent::WithDiag(
                Box::new(ExpectedFragmentContent::Input(" ")),
                (DiagnosticSeverity::Error(None), "missing".into()),
            ),
            None,
        )]);
    }

    #[test]
    #[should_panic]
    fn tokenizer_unexpected_desc() {
        assert_tokenizer_output(vec![(
            ExpectedFragmentContent::Input("a"),
            Some("a".into()),
        )]);
    }

    #[test]
    #[should_panic]
    fn tokenizer_missing_desc() {
        assert_tokenizer_output(vec![(
            ExpectedFragmentContent::WithDesc(
                Box::new(ExpectedFragmentContent::Input(" ")),
                SpanDesc::Keyword,
            ),
            None,
        )]);
    }

    fn construct_tokenizer_output(input: &str) -> Vec<ExpectedFragment<Token>> {
        construct_parser_output::<TokenizerConfig>(input, TokenizerConfig)
    }

    #[test]
    fn tokenizer_construct() {
        let fragments = construct_tokenizer_output("a  b");
        assert_eq!(
            format!("{fragments:?}"),
            "[(WithDesc(Input(\"a\"), String), Some(\"a\")), (WithDiag(Input(\"  \"), (Warning(Some(SyntaxWarning)), \"excessive whitespace length 2\")), None), (WithDesc(Input(\"b\"), String), Some(\"b\"))]"
        );
    }
}
