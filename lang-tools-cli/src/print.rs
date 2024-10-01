use std::{borrow::Cow, io};

use lang_def::{
    parser::{
        impls::str::parse_all, str::StrPosition, CharParserDesc, Diagnostic, DiagnosticSeverity,
        NameKindDesc, NameScopeDesc, SpanDesc, Spanned, WithSpan,
    },
    parser_tools::flatten_span_descs,
    LanguageDefinition,
};
use termcolor::{Color, ColorSpec, WriteColor};

pub trait OutputStyle {
    fn tab_size(&self) -> usize;

    fn line_numbers(&self) -> Option<Cow<LineNumbersSpec>>;

    fn diags(&self, severity: DiagnosticSeverity) -> Option<Cow<DiagsSpec>>;

    fn parens(&self, level: usize) -> Cow<ColorSpec>;
    fn comments(&self) -> Cow<ColorSpec>;
    fn keywords(&self) -> Cow<ColorSpec>;
    fn numbers(&self) -> Cow<ColorSpec>;
    fn strings(&self) -> Cow<ColorSpec>;
    fn name_defs(&self, scope: NameScopeDesc, kind: Option<NameKindDesc>) -> Cow<ColorSpec>;
    fn name_refs(&self, scope: NameScopeDesc, kind: Option<NameKindDesc>) -> Cow<ColorSpec>;
}

#[derive(Default, Clone)]
pub struct LineNumbersSpec {
    pub color: ColorSpec,
    pub separator: String,
}

impl LineNumbersSpec {
    pub fn standard() -> Self {
        let mut color = ColorSpec::new();
        color.set_dimmed(true);
        LineNumbersSpec {
            color,
            separator: " ┆ ".into(),
        }
    }
}

#[derive(Default, Clone)]
pub struct DiagsSpec {
    pub diag_color: ColorSpec,
    pub indicator: char,
    pub squiggle: char,
    pub cont_end: char,
    pub cont_start: char,
    pub msg_color: ColorSpec,
}

impl DiagsSpec {
    pub fn standard(diag_color: Color) -> Self {
        let mut diag_color = color(diag_color);
        diag_color.set_bold(true);
        let mut msg_color = ColorSpec::new();
        msg_color.set_bold(true);
        DiagsSpec {
            diag_color,
            indicator: '!',
            squiggle: '~',
            cont_end: '>',
            cont_start: '>',
            msg_color,
        }
    }
}

#[derive(Default, Clone)]
pub struct StandardOutputStyle {
    pub tab_size: usize,
    pub line_numbers: Option<LineNumbersSpec>,
    pub errors: Option<DiagsSpec>,
    pub warnings: Option<DiagsSpec>,
    pub infos: Option<DiagsSpec>,
    pub parens: Vec<ColorSpec>,
    pub comments: ColorSpec,
    pub keywords: ColorSpec,
    pub numbers: ColorSpec,
    pub strings: ColorSpec,
    pub value_names: ColorSpec,
    pub fn_names: ColorSpec,
    pub type_names: ColorSpec,
    pub scope_overrides: ScopeOverrides,
}

impl StandardOutputStyle {
    pub fn standard() -> Self {
        let mut comments = color(Color::Green);
        comments.set_italic(true);
        StandardOutputStyle {
            tab_size: 4,
            line_numbers: Some(LineNumbersSpec::standard()),
            errors: Some(DiagsSpec::standard(Color::Red)),
            warnings: Some(DiagsSpec::standard(Color::Yellow)),
            infos: Some(DiagsSpec::standard(Color::Blue)),
            parens: vec![color(Color::Blue), color(Color::Green), color(Color::Red)],
            comments,
            keywords: color(Color::Blue),
            numbers: color(Color::Red),
            strings: color(Color::Red),
            value_names: ColorSpec::new(),
            fn_names: color(Color::Magenta),
            type_names: color(Color::Cyan),
            scope_overrides: ScopeOverrides::standard(),
        }
    }
}

impl OutputStyle for StandardOutputStyle {
    fn tab_size(&self) -> usize {
        self.tab_size
    }

    fn line_numbers(&self) -> Option<Cow<LineNumbersSpec>> {
        self.line_numbers.as_ref().map(Cow::Borrowed)
    }

    fn diags(&self, severity: DiagnosticSeverity) -> Option<Cow<DiagsSpec>> {
        match severity {
            DiagnosticSeverity::Error(_) => self.errors.as_ref().map(Cow::Borrowed),
            DiagnosticSeverity::Warning(_) => self.warnings.as_ref().map(Cow::Borrowed),
            DiagnosticSeverity::Info => self.infos.as_ref().map(Cow::Borrowed),
            _ => None,
        }
    }

    fn parens(&self, level: usize) -> Cow<ColorSpec> {
        let len = self.parens.len();
        if len == 0 {
            Cow::Owned(ColorSpec::new())
        } else {
            Cow::Borrowed(&self.parens[level % len])
        }
    }

    fn comments(&self) -> Cow<ColorSpec> {
        Cow::Borrowed(&self.comments)
    }

    fn keywords(&self) -> Cow<ColorSpec> {
        Cow::Borrowed(&self.keywords)
    }

    fn numbers(&self) -> Cow<ColorSpec> {
        Cow::Borrowed(&self.numbers)
    }

    fn strings(&self) -> Cow<ColorSpec> {
        Cow::Borrowed(&self.strings)
    }

    fn name_defs(&self, scope: NameScopeDesc, kind: Option<NameKindDesc>) -> Cow<ColorSpec> {
        let kind_spec = match kind {
            Some(NameKindDesc::Value) => Cow::Borrowed(&self.value_names),
            Some(NameKindDesc::Function) => Cow::Borrowed(&self.fn_names),
            Some(NameKindDesc::Type) | Some(NameKindDesc::GenericType) => {
                Cow::Borrowed(&self.type_names)
            }
            _ => Cow::Owned(ColorSpec::new()),
        };
        self.scope_overrides.color_spec(kind_spec, scope)
    }

    fn name_refs(&self, scope: NameScopeDesc, kind: Option<NameKindDesc>) -> Cow<ColorSpec> {
        self.name_defs(scope, kind)
    }
}

#[derive(Default, Clone)]
pub struct ScopeOverrides {
    pub global: Option<ColorSpec>,
    pub instance: Option<ColorSpec>,
    pub field: Option<ColorSpec>,
    pub param: Option<ColorSpec>,
    pub local: Option<ColorSpec>,
}

impl ScopeOverrides {
    pub fn standard() -> Self {
        ScopeOverrides {
            global: None,
            instance: Some(color(Color::Yellow)),
            field: None,
            param: None,
            local: None,
        }
    }

    pub fn scope_override(&self, scope: NameScopeDesc) -> Option<&ColorSpec> {
        match scope {
            NameScopeDesc::Global => self.global.as_ref(),
            NameScopeDesc::Instance => self.instance.as_ref(),
            NameScopeDesc::Field => self.field.as_ref(),
            NameScopeDesc::Param => self.param.as_ref(),
            NameScopeDesc::Local => self.local.as_ref(),
            _ => None,
        }
    }

    pub fn color_spec<'a>(
        &self,
        orig: Cow<'a, ColorSpec>,
        scope: NameScopeDesc,
    ) -> Cow<'a, ColorSpec> {
        if let Some(over) = self.scope_override(scope) {
            let mut result = orig.into_owned();
            if let Some(fg) = over.fg() {
                result.set_fg(Some(*fg));
            }
            if let Some(bg) = over.bg() {
                result.set_bg(Some(*bg));
            }
            if over.bold() {
                result.set_bold(true);
            }
            if over.dimmed() {
                result.set_dimmed(true);
            }
            if over.italic() {
                result.set_italic(true);
            }
            if over.underline() {
                result.set_underline(true);
            }
            if over.strikethrough() {
                result.set_strikethrough(true);
            }
            if over.reset() {
                result.set_reset(true);
            }
            if over.intense() {
                result.set_intense(true);
            }
            Cow::Owned(result)
        } else {
            orig
        }
    }
}

fn color(color: Color) -> ColorSpec {
    let mut color_spec = ColorSpec::new();
    color_spec.set_fg(Some(color));
    color_spec
}

pub fn pretty_print<'a, Lang: LanguageDefinition, W: WriteColor>(
    input: &'a str,
    config: <Lang::ParserDesc as CharParserDesc>::Config<'a>,
    style: &impl OutputStyle,
    w: &mut W,
) -> io::Result<()> {
    let (_, mut diag) = parse_all::<Lang::ParserDesc>(input, config);

    let fragments = flatten_span_descs(&mut diag.desc);
    WithSpan::sort_by_spans(&mut diag.diag);

    let tab_size = style.tab_size();
    let line_numbers = if let Some(line_numbers_style) = style.line_numbers() {
        let line_count = input.split_inclusive('\n').count();
        let digits = line_count.ilog10() as usize + 1;
        Some((line_numbers_style, digits))
    } else {
        None
    };

    let mut paren_level = 0;
    let mut begin_desc = |span_desc: Option<&SpanDesc>| {
        if let Some(span_desc) = span_desc {
            let color_spec = match span_desc {
                SpanDesc::ParenStart => {
                    let color_spec = style.parens(paren_level);
                    paren_level += 1;
                    color_spec
                }
                SpanDesc::ParenMid => style.parens(paren_level - 1),
                SpanDesc::ParenEnd => {
                    paren_level -= 1;
                    style.parens(paren_level)
                }
                SpanDesc::Comment => style.comments(),
                SpanDesc::Keyword => style.keywords(),
                SpanDesc::Number => style.numbers(),
                SpanDesc::String => style.strings(),
                SpanDesc::NameDef(scope, kind) => style.name_defs(*scope, *kind),
                SpanDesc::NameRef(scope, kind) => style.name_refs(*scope, *kind),
                _ => Cow::Owned(ColorSpec::new()),
            };
            Some(color_spec)
        } else {
            None
        }
    };

    let mut line_start: usize = 0;
    let mut line_idx: usize = 0;
    let mut fragment_iter = fragments.iter();
    let mut cur_fragment = fragment_iter.next();
    let mut cur_color_spec =
        begin_desc(cur_fragment.and_then(|fragment| fragment.span_desc.as_ref()));
    let mut diag_iter = diag.diag.iter();
    let mut cur_diag = diag_iter.next();
    let mut open_diags: Vec<&WithSpan<Diagnostic<StrPosition>, StrPosition>> = Vec::new();

    for line in input.split_inclusive('\n') {
        let line_end = line_start + line.len();

        if let Some((line_numbers_spec, digits)) = &line_numbers {
            w.set_color(&line_numbers_spec.color)?;
            let line_number = line_idx + 1;
            write!(w, "{line_number:>digits$}{}", line_numbers_spec.separator)?;
        }

        let mut line_mgr = LineMgr::new(tab_size);
        let mut write_part =
            |w: &mut W, part: &str| line_mgr.output(part, |s| w.write_all(s.as_bytes()));

        if let Some(cur_color_spec) = &cur_color_spec {
            w.set_color(cur_color_spec)?;
        } else {
            w.reset()?;
        }
        let mut part_rest = line;
        let mut part_start = line_start;
        while let Some(fragment) = &cur_fragment {
            let cur_end = fragment.end.to_usize();
            if cur_end >= line_end {
                break;
            }
            let (part, rest) = part_rest.split_at(cur_end - part_start);
            write_part(w, part)?;
            part_rest = rest;
            part_start += part.len();
            cur_fragment = fragment_iter.next();
            cur_color_spec =
                begin_desc(cur_fragment.and_then(|fragment| fragment.span_desc.as_ref()));
            if let Some(cur_color_spec) = &cur_color_spec {
                w.set_color(cur_color_spec)?;
            } else {
                w.reset()?;
            }
        }
        write_part(w, part_rest)?;

        let open_diag_iter = open_diags.into_iter();
        open_diags = Vec::new();
        for diag in open_diag_iter {
            if let Some(diag_spec) = style.diags(diag.severity) {
                let span = StrPosition::span_to_range(diag.span());
                if let Some((line_numbers_spec, digits)) = &line_numbers {
                    w.set_color(&line_numbers_spec.color)?;
                    write!(w, "{}{}", " ".repeat(*digits), line_numbers_spec.separator)?;
                }
                let mut line_mgr = LineMgr::new(tab_size);
                w.set_color(&diag_spec.diag_color)?;
                write!(w, "{}", diag_spec.cont_start)?;
                if span.end <= line_end {
                    let line_part = &line[..(span.end - line_start)];
                    let line_part_chars = line_mgr.count_chars(line_part);
                    for _ in 1..line_part_chars {
                        write!(w, "{}", diag_spec.squiggle)?;
                    }
                } else {
                    let line_chars = line_mgr.count_chars(line);
                    for _ in 1..(line_chars - 1) {
                        write!(w, "{}", diag_spec.squiggle)?;
                    }
                    write!(w, "{}", diag_spec.cont_end)?;
                    open_diags.push(diag);
                }
                w.reset()?;
                write!(w, "\n")?;
            }
        }

        while let Some(diag) = cur_diag {
            let span = StrPosition::span_to_range(diag.span());
            if span.start >= line_end {
                break;
            }
            if let Some(diag_spec) = style.diags(diag.severity) {
                if let Some((line_numbers_spec, digits)) = &line_numbers {
                    w.set_color(&diag_spec.diag_color)?;
                    for _ in 0..*digits {
                        write!(w, "{}", diag_spec.indicator)?;
                    }
                    w.set_color(&line_numbers_spec.color)?;
                    write!(w, "{}", line_numbers_spec.separator)?;
                }
                let mut line_mgr = LineMgr::new(tab_size);
                let line_prefix = &line[..(span.start - line_start)];
                let line_prefix_chars = line_mgr.count_chars(line_prefix);
                for _ in 0..line_prefix_chars {
                    write!(w, " ")?;
                }
                w.set_color(&diag_spec.diag_color)?;
                if span.end <= span.start {
                    write!(w, "{}", diag_spec.squiggle)?;
                } else if span.end <= line_end {
                    let line_part = &line[(span.start - line_start)..(span.end - line_start)];
                    let line_part_chars = line_mgr.count_chars(line_part);
                    for _ in 0..line_part_chars {
                        write!(w, "{}", diag_spec.squiggle)?;
                    }
                } else {
                    let line_part = &line[(span.start - line_start)..];
                    let line_part_chars = line_mgr.count_chars(line_part);
                    for _ in 0..(line_part_chars - 1) {
                        write!(w, "{}", diag_spec.squiggle)?;
                    }
                    write!(w, "{}", diag_spec.cont_end)?;
                    open_diags.push(diag);
                }
                w.reset()?;
                write!(w, " ")?;
                w.set_color(&diag_spec.msg_color)?;
                w.write_all(diag.message.msg.as_bytes())?;
                write!(w, "\n")?;
                // TODO: also show hints (configurably)
            }
            cur_diag = diag_iter.next();
        }

        line_start = line_end;
        line_idx += 1;
    }

    if !input.ends_with('\n') {
        write!(w, "\n")?;
    }
    w.reset()
}

struct LineMgr {
    tab_size: usize,
    col_idx: usize,
}

impl LineMgr {
    fn new(tab_size: usize) -> Self {
        LineMgr {
            tab_size,
            col_idx: 0,
        }
    }

    fn output(
        &mut self,
        mut part: &str,
        mut f: impl FnMut(&str) -> io::Result<()>,
    ) -> io::Result<()> {
        while let Some((prefix, rest)) = part.split_once('\t') {
            self.col_idx += prefix.chars().count();
            f(prefix)?;
            loop {
                self.col_idx += 1;
                f(" ")?;
                if self.col_idx % self.tab_size == 0 {
                    break;
                }
            }
            part = rest;
        }
        self.col_idx += part.chars().count();
        f(part)
    }

    fn count_chars(&mut self, part: &str) -> usize {
        let start_idx = self.col_idx;
        self.output(part, |_| Ok(())).unwrap();
        self.col_idx - start_idx
    }
}

#[cfg(test)]
mod tests {
    use lang_def::parser::testing::test_parsers::*;
    use termcolor::Ansi;

    use super::*;

    struct TestLang;

    impl LanguageDefinition for TestLang {
        type ParserDesc = TokenizerConfig;
    }

    #[test]
    fn without_line_numbers() -> io::Result<()> {
        let mut style = StandardOutputStyle::standard();
        style.line_numbers = None;
        let mut buf = Ansi::new(Vec::new());
        pretty_print::<TestLang, _>(
            "a ∆ b \n\tc\t de\n\t\n   \nf",
            TokenizerConfig,
            &style,
            &mut buf,
        )?;
        let result = String::from_utf8(buf.into_inner()).unwrap();
        print!("{result}");
        assert_eq!(
            result,
            "\u{1b}[0m\u{1b}[31ma\u{1b}[0m ∆ \u{1b}[0m\u{1b}[31mb\u{1b}[0m \n  \u{1b}[0m\u{1b}[1m\u{1b}[31m~\u{1b}[0m \u{1b}[0m\u{1b}[1munexpected non-ascii character `∆`\n     \u{1b}[0m\u{1b}[1m\u{1b}[33m~>\u{1b}[0m \u{1b}[0m\u{1b}[1mexcessive whitespace length 3\n\u{1b}[0m    \u{1b}[0m\u{1b}[31mc\u{1b}[0m    \u{1b}[0m\u{1b}[31mde\u{1b}[0m\n\u{1b}[0m\u{1b}[1m\u{1b}[33m>~~~\u{1b}[0m\n     \u{1b}[0m\u{1b}[1m\u{1b}[33m~~~~\u{1b}[0m \u{1b}[0m\u{1b}[1mexcessive whitespace length 2\n           \u{1b}[0m\u{1b}[1m\u{1b}[33m>\u{1b}[0m \u{1b}[0m\u{1b}[1mexcessive whitespace length 7\n\u{1b}[0m    \n\u{1b}[0m\u{1b}[1m\u{1b}[33m>~~~>\u{1b}[0m\n\u{1b}[0m   \n\u{1b}[0m\u{1b}[1m\u{1b}[33m>~~~\u{1b}[0m\n\u{1b}[0m\u{1b}[0m\u{1b}[31mf\n\u{1b}[0m"
        );
        Ok(())
    }

    #[test]
    fn with_line_numbers() -> io::Result<()> {
        let style = StandardOutputStyle::standard();
        let mut buf = Ansi::new(Vec::new());
        pretty_print::<TestLang, _>(
            "a ∆ b \n\tc\t de\n\t\n   \nf",
            TokenizerConfig,
            &style,
            &mut buf,
        )?;
        let result = String::from_utf8(buf.into_inner()).unwrap();
        print!("{result}");
        assert_eq!(
            result,
            "\u{1b}[0m\u{1b}[2m1 ┆ \u{1b}[0m\u{1b}[31ma\u{1b}[0m ∆ \u{1b}[0m\u{1b}[31mb\u{1b}[0m \n\u{1b}[0m\u{1b}[1m\u{1b}[31m!\u{1b}[0m\u{1b}[2m ┆   \u{1b}[0m\u{1b}[1m\u{1b}[31m~\u{1b}[0m \u{1b}[0m\u{1b}[1munexpected non-ascii character `∆`\n\u{1b}[0m\u{1b}[1m\u{1b}[33m!\u{1b}[0m\u{1b}[2m ┆      \u{1b}[0m\u{1b}[1m\u{1b}[33m~>\u{1b}[0m \u{1b}[0m\u{1b}[1mexcessive whitespace length 3\n\u{1b}[0m\u{1b}[2m2 ┆ \u{1b}[0m    \u{1b}[0m\u{1b}[31mc\u{1b}[0m    \u{1b}[0m\u{1b}[31mde\u{1b}[0m\n\u{1b}[0m\u{1b}[2m  ┆ \u{1b}[0m\u{1b}[1m\u{1b}[33m>~~~\u{1b}[0m\n\u{1b}[0m\u{1b}[1m\u{1b}[33m!\u{1b}[0m\u{1b}[2m ┆      \u{1b}[0m\u{1b}[1m\u{1b}[33m~~~~\u{1b}[0m \u{1b}[0m\u{1b}[1mexcessive whitespace length 2\n\u{1b}[0m\u{1b}[1m\u{1b}[33m!\u{1b}[0m\u{1b}[2m ┆            \u{1b}[0m\u{1b}[1m\u{1b}[33m>\u{1b}[0m \u{1b}[0m\u{1b}[1mexcessive whitespace length 7\n\u{1b}[0m\u{1b}[2m3 ┆ \u{1b}[0m    \n\u{1b}[0m\u{1b}[2m  ┆ \u{1b}[0m\u{1b}[1m\u{1b}[33m>~~~>\u{1b}[0m\n\u{1b}[0m\u{1b}[2m4 ┆ \u{1b}[0m   \n\u{1b}[0m\u{1b}[2m  ┆ \u{1b}[0m\u{1b}[1m\u{1b}[33m>~~~\u{1b}[0m\n\u{1b}[0m\u{1b}[2m5 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mf\n\u{1b}[0m"
        );
        Ok(())
    }

    #[test]
    fn newline_terminated_9() -> io::Result<()> {
        let style = StandardOutputStyle::standard();
        let mut buf = Ansi::new(Vec::new());
        pretty_print::<TestLang, _>(
            "a\nb\nc\nd\në\nf\ng\nh\ni\n",
            TokenizerConfig,
            &style,
            &mut buf,
        )?;
        let result = String::from_utf8(buf.into_inner()).unwrap();
        print!("{result}");
        assert_eq!(
            result,
            "\u{1b}[0m\u{1b}[2m1 ┆ \u{1b}[0m\u{1b}[31ma\u{1b}[0m\n\u{1b}[0m\u{1b}[2m2 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mb\u{1b}[0m\n\u{1b}[0m\u{1b}[2m3 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mc\u{1b}[0m\n\u{1b}[0m\u{1b}[2m4 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31md\u{1b}[0m\n\u{1b}[0m\u{1b}[2m5 ┆ \u{1b}[0më\n\u{1b}[0m\u{1b}[1m\u{1b}[31m!\u{1b}[0m\u{1b}[2m ┆ \u{1b}[0m\u{1b}[1m\u{1b}[31m~\u{1b}[0m \u{1b}[0m\u{1b}[1munexpected non-ascii character `ë`\n\u{1b}[0m\u{1b}[2m6 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mf\u{1b}[0m\n\u{1b}[0m\u{1b}[2m7 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mg\u{1b}[0m\n\u{1b}[0m\u{1b}[2m8 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mh\u{1b}[0m\n\u{1b}[0m\u{1b}[2m9 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mi\u{1b}[0m\n\u{1b}[0m"
        );
        Ok(())
    }

    #[test]
    fn newline_terminated_10() -> io::Result<()> {
        let style = StandardOutputStyle::standard();
        let mut buf = Ansi::new(Vec::new());
        pretty_print::<TestLang, _>(
            "a\nb\nc\nd\në\nf\ng\nh\ni\nj\n",
            TokenizerConfig,
            &style,
            &mut buf,
        )?;
        let result = String::from_utf8(buf.into_inner()).unwrap();
        print!("{result}");
        assert_eq!(
            result,
            "\u{1b}[0m\u{1b}[2m 1 ┆ \u{1b}[0m\u{1b}[31ma\u{1b}[0m\n\u{1b}[0m\u{1b}[2m 2 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mb\u{1b}[0m\n\u{1b}[0m\u{1b}[2m 3 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mc\u{1b}[0m\n\u{1b}[0m\u{1b}[2m 4 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31md\u{1b}[0m\n\u{1b}[0m\u{1b}[2m 5 ┆ \u{1b}[0më\n\u{1b}[0m\u{1b}[1m\u{1b}[31m!!\u{1b}[0m\u{1b}[2m ┆ \u{1b}[0m\u{1b}[1m\u{1b}[31m~\u{1b}[0m \u{1b}[0m\u{1b}[1munexpected non-ascii character `ë`\n\u{1b}[0m\u{1b}[2m 6 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mf\u{1b}[0m\n\u{1b}[0m\u{1b}[2m 7 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mg\u{1b}[0m\n\u{1b}[0m\u{1b}[2m 8 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mh\u{1b}[0m\n\u{1b}[0m\u{1b}[2m 9 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mi\u{1b}[0m\n\u{1b}[0m\u{1b}[2m10 ┆ \u{1b}[0m\u{1b}[0m\u{1b}[31mj\u{1b}[0m\n\u{1b}[0m"
        );
        Ok(())
    }
}
