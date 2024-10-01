use crate::parser::{Position, SpanDesc, Spanned, WithSpan};

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SpanDescFragment<Pos: Position> {
    pub span_desc: Option<SpanDesc>,
    pub end: Pos,
}

pub fn flatten_span_descs<Pos: Position>(
    span_descs: &mut [WithSpan<SpanDesc, Pos>],
) -> Vec<SpanDescFragment<Pos>> {
    WithSpan::sort_by_spans(span_descs);

    let mut fragments = Vec::new();
    let mut last_pos = Pos::default();
    let mut current_descs: Vec<&WithSpan<SpanDesc, Pos>> = Vec::new();
    let mut iter = span_descs.iter();

    loop {
        let next = iter.next();
        let next_span = next.map(Spanned::span);
        if let Some(next_span) = &next_span {
            if next_span.start == next_span.end {
                continue;
            }
        }

        // Close current span if it ends before or at start of next (possibly repeatedly), or if
        // iterator is exhausted.
        while let Some(current_desc) = current_descs.last() {
            let current_end = current_desc.span().end;
            if last_pos < current_end {
                if let Some(next_span) = &next_span {
                    if current_end > next_span.start {
                        break;
                    }
                }
                fragments.push(SpanDescFragment {
                    span_desc: Some(***current_desc),
                    end: current_end.clone(),
                });
                last_pos = current_end;
            }
            current_descs.pop();
        }

        let Some(next) = next else { break };

        // Special treatment of parentheses: do not output them if they are part of another span.
        if !current_descs.is_empty() && matches!(**next, SpanDesc::ParenStart | SpanDesc::ParenEnd)
        {
            continue;
        }

        let next_span = next_span.unwrap();

        // Output fragment for space up to next, if any.
        if last_pos < next_span.start {
            fragments.push(SpanDescFragment {
                span_desc: current_descs.last().map(|desc| ***desc),
                end: next_span.start.clone(),
            });
            last_pos = next_span.start;
        }

        // Open span for next.
        current_descs.push(next);
    }

    fragments
}

#[cfg(test)]
mod tests {
    use crate::parser::{str::StrPosition, NameScopeDesc};

    use super::*;

    #[test]
    fn flatten_disjoint() {
        assert_eq!(
            flatten_span_descs(&mut [
                WithSpan::new(SpanDesc::Keyword, StrPosition::span_from_range(0..2)),
                WithSpan::new(SpanDesc::Number, StrPosition::span_from_range(2..3)),
            ]),
            [
                SpanDescFragment {
                    span_desc: Some(SpanDesc::Keyword),
                    end: StrPosition::from_usize(2),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::Number),
                    end: StrPosition::from_usize(3),
                },
            ]
        );

        assert_eq!(
            flatten_span_descs(&mut [
                WithSpan::new(SpanDesc::Keyword, StrPosition::span_from_range(0..2)),
                WithSpan::new(SpanDesc::Comment, StrPosition::span_from_range(2..2)),
                WithSpan::new(SpanDesc::Number, StrPosition::span_from_range(2..3)),
            ]),
            [
                SpanDescFragment {
                    span_desc: Some(SpanDesc::Keyword),
                    end: StrPosition::from_usize(2),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::Number),
                    end: StrPosition::from_usize(3),
                },
            ]
        );

        assert_eq!(
            flatten_span_descs(&mut [
                WithSpan::new(SpanDesc::Keyword, StrPosition::span_from_range(1..3)),
                WithSpan::new(SpanDesc::Comment, StrPosition::span_from_range(4..4)),
                WithSpan::new(SpanDesc::Number, StrPosition::span_from_range(5..6)),
            ]),
            [
                SpanDescFragment {
                    span_desc: None,
                    end: StrPosition::from_usize(1),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::Keyword),
                    end: StrPosition::from_usize(3),
                },
                SpanDescFragment {
                    span_desc: None,
                    end: StrPosition::from_usize(5),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::Number),
                    end: StrPosition::from_usize(6),
                },
            ]
        );
    }

    #[test]
    fn flatten_nested() {
        assert_eq!(
            flatten_span_descs(&mut [
                WithSpan::new(
                    SpanDesc::NameDef(NameScopeDesc::Global, None),
                    StrPosition::span_from_range(0..5),
                ),
                WithSpan::new(
                    SpanDesc::NameRef(NameScopeDesc::Local, None),
                    StrPosition::span_from_range(0..2),
                ),
                WithSpan::new(
                    SpanDesc::NameRef(NameScopeDesc::Local, None),
                    StrPosition::span_from_range(3..5),
                ),
            ]),
            [
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameRef(NameScopeDesc::Local, None)),
                    end: StrPosition::from_usize(2),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameDef(NameScopeDesc::Global, None)),
                    end: StrPosition::from_usize(3),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameRef(NameScopeDesc::Local, None)),
                    end: StrPosition::from_usize(5),
                },
            ]
        );

        assert_eq!(
            flatten_span_descs(&mut [
                WithSpan::new(
                    SpanDesc::NameRef(NameScopeDesc::Local, None),
                    StrPosition::span_from_range(0..2),
                ),
                WithSpan::new(
                    SpanDesc::NameRef(NameScopeDesc::Local, None),
                    StrPosition::span_from_range(3..5),
                ),
                WithSpan::new(
                    SpanDesc::NameDef(NameScopeDesc::Global, None),
                    StrPosition::span_from_range(0..5),
                ),
            ]),
            [
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameRef(NameScopeDesc::Local, None)),
                    end: StrPosition::from_usize(2),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameDef(NameScopeDesc::Global, None)),
                    end: StrPosition::from_usize(3),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameRef(NameScopeDesc::Local, None)),
                    end: StrPosition::from_usize(5),
                },
            ]
        );

        assert_eq!(
            flatten_span_descs(&mut [
                WithSpan::new(
                    SpanDesc::NameDef(NameScopeDesc::Global, None),
                    StrPosition::span_from_range(0..5),
                ),
                WithSpan::new(
                    SpanDesc::NameRef(NameScopeDesc::Local, None),
                    StrPosition::span_from_range(1..4),
                ),
                WithSpan::new(SpanDesc::Number, StrPosition::span_from_range(2..3)),
            ]),
            [
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameDef(NameScopeDesc::Global, None)),
                    end: StrPosition::from_usize(1),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameRef(NameScopeDesc::Local, None)),
                    end: StrPosition::from_usize(2),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::Number),
                    end: StrPosition::from_usize(3),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameRef(NameScopeDesc::Local, None)),
                    end: StrPosition::from_usize(4),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameDef(NameScopeDesc::Global, None)),
                    end: StrPosition::from_usize(5),
                },
            ]
        );

        assert_eq!(
            flatten_span_descs(&mut [
                WithSpan::new(
                    SpanDesc::NameDef(NameScopeDesc::Global, None),
                    StrPosition::span_from_range(0..5),
                ),
                WithSpan::new(
                    SpanDesc::NameRef(NameScopeDesc::Local, None),
                    StrPosition::span_from_range(1..4),
                ),
                WithSpan::new(SpanDesc::Number, StrPosition::span_from_range(1..4)),
                WithSpan::new(SpanDesc::Keyword, StrPosition::span_from_range(2..2)),
            ]),
            [
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameDef(NameScopeDesc::Global, None)),
                    end: StrPosition::from_usize(1),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::Number),
                    end: StrPosition::from_usize(4),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameDef(NameScopeDesc::Global, None)),
                    end: StrPosition::from_usize(5),
                },
            ]
        );
    }

    #[test]
    fn flatten_overlapping() {
        assert_eq!(
            flatten_span_descs(&mut [
                WithSpan::new(
                    SpanDesc::NameDef(NameScopeDesc::Global, None),
                    StrPosition::span_from_range(0..2),
                ),
                WithSpan::new(
                    SpanDesc::NameRef(NameScopeDesc::Local, None),
                    StrPosition::span_from_range(1..3),
                ),
            ]),
            [
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameDef(NameScopeDesc::Global, None)),
                    end: StrPosition::from_usize(1),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameRef(NameScopeDesc::Local, None)),
                    end: StrPosition::from_usize(3),
                },
            ]
        );
    }

    #[test]
    fn flatten_parens() {
        assert_eq!(
            flatten_span_descs(&mut [
                WithSpan::new(SpanDesc::ParenStart, StrPosition::span_from_range(0..1)),
                WithSpan::new(
                    SpanDesc::NameDef(NameScopeDesc::Global, None),
                    StrPosition::span_from_range(1..4),
                ),
                WithSpan::new(SpanDesc::ParenStart, StrPosition::span_from_range(1..2)),
                WithSpan::new(SpanDesc::ParenEnd, StrPosition::span_from_range(3..4)),
                WithSpan::new(SpanDesc::ParenEnd, StrPosition::span_from_range(4..5)),
            ]),
            [
                SpanDescFragment {
                    span_desc: Some(SpanDesc::ParenStart),
                    end: StrPosition::from_usize(1),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::NameDef(NameScopeDesc::Global, None)),
                    end: StrPosition::from_usize(4),
                },
                SpanDescFragment {
                    span_desc: Some(SpanDesc::ParenEnd),
                    end: StrPosition::from_usize(5),
                },
            ]
        );
    }
}
