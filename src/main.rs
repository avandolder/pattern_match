/**
 * # Pattern Match
 *
 * Handle pattern matching for a small Erlang-esque system.
 * Has first-class tuples, records, arrays, ints, strings, chars, bools,
 * and symbols (:id atomic strings).
 *
 * ## Syntax Mockup
 *
 * match val with
 *   (x, y) => x + y
 *   [1, 2, ..rest] => rest
 *   {tag: :pat, val, line: l} => matchPattern(val)
 *   "hello" => "world"
 *   'A' | 'B' => "AB"
 *   [x, _, z] => x + z
 *   _ => "default"
 *   is Int => $"{val} is an int"
 * end
 *
 * ## Grammar
 *
 * match -> 'match' expr 'with' cases
 * cases -> pats '=>' expr cases | 'end'
 * pats -> pat '|' pats | pat
 * pat -> '_' | id | int | string | char | bool | symbol | tuple_pat | record_pat | array_pat
 * tuple_pat -> '(' elems
 * array_pat -> '[' array_elems
 * record_pat -> '{' fields
 * elems -> pat ',' elems | pat ')' | ')'
 * array_elems -> pat ',' array_elems | pat ']' | '..' id ']' | '..' ']' | ']'
 * fields -> field ',' fields | field '}' | '..' id '}' | '..' '}'
 * field -> '[' expr ']' ':' pat | id ':' pat | id
 */

#[macro_use]
extern crate pest_derive;

use pest::{iterators::Pair, Parser};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct PatternParser;

#[derive(Clone, Debug, PartialEq)]
enum Atom<'a> {
    Char(&'a str),
    String(&'a str),
    Bool(bool),
    Int(i64),
    Symbol(&'a str),
}

fn parse_atom(pair: Pair<Rule>) -> Atom {
    match pair.as_rule() {
        Rule::Int => Atom::Int(pair.as_str().parse().unwrap()),
        Rule::String => Atom::String(pair.as_str()),
        Rule::Char => Atom::Char(pair.as_str()),
        Rule::Bool => Atom::Bool(pair.as_str().parse().unwrap()),
        Rule::Symbol => Atom::Symbol(pair.into_inner().next().unwrap().as_str()),
        _ => unreachable!(),
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Value<'a> {
    Atom(Atom<'a>),
    Id(&'a str),
    Array(Vec<Value<'a>>),
    Tuple(Vec<Value<'a>>),
    Record(Vec<(Value<'a>, Value<'a>)>),
}

impl<'a> Value<'a> {
    fn as_id(&self) -> Option<&'a str> {
        match self {
            Value::Id(id) => Some(id),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
enum Match<'a> {
    Any(Value<'a>),
    Binding(&'a str, Value<'a>),
    Atom(Atom<'a>),
}

fn match_pattern<'a>(pat: Pattern<'a>, val: Value<'a>) -> Option<Vec<Match<'a>>> {
    match (pat, val) {
        (Pattern::Atom(pat), Value::Atom(val)) => {
            if pat == val {
                Some(vec![Match::Atom(pat)])
            } else {
                None
            }
        }
        (Pattern::Tuple(pats), Value::Tuple(val)) => match_tuple(pats, val),
        (Pattern::Array(pats, rest), Value::Array(val)) => match_array(pats, rest, val),
        (Pattern::Record(fields, rest), Value::Record(val)) => match_record(fields, rest, val),
        (Pattern::Id(id), val) => Some(vec![Match::Binding(id, val)]),
        (Pattern::Any, val) => Some(vec![Match::Any(val)]),
        _ => None,
    }
}

fn match_record<'a>(
    fields: Vec<(Value<'a>, Pattern<'a>)>,
    rest: Option<Rest<'a>>,
    mut val: Vec<(Value<'a>, Value<'a>)>,
) -> Option<Vec<Match<'a>>> {
    let mut matches: Option<Vec<Match<'a>>> = None;
    for field in fields {
        let result = val
            .iter()
            .enumerate()
            .find(|(_, (key, _))| *key == field.0)
            .and_then(|(idx, (_, val))| Some((idx, match_pattern(field.1, val.clone())?)));

        matches = match (result, matches) {
            (None, _) => return None,
            (Some((idx, res)), Some(mut matches)) => {
                val.remove(idx);
                matches.extend(res.into_iter());
                Some(matches)
            }
            (Some((idx, res)), None) => {
                val.remove(idx);
                Some(res)
            }
        };
    }

    match rest {
        None if val.is_empty() => matches,
        None => None,
        Some(Rest::Unnamed) => matches,
        Some(Rest::Named(id)) => match matches {
            Some(mut matches) => {
                matches.push(Match::Binding(id, Value::Record(val)));
                Some(matches)
            }
            None => Some(vec![Match::Binding(id, Value::Record(val))]),
        },
    }
}

fn match_array<'a>(
    pats: Vec<Pattern<'a>>,
    rest: Option<Rest<'a>>,
    val: Vec<Value<'a>>,
) -> Option<Vec<Match<'a>>> {
    if val.len() < pats.len() {
        return None;
    }

    let mut matches: Option<Vec<Match<'a>>> = None;
    let mut pats = pats.into_iter();
    let mut val = val.into_iter();

    while let (Some(pat), Some(val)) = (pats.next(), val.next()) {
        matches = match (match_pattern(pat, val), matches) {
            (None, _) => return None,
            (Some(res), Some(mut matches)) => {
                matches.extend(res.into_iter());
                Some(matches)
            }
            (Some(res), None) => Some(res),
        };
    }

    let val = val.collect::<Vec<_>>();
    match rest {
        None if val.is_empty() => matches,
        None => None,
        Some(Rest::Unnamed) => matches,
        Some(Rest::Named(id)) => {
            let binding = Match::Binding(id, Value::Array(val));
            match matches {
                Some(mut matches) => {
                    matches.push(binding);
                    Some(matches)
                }
                None => Some(vec![binding]),
            }
        }
    }
}

fn match_tuple<'a>(pats: Vec<Pattern<'a>>, val: Vec<Value<'a>>) -> Option<Vec<Match<'a>>> {
    if pats.len() != val.len() {
        return None;
    }

    let mut matches: Option<Vec<Match<'a>>> = None;
    let mut pats = pats.into_iter();
    let mut val = val.into_iter();

    while let (Some(pat), Some(val)) = (pats.next(), val.next()) {
        matches = match (match_pattern(pat, val), matches) {
            (None, _) => return None,
            (Some(res), Some(mut matches)) => {
                matches.extend(res.into_iter());
                Some(matches)
            }
            (Some(res), None) => Some(res),
        };
    }

    matches
}

#[derive(Clone, Debug)]
enum Rest<'a> {
    Named(&'a str),
    Unnamed,
}

#[derive(Clone, Debug)]
enum Pattern<'a> {
    Any,
    Atom(Atom<'a>),
    Id(&'a str),
    Tuple(Vec<Pattern<'a>>),
    Array(Vec<Pattern<'a>>, Option<Rest<'a>>),
    Record(Vec<(Value<'a>, Pattern<'a>)>, Option<Rest<'a>>),
}

fn parse_field(pair: Pair<Rule>) -> (Value, Pattern) {
    let mut pairs = pair.into_inner();

    let first = pairs.next().unwrap();
    let val = match first.as_rule() {
        Rule::Expr => parse_expr(first),
        Rule::Id => Value::Id(first.as_str()),
        _ => unreachable!(),
    };

    let pat = pairs.next();
    let pat = match pat.as_ref().map(|p| p.as_rule()) {
        Some(Rule::Pattern) => parse_pattern(pat.unwrap()),
        Some(_) => unreachable!(),
        None => Pattern::Id(val.as_id().unwrap()),
    };

    (val, pat)
}

fn parse_pattern(pair: Pair<Rule>) -> Pattern {
    match pair.as_rule() {
        Rule::Pattern if pair.as_str() == "_" => Pattern::Any,
        Rule::Pattern => parse_pattern(pair.into_inner().next().unwrap()),
        Rule::TuplePattern => Pattern::Tuple(pair.into_inner().map(parse_pattern).collect()),
        Rule::ArrayPattern => {
            let mut rest = None;
            let mut elements = vec![];
            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::Pattern => elements.push(parse_pattern(p)),
                    Rule::Rest => match p.into_inner().next() {
                        Some(id) => rest = Some(Rest::Named(id.as_str())),
                        None => rest = Some(Rest::Unnamed),
                    },
                    _ => unreachable!(),
                }
            }
            Pattern::Array(elements, rest)
        }
        Rule::RecordPattern => {
            let mut rest = None;
            let mut fields = vec![];
            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::Field => fields.push(parse_field(p)),
                    Rule::Rest => match p.into_inner().next() {
                        Some(id) => rest = Some(Rest::Named(id.as_str())),
                        None => rest = Some(Rest::Unnamed),
                    },
                    _ => unreachable!(),
                }
            }
            Pattern::Record(fields, rest)
        }
        Rule::Atom => Pattern::Atom(parse_atom(pair.into_inner().next().unwrap())),
        Rule::Id => Pattern::Id(pair.as_str()),
        _ => unreachable!(),
    }
}

fn parse_expr(pair: Pair<Rule>) -> Value {
    let expr = pair.into_inner().next().unwrap();
    match expr.as_rule() {
        Rule::Atom => Value::Atom(parse_atom(expr.into_inner().next().unwrap())),
        Rule::Id => Value::Id(expr.as_str()),
        _ => unreachable!(),
    }
}

fn main() {
    let example = r#"match val with
  (x, y) => x
  [1, 2, ..rest] => rest
  {tag: :pat, val, line: l, ..} => val
  "hello" => "world"
  'A' | 'B' => "AB"
  [_, _, z] => z
  _ => "default"
end"#;

    let program = PatternParser::parse(Rule::Program, example)
        .expect("failed parse")
        .next()
        .unwrap();

    for m in program.into_inner() {
        if let Rule::EOI = m.as_rule() {
            break;
        }

        let mut m = m.into_inner();
        let expr = m.next().unwrap();
        println!("match {} with", expr.as_str());

        let mut cases = vec![];
        for case in m {
            let mut patterns = vec![];
            let mut value;
            let mut case = case.into_inner();

            loop {
                let pair = case.next().unwrap();
                match pair.as_rule() {
                    Rule::Pattern => {
                        let pat = parse_pattern(pair);
                        print!("| {:?} ", pat);
                        patterns.push(pat);
                    }
                    Rule::Expr => {
                        println!("=> {}", pair.as_str());
                        value = parse_expr(pair);
                        break;
                    }
                    _ => unreachable!(),
                }
            }

            cases.push((patterns, value));
        }

        println!("end");

        let expr = parse_expr(expr);
        let result = cases.into_iter().find_map(|(patterns, _)| {
            patterns
                .into_iter()
                .find_map(|pat| match_pattern(pat, expr.clone()))
        });
        println!("==> {:?}", result);
    }
}
