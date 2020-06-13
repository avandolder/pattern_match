/**
 * # Pattern Match
 *
 * Handle pattern matching for a small Erlang-esque system.
 * Has first-class tuples, records, arrays, ranges, ints, strings, chars, bools,
 * and symbols (:id atomic strings).
 *
 * ## Syntax Mockup
 *
 * match val with
 *   (x, y) => x + y
 *   [1, 2, ...rest] => rest
 *   {tag: :pat, val, line: l} => matchPattern(val)
 *   "hello" => "world"
 *   1..10 => :small
 *   'a'..'z' => lower
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
 *      | range
 * tuple_pat -> '(' elems
 * array_pat -> '[' array_elems
 * record_pat -> '{' fields
 * elems -> pat ',' elems | pat ')' | ')'
 * array_elems -> pat ',' array_elems | pat ']' | '...' id ']' | '...' ']' | ']'
 * fields -> field ',' fields | field '}' | '...' id '}' | '...' '}'
 * field -> '[' expr ']' ':' pat | id ':' pat | id
 */

#[macro_use]
extern crate pest_derive;

use pest::{
    iterators::Pair,
    Parser,
};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct PatternParser;

#[derive(Clone, Debug)]
enum Rest<'a> {
    Named(&'a str),
    Unnamed,
    None,
}

#[derive(Clone, Debug)]
enum Pattern<'a> {
    Any,
    Symbol(&'a str),
    Int(i64),
    Char(&'a str),
    Bool(bool),
    Id(&'a str),
    String(&'a str),
    Tuple(Vec<Pattern<'a>>),
    Array(Vec<Pattern<'a>>, Rest<'a>),
    Record(Vec<(&'a str, Pattern<'a>)>, Rest<'a>),
}

fn parse_field(mut pair: Pair<Rule>) -> (&str, Pattern) {
    let mut pairs = pair.into_inner();

    let first = pairs.next().unwrap();
    let s = match first.as_rule() {
        Rule::Expr | Rule::Id => first.as_str(),
        _ => unreachable!(),
    };

    let pat = pairs.next();
    let pat = match pat.as_ref().map(|p| p.as_rule()) {
        Some(Rule::Pattern) => parse_pattern(pat.unwrap()),
        Some(_) => unreachable!(),
        None => Pattern::Id(s),
    };

    (s, pat)
}

fn parse_pattern(pair: Pair<Rule>) -> Pattern {
    match pair.as_rule() {
        Rule::Pattern if pair.as_str() == "_" => Pattern::Any,
        Rule::Pattern => parse_pattern(pair.into_inner().next().unwrap()),
        Rule::TuplePattern => Pattern::Tuple(pair.into_inner().map(parse_pattern).collect()),
        Rule::ArrayPattern => {
            let mut rest = Rest::None;
            let mut elements = vec![];
            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::Pattern => elements.push(parse_pattern(p)),
                    Rule::Rest => match p.into_inner().next() {
                        Some(id) => rest = Rest::Named(id.as_str()),
                        None => rest = Rest::Unnamed,
                    }
                    _ => unreachable!(),
                }
            }
            Pattern::Array(elements, rest)
        }
        Rule::RecordPattern => {
            let mut rest = Rest::None;
            let mut fields = vec![];
            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::Field => fields.push(parse_field(p)),
                    Rule::Rest => match p.into_inner().next() {
                        Some(id) => rest = Rest::Named(id.as_str()),
                        None => rest = Rest::Unnamed,
                    }
                    _ => unreachable!(),
                }
            }
            Pattern::Record(fields, rest)
        }
        // Rule::Range => ,
        Rule::Int => Pattern::Int(pair.as_str().parse().unwrap()),
        Rule::String => Pattern::String(pair.as_str()),
        Rule::Char => Pattern::Char(pair.as_str()),
        Rule::Bool => Pattern::Bool(pair.as_str().parse().unwrap()),
        Rule::Symbol => Pattern::Symbol(pair.into_inner().next().unwrap().as_str()),
        Rule::Id => Pattern::Id(pair.as_str()),
        _ => unreachable!(),
    }
}

fn main() {
    let example = r#"match val with
  (x, y) => x
  [1, 2, ...rest] => rest
  {tag: :pat, val, line: l} => val
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

        for case in m {
            let mut case = case.into_inner();

            loop {
                let mut pair = case.next().unwrap();
                match pair.as_rule() {
                    Rule::Pattern => print!("| {:?} ", parse_pattern(pair)),
                    Rule::Expr => {
                        println!("=> {}", pair.as_str());
                        break;
                    }
                    _ => unreachable!(),
                }
            }
        }

        println!("end");
    }
}
