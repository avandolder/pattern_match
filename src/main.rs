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

use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct PatternParser;

fn main() {
    let example = r#"match val with
  (x, y) => x
  [1, 2, ...rest] => rest
  {tag: :pat, val, line: l} => val
  "hello" => "world"
  1..10 => :small
  'a'..'z' => lower
  'A' | 'B' => "AB"
  [_, _, z] => z
  _ => "default"
end"#;

    let program = PatternParser::parse(Rule::Program, example)
        .expect("failed parse")
        .next()
        .unwrap();
    print!("{:#?}", program);
}
