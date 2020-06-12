/**
 * Handle pattern matching for a small Erlang-esque system.
 * Has first-class tuples, records, arrays, ranges, ints, strings, chars, bools,
 * and symbols (:id atomic strings).
 *
 * A rough syntax mockup:
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
 *   Int(x) => $"{x} is an int"
 * end
 *
 * Grammar:
 * match -> 'match' value 'with' cases
 * cases -> pats '=>' expr cases | 'end'
 * pats -> pat '|' pats | pat
 * pat -> '_' | id | int | string | char | bool | symbol | tuple_pat | record_pat | array_pat
 *      | range | type_pat
 * tuple_pat -> '(' elems
 * array_pat -> '[' array_elems
 * record_pat -> '{' fields
 * type_pat -> type_name '(' pat ')'
 * elems -> pat ',' elems | pat ')' | ')'
 * array_elems -> pat ',' array_elems | pat ']' | '...' id ']' | '...' ']' | ']'
 * fields -> field ',' fields | field '}' | '...' id '}' | '...' '}'
 * field -> '[' expr ']' ':' pat | id ':' pat | id
 */

fn main() {
}
