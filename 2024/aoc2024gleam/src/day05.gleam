import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/set
import gleam/string
import u

fn parse_rules(idx, line) {
  let assert Ok(#(a, b)) = string.split_once(line, on: "|")
  dict.upsert(idx, u.to_int(a), fn(v) {
    let set = case v {
      Some(set) -> set
      None -> set.new()
    }
    set.insert(set, u.to_int(b))
  })
}

fn parse(file) {
  let assert #(rules, [_, ..updates]) =
    file |> u.lines |> list.split_while(fn(line) { line != "" })
  let parsed_rules = rules |> list.fold(dict.new(), parse_rules)
  let parsed_updates = updates |> list.map(u.to_int_list)
  #(parsed_rules, parsed_updates)
}

fn middle(l) {
  let m = list.length(l) / 2
  let assert Ok(res) =
    l
    |> list.index_map(fn(n, i) { #(i, n) })
    |> list.key_filter(m)
    |> list.first
  res
}

fn is_valid(rules, update) {
  case update {
    [] -> True
    [n, ..rest] ->
      case dict.get(rules, n) {
        Error(Nil) -> True
        Ok(set) -> list.all(rest, fn(n) { !set.contains(set, n) })
      }
      && is_valid(rules, rest)
  }
}

fn solve1(file) {
  let #(rules, updates) = parse(file)
  updates
  |> list.filter(fn(update) { update |> list.reverse |> is_valid(rules, _) })
  |> list.map(middle)
  |> int.sum
}

fn solve2(file) {
  let #(rules, updates) = parse(file)
  let is_invalid = fn(update) { !is_valid(rules, update) }
  let cmp = fn(a, b) {
    case dict.get(rules, a), dict.get(rules, b) {
      Error(Nil), Error(Nil) -> order.Eq
      Error(Nil), Ok(bset) ->
        case set.contains(bset, a) {
          True -> order.Gt
          False -> order.Eq
        }
      Ok(aset), Error(Nil) ->
        case set.contains(aset, b) {
          True -> order.Lt
          False -> order.Eq
        }
      Ok(aset), Ok(bset) ->
        case set.contains(aset, b), set.contains(bset, a) {
          False, False -> order.Eq
          False, True -> order.Gt
          True, False -> order.Lt
          True, True -> order.Eq
        }
    }
  }
  updates
  |> list.filter(fn(update) { update |> list.reverse |> is_invalid })
  |> list.map(list.sort(_, cmp))
  |> list.map(middle)
  |> int.sum
}

pub fn run() {
  "input/05.ex" |> solve1 |> io.debug
  "input/05.in" |> solve1 |> io.debug
  "input/05.ex" |> solve2 |> io.debug
  "input/05.in" |> solve2 |> io.debug
  Nil
}
