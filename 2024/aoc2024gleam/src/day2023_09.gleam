import gleam/int
import gleam/io
import gleam/list as l
import u

fn solve1(items) {
  let diffs = l.map2(items, l.drop(items, 1), fn(a, b) { b - a })
  let done = l.all(diffs, fn(n) { n == 0 })
  let assert Ok(last) = l.last(items)
  case done {
    True -> last
    False -> last + solve1(diffs)
  }
}

fn solve2(items) {
  let diffs = l.map2(items, l.drop(items, 1), fn(a, b) { b - a })
  let done = l.all(diffs, fn(n) { n == 0 })
  let assert Ok(first) = l.first(items)
  case done {
    True -> first
    False -> first - solve2(diffs)
  }
}

fn solve(input, f) {
  input |> l.map(f) |> int.sum
}

fn parse(file) {
  file |> u.lines |> l.map(u.to_int_list)
}

pub fn run() {
  "input/202309.ex" |> parse |> solve(solve1) |> io.debug
  "input/202309.in" |> parse |> solve(solve1) |> io.debug
  "input/202309.ex" |> parse |> solve(solve2) |> io.debug
  "input/202309.in" |> parse |> solve(solve2) |> io.debug
  Nil
}
