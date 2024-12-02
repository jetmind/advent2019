import gleam/io
import gleam/list
import gleam/string as s
import u.{lines}

fn parse_line(line) {
  line |> s.split(on: " ") |> list.map(u.to_int)
}

fn diff(pair) {
  let #(a, b) = pair
  a - b
}

fn is_safe_line1(line) {
  let diffs = line |> list.zip(list.drop(line, 1)) |> list.map(diff)
  let is_inc = fn(n) { n >= 1 && n <= 3 }
  let is_dec = fn(n) { n <= -1 && n >= -3 }
  list.all(diffs, is_inc) || list.all(diffs, is_dec)
}

fn skip(l, n) {
  list.flatten([list.take(l, n), list.drop(l, n + 1)])
}

fn is_safe_line2(line) {
  let len = list.length(line)
  is_safe_line1(line)
  || list.range(0, len - 1)
  |> list.map(fn(n) { skip(line, n) })
  |> list.any(is_safe_line1)
}

pub fn run() {
  let ex = "input/02.ex" |> lines |> list.map(parse_line)
  let in = "input/02.in" |> lines |> list.map(parse_line)
  ex |> list.count(is_safe_line1) |> io.debug
  in |> list.count(is_safe_line1) |> io.debug
  ex |> list.count(is_safe_line2) |> io.debug
  in |> list.count(is_safe_line2) |> io.debug
  Nil
}
