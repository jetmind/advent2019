import gleam/dict
import gleam/function as f
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/set
import gleam/string
import u

fn expectchar(idx, pair) {
  let #(char, coord) = pair
  case dict.get(idx, char) {
    Ok(set) -> set.contains(set, coord)
    Error(Nil) -> False
  }
}

fn expect(idx, start, dx, dy, expected) {
  let #(x, y) = start
  let expected = expected |> string.split("")
  let coords =
    expected |> list.index_map(fn(_, i) { #(x + dx * i, y + dy * i) })
  expected |> list.zip(coords) |> list.all(expectchar(idx, _))
}

fn expect_xmas(idx, start, dx, dy) {
  expect(idx, start, dx, dy, "XMAS")
}

fn expect_mas(idx, start, dx, dy) {
  expect(idx, start, dx, dy, "MAS")
}

fn expect_sam(idx, start, dx, dy) {
  expect(idx, start, dx, dy, "SAM")
}

fn parse(file) {
  file
  |> u.lines
  |> list.index_map(fn(l, y) {
    l |> string.split("") |> list.index_map(fn(c, x) { #(c, #(x, y)) })
  })
  |> list.flatten
  |> list.filter(fn(pair) {
    let #(c, _) = pair
    c == "X" || c == "M" || c == "A" || c == "S"
  })
  |> list.group(pair.first)
  |> dict.map_values(fn(_, v) { v |> list.map(pair.second) |> set.from_list })
}

fn solve1(file) {
  let idx = parse(file)
  let assert Ok(xs) = dict.get(idx, "X")
  xs
  |> set.to_list
  |> list.map(fn(start) {
    [
      expect_xmas(idx, start, 1, 0),
      expect_xmas(idx, start, -1, 0),
      expect_xmas(idx, start, 0, 1),
      expect_xmas(idx, start, 0, -1),
      expect_xmas(idx, start, 1, 1),
      expect_xmas(idx, start, -1, -1),
      expect_xmas(idx, start, 1, -1),
      expect_xmas(idx, start, -1, 1),
    ]
    |> list.count(f.identity)
  })
  |> int.sum
}

fn expect2(idx, start) {
  let #(x, y) = start
  let tl = #(x - 1, y - 1)
  let tr = #(x + 1, y - 1)
  { expect_mas(idx, tl, 1, 1) || expect_sam(idx, tl, 1, 1) }
  && { expect_mas(idx, tr, -1, 1) || expect_sam(idx, tr, -1, 1) }
}

fn solve2(file) {
  let idx = parse(file)
  let assert Ok(a) = dict.get(idx, "A")
  a
  |> set.to_list
  |> list.map(expect2(idx, _))
  |> list.count(f.identity)
}

pub fn run() {
  "input/04.ex" |> solve1 |> io.debug
  "input/04.in" |> solve1 |> io.debug
  "input/04.ex" |> solve2 |> io.debug
  "input/04.in" |> solve2 |> io.debug
  Nil
}
