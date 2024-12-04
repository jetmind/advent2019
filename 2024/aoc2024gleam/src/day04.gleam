import gleam/dict
import gleam/function as f
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/set
import gleam/string
import u

fn expect1(dict, pair) {
  let #(char, coord) = pair
  case dict.get(dict, char) {
    Ok(set) -> set.contains(set, coord)
    Error(Nil) -> False
  }
}

fn expect(dict, start, dx, dy) {
  let #(x, y) = start
  let coords = [
    #(x + dx * 1, y + dy * 1),
    #(x + dx * 2, y + dy * 2),
    #(x + dx * 3, y + dy * 3),
  ]
  ["M", "A", "S"] |> list.zip(coords) |> list.all(expect1(dict, _))
}

fn solve1(file) {
  let idx =
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

  let assert Ok(xs) = dict.get(idx, "X")
  xs
  |> set.to_list
  |> list.map(fn(start) {
    [
      expect(idx, start, 1, 0),
      expect(idx, start, -1, 0),
      expect(idx, start, 0, 1),
      expect(idx, start, 0, -1),
      expect(idx, start, 1, 1),
      expect(idx, start, -1, -1),
      expect(idx, start, 1, -1),
      expect(idx, start, -1, 1),
    ]
    |> list.count(f.identity)
  })
  |> int.sum
}

pub fn run() {
  "input/04.ex" |> solve1 |> io.debug
  "input/04.in" |> solve1 |> io.debug
  Nil
}
