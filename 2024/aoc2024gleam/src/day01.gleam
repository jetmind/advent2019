import gleam/dict
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/string
import u.{lines}

fn parse(lines) {
  let pairs =
    list.map(lines, fn(line) {
      let assert Ok(#(a, b)) =
        line |> string.trim |> string.split_once(on: "   ")
      let assert Ok(a) = int.parse(a)
      let assert Ok(b) = int.parse(b)
      #(a, b)
    })
  let l = pairs |> list.map(pair.first)
  let r = pairs |> list.map(pair.second)
  #(l, r)
}

fn solve1(lines) {
  let #(l, r) = parse(lines)
  let l = list.sort(l, int.compare)
  let r = list.sort(r, int.compare)
  list.map2(l, r, fn(l, r) { int.absolute_value(l - r) })
  |> list.fold(0, int.add)
}

fn solve2(lines) {
  let #(l, r) = parse(lines)
  let index =
    list.group(r, function.identity)
    |> dict.map_values(fn(_, v) { list.length(v) })
  list.fold(l, 0, fn(acc, n) {
    let freq = case dict.get(index, n) {
      Ok(n) -> n
      Error(Nil) -> 0
    }
    acc + n * freq
  })
}

pub fn run() {
  "input/01ex.txt" |> lines |> solve1 |> int.to_string |> io.println
  "input/01.txt" |> lines |> solve1 |> int.to_string |> io.println
  "input/01ex.txt" |> lines |> solve2 |> int.to_string |> io.println
  "input/01.txt" |> lines |> solve2 |> int.to_string |> io.println
}
