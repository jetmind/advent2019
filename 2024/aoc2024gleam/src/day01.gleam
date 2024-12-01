import file_streams/file_stream as fs
import file_streams/file_stream_error as fse
import gleam/dict
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/string

fn slurp_loop(f, res) {
  case fs.read_line(f) {
    Ok(line) -> slurp_loop(f, [line, ..res])
    Error(fse.Eof) -> list.reverse(res)
    _ -> panic as "can't read file"
  }
}

fn slurp(filename) {
  let assert Ok(f) = fs.open_read(filename)
  slurp_loop(f, [])
}

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
  "input/01ex.txt" |> slurp |> solve1 |> int.to_string |> io.println
  "input/01.txt" |> slurp |> solve1 |> int.to_string |> io.println
  "input/01ex.txt" |> slurp |> solve2 |> int.to_string |> io.println
  "input/01.txt" |> slurp |> solve2 |> int.to_string |> io.println
}
