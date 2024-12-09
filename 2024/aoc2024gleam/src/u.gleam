import gleam/int
import gleam/list
import gleam/regexp as re
import gleam/string as s
import simplifile as f

pub type Point {
  Point(char: String, x: Int, y: Int)
}

pub type Grid {
  Grid(points: List(Point), height: Int, width: Int)
}

pub fn grid(filename) {
  let lines = filename |> lines |> list.map(s.split(_, ""))
  let assert Ok(line) = list.first(lines)
  let h = list.length(lines)
  let w = list.length(line)
  let points =
    lines
    |> list.index_map(fn(line, y) {
      line |> list.index_map(fn(char, x) { Point(char, x, y) })
    })
    |> list.flatten
  Grid(points, h, w)
}

pub fn lines(filename) {
  filename |> slurp |> s.split(on: "\n")
}

pub fn slurp(filename) {
  let assert Ok(content) = f.read(from: filename)
  content |> s.trim
}

pub fn to_int(s: String) -> Int {
  case int.parse(s) {
    Ok(n) -> n
    Error(Nil) -> panic as { "can't parse int from " <> s }
  }
}

pub fn to_int_list(line: String) -> List(Int) {
  let assert Ok(re) = re.from_string("[\\s,]+")
  line |> re.split(with: re) |> list.map(to_int)
}
