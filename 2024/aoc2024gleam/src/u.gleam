import birl
import birl/duration
import gleam/dict.{type Dict}
import gleam/float
import gleam/function
import gleam/int
import gleam/list
import gleam/regexp as re
import gleam/string as s
import pprint as pp
import simplifile as f

pub fn time(arg, f) {
  let start = birl.now()
  let res = f(arg)
  let diff = birl.difference(birl.now(), start)
  let _ = pp.debug(diff |> duration.accurate_decompose |> list.first)
  res
}

pub type Point(a) {
  Point(char: a, x: Int, y: Int)
}

pub type Grid(a) {
  Grid(
    points: List(Point(a)),
    xy: Dict(#(Int, Int), a),
    height: Int,
    width: Int,
  )
}

pub fn grid(filename) {
  grid_impl(function.identity, filename)
}

pub fn gridint(filename) {
  grid_impl(to_int, filename)
}

fn grid_impl(parse, filename) {
  let lines = filename |> lines |> list.map(s.split(_, ""))
  let assert Ok(line) = list.first(lines)
  let h = list.length(lines)
  let w = list.length(line)

  let points =
    lines
    |> list.index_map(fn(line, y) {
      line |> list.index_map(fn(char, x) { Point(parse(char), x, y) })
    })
    |> list.flatten

  let xy =
    lines
    |> list.index_fold(dict.new(), fn(d, line, y) {
      line
      |> list.index_fold(d, fn(d, char, x) {
        dict.insert(d, #(x, y), parse(char))
      })
    })
  Grid(points, xy, h, w)
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
