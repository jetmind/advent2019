import gleam/dict
import gleam/int
import gleam/list as l
import gleam/set
import gleam/yielder as yi
import pprint as pp
import u.{type Point, Point}

fn solve1(file) {
  let grid = u.grid(file)
  let inbounds = fn(p: Point) {
    p.x >= 0 && p.y >= 0 && p.x < grid.width && p.y < grid.height
  }
  let antidotes = fn(points) {
    points
    |> l.combination_pairs
    |> l.flat_map(fn(pair: #(Point, Point)) {
      let #(p1, p2) = pair
      let dx = p2.x - p1.x
      let dy = p2.y - p1.y
      [Point("#", p2.x + dx, p2.y + dy), Point("#", p1.x - dx, p1.y - dy)]
    })
  }
  grid.points
  |> l.filter(fn(p) { p.char != "." })
  |> l.group(fn(p) { p.char })
  |> dict.values
  |> l.flat_map(antidotes)
  |> l.filter(inbounds)
  |> set.from_list
  |> set.size
}

fn solve2(file) {
  let grid = u.grid(file)
  let inbounds = fn(p: Point) {
    p.x >= 0 && p.y >= 0 && p.x < grid.width && p.y < grid.height
  }
  let next_antidote = fn(p: Point, dx, dy, op) {
    Point("#", op(p.x, dx), op(p.y, dy))
  }
  let antidotes = fn(points) {
    points
    |> l.combination_pairs
    |> l.flat_map(fn(pair: #(Point, Point)) {
      let #(p1, p2) = pair
      let dx = p2.x - p1.x
      let dy = p2.y - p1.y
      let next = next_antidote(_, dx, dy, int.add)
      let prev = next_antidote(_, dx, dy, int.subtract)
      [
        yi.iterate(next(p1), next) |> yi.take_while(inbounds) |> yi.to_list,
        yi.iterate(next(p2), next) |> yi.take_while(inbounds) |> yi.to_list,
        yi.iterate(prev(p1), prev) |> yi.take_while(inbounds) |> yi.to_list,
        yi.iterate(prev(p2), prev) |> yi.take_while(inbounds) |> yi.to_list,
      ]
      |> l.flatten
    })
  }
  grid.points
  |> l.filter(fn(p) { p.char != "." })
  |> l.group(fn(p) { p.char })
  |> dict.values
  |> l.flat_map(antidotes)
  |> set.from_list
  |> set.size
}

pub fn run() {
  "input/08.ex" |> solve1 |> pp.debug
  "input/08.in" |> solve1 |> pp.debug
  "input/08.ex" |> solve2 |> pp.debug
  "input/08.in" |> solve2 |> pp.debug
  Nil
}
