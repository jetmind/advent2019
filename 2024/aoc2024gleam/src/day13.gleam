import gleam/int
import gleam/list as l
import gleam/regexp
import gleam/string as str
import pprint as pp
import u

pub fn run() {
  "input/13.ex" |> u.slurp |> parse |> u.time(solve1) |> pp.debug
  "input/13.in" |> u.slurp |> parse |> u.time(solve1) |> pp.debug
  "input/13.in" |> u.slurp |> parse |> u.time(solve2) |> pp.debug
  Nil
}

fn solve1(machines) {
  solve(0, machines)
}

fn solve2(machines) {
  solve(10_000_000_000_000, machines)
}

fn solve(delta, machines) {
  l.map(machines, fn(m) {
    let #(a, b, c, d, x, y) = m
    let #(x, y) = #(x + delta, y + delta)
    let aa = d * x - c * y
    let bb = a * y - b * x
    let dd = a * d - b * c
    case aa % dd == 0 && bb % dd == 0 {
      True -> aa / dd * 3 + bb / dd
      False -> 0
    }
  })
  |> int.sum
}

fn parse(file) {
  let assert Ok(re) = regexp.from_string("[\\d]+")
  let parse1 = fn(block) {
    let assert [a, b, c, d, x, y] =
      regexp.scan(re, block) |> l.map(fn(m) { u.to_int(m.content) })
    #(a, b, c, d, x, y)
  }
  str.split(file, "\n\n") |> l.map(parse1)
}
