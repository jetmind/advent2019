import gleam/function as f
import gleam/int
import gleam/list as l
import gleam/regexp
import gleam/string as str
import pprint as pp
import u

pub fn run() {
  "input/13.ex" |> u.slurp |> parse |> u.time(solve1) |> pp.debug
  "input/13.in" |> u.slurp |> parse |> u.time(solve1) |> pp.debug
  Nil
}

type ClawMachine {
  ClawMachine(xa: Int, ya: Int, xb: Int, yb: Int, xp: Int, yp: Int)
}

fn solve1(machines: List(ClawMachine)) {
  l.map(machines, fn(m) {
    l.range(0, 100)
    |> l.filter_map(fn(b) {
      let dividend = m.xp - b * m.xb
      let divisible = dividend % m.xa == 0
      let a = dividend / m.xa
      case divisible {
        True
          if a >= 0
          && a <= 100
          && a * m.xa + b * m.xb == m.xp
          && a * m.ya + b * m.yb == m.yp
        -> Ok(a * 3 + b)
        _ -> Error(Nil)
      }
    })
    |> l.reduce(int.min)
  })
  |> l.filter_map(f.identity)
  |> int.sum
}

fn parse(file) {
  let assert Ok(re) = regexp.from_string("[\\d]+")
  let parse1 = fn(block) {
    let assert [xa, ya, xb, yb, xp, yp] =
      regexp.scan(re, block) |> l.map(fn(m) { u.to_int(m.content) })
    ClawMachine(xa, ya, xb, yb, xp, yp)
  }
  str.split(file, "\n\n") |> l.map(parse1)
}
