import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/regexp as re
import u

fn solve1(s) {
  let assert Ok(re) = re.from_string("mul\\((\\d+),(\\d+)\\)")
  re.scan(re, s)
  |> list.map(fn(x) {
    let assert [Some(x), Some(y)] = x.submatches
    u.to_int(x) * u.to_int(y)
  })
  |> int.sum
}

fn solve2(s) {
  let assert Ok(dont) = re.from_string("don't\\(\\)")
  let assert Ok(do) = re.from_string("do\\(\\)")
  let donts = re.split(dont, s)
  let head = list.take(donts, 1)
  let tail = list.flat_map(donts, fn(s) { re.split(do, s) |> list.drop(1) })
  list.append(head, tail) |> list.map(solve1) |> int.sum
}

pub fn run() {
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  |> solve1
  |> io.debug

  "input/03.in" |> u.slurp |> solve1 |> io.debug

  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  |> solve2
  |> io.debug

  "input/03.in" |> u.slurp |> solve2 |> io.debug
  Nil
}
