import gleam/int
import gleam/io
import gleam/list as l
import gleam/pair
import gleam/string as s
import u

fn parse(file) {
  file
  |> u.lines
  |> l.map(fn(line) {
    let assert Ok(#(t, vs)) = s.split_once(line, ": ")
    #(u.to_int(t), u.to_int_list(vs))
  })
}

fn check_loop1(res, vs, t) {
  case vs {
    [] -> res == t
    [v, ..vs] -> check_loop1(res + v, vs, t) || check_loop1(res * v, vs, t)
  }
}

fn check_loop2(res, vs, t) {
  case vs {
    [] -> res == t
    [v, ..vs] ->
      check_loop2(res + v, vs, t)
      || check_loop2(res * v, vs, t)
      || check_loop2(u.to_int(int.to_string(res) <> int.to_string(v)), vs, t)
  }
}

fn check(line, f) {
  let #(t, vs) = line
  let assert [v, ..vs] = vs
  f(v, vs, t)
}

fn solve(file, f) {
  file |> parse |> l.filter(check(_, f)) |> l.map(pair.first) |> int.sum
}

pub fn run() {
  "input/07.ex" |> solve(check_loop1) |> io.debug
  "input/07.in" |> solve(check_loop1) |> io.debug
  "input/07.ex" |> solve(check_loop2) |> io.debug
  "input/07.in" |> solve(check_loop2) |> io.debug
  Nil
}
