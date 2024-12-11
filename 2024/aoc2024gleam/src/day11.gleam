import gleam/dict as d
import gleam/function as f
import gleam/int
import gleam/list as l
import gleam/option.{None, Some}
import pprint as pp
import u

fn num_digits(n) {
  let assert Ok(digits) = int.digits(n, 10)
  l.length(digits)
}

fn inc(dict, key, count) {
  d.upsert(dict, key, fn(v) {
    case v {
      None -> count
      Some(n) -> n + count
    }
  })
}

fn solve(list, iterations) {
  let counts =
    list |> l.group(f.identity) |> d.map_values(fn(_, v) { l.length(v) })
  l.range(1, iterations)
  |> l.fold(counts, fn(counts, _i) {
    d.fold(counts, d.new(), fn(counts, num, count) {
      let ndigits = num_digits(num)
      case num, int.is_even(ndigits) {
        0, _ -> inc(counts, 1, count)
        n, False -> inc(counts, n * 2024, count)
        n, True -> {
          let divider = int.product(l.repeat(10, ndigits / 2))
          counts |> inc(n / divider, count) |> inc(n % divider, count)
        }
      }
    })
  })
  |> d.fold(0, fn(sum, _, v) { sum + v })
}

pub fn run() {
  "125 17" |> u.to_int_list |> solve(25) |> pp.debug
  let in = "5 62914 65 972 0 805922 6521 1639064" |> u.to_int_list
  in |> solve(25) |> pp.debug
  in |> solve(75) |> pp.debug
  Nil
}
