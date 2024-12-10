import gleam/deque as q
import gleam/dict as d
import gleam/int
import gleam/list as l
import gleam/set as s
import pprint as pp
import u

pub fn run() {
  "input/10.ex" |> solve |> pp.debug
  "input/10.in" |> solve |> pp.debug
  Nil
}

fn solve(file) {
  let grid = file |> u.gridint
  let bfs = bfs(grid, _)
  let is_zero = fn(_, v) { v == 0 }
  let paths = grid.xy |> d.filter(is_zero) |> d.keys |> l.map(bfs)
  let one = paths |> l.map(s.from_list) |> l.map(s.size) |> int.sum
  let two = paths |> l.map(l.length) |> int.sum
  #(one, two)
}

fn bfs(grid, p) {
  let q = q.new() |> q.push_back(p)
  bfs_loop(grid, q, [])
}

fn bfs_loop(grid: u.Grid(Int), q, nines) {
  case q.pop_front(q) {
    Error(Nil) -> nines
    Ok(#(xy, q)) -> {
      let assert Ok(currentn) = d.get(grid.xy, xy)
      let nines = case currentn == 9 {
        True -> [xy, ..nines]
        False -> nines
      }
      let #(x, y) = xy
      let nbr =
        [#(x - 1, y), #(x + 1, y), #(x, y - 1), #(x, y + 1)]
        |> l.filter(fn(xy) {
          case d.get(grid.xy, xy) {
            Ok(n) if n == currentn + 1 -> True
            _ -> False
          }
        })
      let q = l.fold(nbr, q, q.push_back)
      bfs_loop(grid, q, nines)
    }
  }
}
