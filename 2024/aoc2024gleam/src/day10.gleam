import gleam/deque as q
import gleam/dict
import gleam/int
import gleam/list
import gleam/set
import pprint as pp
import u

pub fn run() {
  "input/10.ex" |> solve1 |> pp.debug
  "input/10.in" |> solve1 |> pp.debug
  Nil
}

fn solve1(file) {
  let grid = file |> u.gridint
  let trailheads = grid.xy |> dict.filter(fn(_, v) { v == 0 }) |> dict.keys
  trailheads |> list.map(bfs(grid, _)) |> list.map(list.length) |> int.sum
}

fn bfs(grid, p) {
  let q = q.new() |> q.push_back(p)
  let seen = set.new() |> set.insert(p)
  bfs_loop(grid, q, seen, [])
}

fn bfs_loop(grid: u.Grid(Int), q, seen, nines) {
  case q.pop_front(q) {
    Error(Nil) -> nines
    Ok(#(xy, q)) -> {
      let assert Ok(currentn) = dict.get(grid.xy, xy)
      let nines = case currentn == 9 {
        True -> [xy, ..nines]
        False -> nines
      }
      let #(x, y) = xy
      let nbr =
        [#(x - 1, y), #(x + 1, y), #(x, y - 1), #(x, y + 1)]
        |> list.filter(fn(xy) {
          case dict.get(grid.xy, xy) {
            Ok(n) if n == currentn + 1 -> True
            _ -> False
          }
        })
      let q =
        list.fold(nbr, q, fn(q, xy) {
          case set.contains(seen, xy) {
            True -> q
            False -> q.push_back(q, xy)
          }
        })
      let seen = set.union(seen, set.from_list(nbr))
      bfs_loop(grid, q, seen, nines)
    }
  }
}
