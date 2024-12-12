import gleam/deque
import gleam/dict as d
import gleam/list as l
import gleam/pair
import gleam/set as s
import pprint as pp
import u.{type Grid, Point}

pub fn run() {
  "input/12.ex" |> u.grid |> u.time(solve1) |> pp.debug
  "input/12.in" |> u.grid |> u.time(solve1) |> pp.debug
  Nil
}

fn solve1(grid: Grid(String)) {
  let seen = s.new()
  let #(res, _) =
    l.fold(grid.points, #([], seen), fn(acc, point) {
      let #(res, seen) = acc
      case s.contains(seen, point) {
        True -> #(res, seen)
        False -> {
          let seen = s.insert(seen, point)
          let q = deque.new() |> deque.push_back(point)
          let #(counts, seen) = find_plot(grid, seen, q, #(0, 0))
          let res = [counts, ..res]
          #(res, seen)
        }
      }
    })
  res
  |> l.fold(0, fn(price, counts) {
    let #(area, perimeter) = counts
    price + area * perimeter
  })
}

fn find_plot(grid: Grid(String), seen, q, counts) {
  case deque.pop_front(q) {
    Error(Nil) -> #(counts, seen)
    Ok(#(point, q)) -> {
      let allnbrs = find_nbrs(grid, point)
      let fence = 4 - l.length(allnbrs)
      let nbrs = l.filter(allnbrs, fn(nbr) { !s.contains(seen, nbr) })
      let seen = l.fold(nbrs, seen, s.insert)
      let q = l.fold(nbrs, q, deque.push_back)
      let #(area, perimeter) = counts
      let counts = #(area + 1, perimeter + fence)
      find_plot(grid, seen, q, counts)
    }
  }
}

fn find_nbrs(grid: Grid(String), point) {
  let Point(item, x, y) = point
  [#(x + 1, y), #(x - 1, y), #(x, y + 1), #(x, y - 1)]
  |> l.filter_map(fn(xy) {
    case d.get(grid.xy, xy) {
      Ok(nbr) if nbr == item -> Ok(Point(nbr, pair.first(xy), pair.second(xy)))
      _ -> Error(Nil)
    }
  })
}
