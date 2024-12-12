import gleam/deque
import gleam/dict as d
import gleam/int
import gleam/list as l
import gleam/pair
import gleam/set as s
import pprint as pp
import u.{type Grid, type Point, Point}

pub fn run() {
  "input/12.ex" |> u.grid |> u.time(solve1) |> pp.debug
  "input/12.in" |> u.grid |> u.time(solve1) |> pp.debug
  "input/12.ex" |> u.grid |> u.time(solve2) |> pp.debug
  "input/12.in" |> u.grid |> u.time(solve2) |> pp.debug
  Nil
}

type FenceSegment {
  FenceSegment(dir: Direction, x: Int, y: Int)
}

type Direction {
  T
  D
  L
  R
}

type Plot {
  Plot(area: Int, fences: s.Set(FenceSegment))
}

type State {
  State(
    grid: Grid(String),
    seen: s.Set(Point(String)),
    q: deque.Deque(Point(String)),
    plots: List(Plot),
  )
}

fn solve1(grid) {
  traverse(grid) |> l.fold(0, fn(sum, p) { sum + p.area * s.size(p.fences) })
}

fn solve2(grid) {
  let fx = fn(f: FenceSegment) -> Int { f.x }
  let fy = fn(f: FenceSegment) -> Int { f.y }
  let count_segments = fn(ints) {
    let ints = ints |> l.sort(int.compare)
    let shifted = l.drop(ints, 1)
    let gaps =
      l.map2(shifted, ints, int.subtract) |> l.count(fn(delta) { delta > 1 })
    gaps + 1
  }
  let count_me_daddy = fn(fx, fy, segments) {
    segments
    |> l.group(fx)
    |> d.values
    |> l.map(fn(line) { line |> l.map(fy) |> count_segments })
    |> int.sum
  }
  let direction_count = fn(dir, segments) {
    case dir {
      T | D -> count_me_daddy(fy, fx, segments)
      L | R -> count_me_daddy(fx, fy, segments)
    }
  }
  let perimeter = fn(p: Plot) {
    p.fences
    |> s.to_list
    |> l.group(fn(f) { f.dir })
    |> d.map_values(direction_count)
    |> d.values
    |> int.sum
  }
  grid |> traverse |> l.map(fn(p) { p.area * perimeter(p) }) |> int.sum
}

fn traverse(grid: Grid(String)) {
  let state: State = State(grid: grid, seen: s.new(), q: deque.new(), plots: [])
  let newstate =
    l.fold(state.grid.points, state, fn(state, point) {
      case s.contains(state.seen, point) {
        True -> state
        False -> {
          let seen = s.insert(state.seen, point)
          let q = deque.new() |> deque.push_back(point)
          State(..state, seen: seen, q: q) |> find_plot(0, s.new())
        }
      }
    })
  newstate.plots
}

fn find_plot(state: State, area: Int, fences: s.Set(FenceSegment)) {
  case deque.pop_front(state.q) {
    Error(Nil) -> State(..state, plots: [Plot(area, fences), ..state.plots])
    Ok(#(point, q)) -> {
      let Point(item, x, y) = point
      let fsegs =
        [#(x + 1, y), #(x - 1, y), #(x, y + 1), #(x, y - 1)]
        |> l.filter_map(fn(nbr_xy) {
          case d.get(state.grid.xy, nbr_xy) {
            Ok(nbr) if nbr == item -> Error(Nil)
            _ -> {
              let #(x2, y2) = nbr_xy
              case x2 - x, y2 - y {
                -1, 0 -> Ok(FenceSegment(L, x, y))
                1, 0 -> Ok(FenceSegment(R, x, y))
                0, -1 -> Ok(FenceSegment(T, x, y))
                0, 1 -> Ok(FenceSegment(D, x, y))
                _, _ -> Error(Nil)
              }
            }
          }
        })
        |> s.from_list

      let allnbrs = find_nbrs(state.grid, point)
      let nbrs = l.filter(allnbrs, fn(nbr) { !s.contains(state.seen, nbr) })
      let seen = l.fold(nbrs, state.seen, s.insert)
      let q = l.fold(nbrs, q, deque.push_back)
      let newstate = State(..state, seen: seen, q: q)
      find_plot(newstate, area + 1, s.union(fsegs, fences))
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
