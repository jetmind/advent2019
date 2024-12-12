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
  L(x: Int, y: Int)
  R(x: Int, y: Int)
  T(x: Int, y: Int)
  B(x: Int, y: Int)
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
  traverse(grid)
  |> l.map(fn(p) {
    let groups =
      p.fences
      |> s.to_list
      |> l.group(fn(f) {
        case f {
          T(..) -> "T"
          B(..) -> "B"
          L(..) -> "L"
          R(..) -> "R"
        }
      })
    let assert Ok(t) = d.get(groups, "T")
    let assert Ok(b) = d.get(groups, "B")
    let assert Ok(l) = d.get(groups, "L")
    let assert Ok(r) = d.get(groups, "R")
    let fx = fn(f: FenceSegment) -> Int { f.x }
    let fy = fn(f: FenceSegment) -> Int { f.y }
    let count_me_daddy = fn(getx, gety, list) {
      list
      |> l.group(getx)
      |> d.values
      |> l.map(fn(line) { line |> l.map(gety) |> count_segments })
      |> int.sum
    }
    let ct = count_me_daddy(fy, fx, t)
    let cb = count_me_daddy(fy, fx, b)
    let cl = count_me_daddy(fx, fy, l)
    let cr = count_me_daddy(fx, fy, r)
    p.area * { ct + cb + cl + cr }
  })
  |> int.sum
}

fn count_segments(l) {
  let l = l |> l.sort(int.compare)
  let gaps =
    l.map2(l.drop(l, 1), l, int.subtract) |> l.count(fn(delta) { delta > 1 })
  gaps + 1
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
                -1, 0 -> Ok(L(x, y))
                1, 0 -> Ok(R(x, y))
                0, -1 -> Ok(T(x, y))
                0, 1 -> Ok(B(x, y))
                _, _ -> panic as "DIFF"
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
