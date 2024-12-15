import gleam/dict as d
import gleam/list as l
import gleam/string
import pprint as pp
import u

pub fn run() {
  "input/15.ex" |> u.time(solve1) |> pp.debug
  "input/15.in" |> u.time(solve1) |> pp.debug
  Nil
}

fn solve1(file) {
  let #(grid, _w, _h, robot, moves) = parse(file)
  let #(_robot, grid) = l.fold(moves, #(robot, grid), gotta_move)
  grid
  |> d.fold(0, fn(sum, k, v) {
    let #(x, y) = k
    case v {
      "O" -> sum + y * 100 + x
      _ -> sum
    }
  })
}

fn gotta_move(state, move) {
  let #(xy, grid) = state
  let assert Ok(robot_or_box) = d.get(grid, xy)
  let #(x, y) = xy
  let newxy = case move {
    "^" -> #(x, y - 1)
    "v" -> #(x, y + 1)
    "<" -> #(x - 1, y)
    ">" -> #(x + 1, y)
    _ -> panic
  }
  case d.get(grid, newxy) {
    Ok("#") -> state
    Ok(".") -> {
      let newgrid = grid |> d.insert(xy, ".") |> d.insert(newxy, robot_or_box)
      #(newxy, newgrid)
    }
    Ok("O") -> {
      let #(_, newgrid) = gotta_move(#(newxy, grid), move)
      case d.get(newgrid, newxy) {
        // stone moved, move robot
        Ok(stone) if stone != "O" -> gotta_move(#(xy, newgrid), move)
        _ -> state
      }
    }
    _ -> panic
  }
}

fn parse(file) {
  let assert Ok(#(rawg, rawm)) = file |> u.slurp |> string.split_once("\n\n")
  let lines = string.split(rawg, "\n") |> l.map(string.split(_, ""))
  let assert Ok(line) = l.first(lines)

  let w = l.length(line)
  let h = l.length(lines)

  let parse_line = fn(d, line, y) {
    line |> l.index_fold(d, fn(d, char, x) { d.insert(d, #(x, y), char) })
  }
  let grid = lines |> l.index_fold(d.new(), parse_line)
  let assert Ok(robot) =
    grid |> d.keys |> l.find(fn(k) { d.get(grid, k) == Ok("@") })

  let moves = rawm |> string.split("") |> l.filter(fn(s) { s != "\n" })
  #(grid, w, h, robot, moves)
}
