import gleam/dict as d
import gleam/list as l
import gleam/string
import pprint as pp
import u

pub fn run() {
  "input/15.ex" |> u.time(solve1) |> pp.debug
  "input/15.in" |> u.time(solve1) |> pp.debug
  "input/15.ex" |> u.time(solve2) |> pp.debug
  "input/15.in" |> u.time(solve2) |> pp.debug
  Nil
}

fn solve1(file) {
  let #(grid, moves) = parse(file)
  let robot = find_robot(grid)
  let #(_, grid) = l.fold(moves, #(robot, grid), gotta_move)
  sum_gps(grid, "O")
}

fn sum_gps(grid, target) {
  d.fold(grid, 0, fn(sum, k, v) {
    let #(x, y) = k
    case v {
      v if v == target -> sum + y * 100 + x
      _ -> sum
    }
  })
}

fn pp(grid) {
  u.print_map(grid, 20, 10)
}

fn solve2(file) {
  let #(grid, moves) = parse(file)
  let grid = scale(grid)
  let robot = find_robot(grid)
  let #(_, grid) = l.fold(moves, #(robot, grid), gotta_move)
  // pp(grid)
  sum_gps(grid, "[")
}

fn gotta_move(state, move) {
  let #(xy, grid) = state
  // pp(grid)
  let assert Ok(robot_or_box) = d.get(grid, xy)
  // pp.debug("Move " <> move <> " " <> robot_or_box <> " " <> pp.format(xy))
  let #(x, y) = xy
  let newxy = case move {
    "^" -> #(x, y - 1)
    "v" -> #(x, y + 1)
    "<" -> #(x - 1, y)
    ">" -> #(x + 1, y)
    _ -> panic
  }
  let moveline = fn() {
    let #(_, newgrid) = gotta_move(#(newxy, grid), move)
    case d.get(newgrid, newxy) {
      // stone moved, move robot
      Ok(".") -> gotta_move(#(xy, newgrid), move)
      _ -> state
    }
  }
  let movetree = fn(curv) {
    let #(newx, newy) = newxy
    let #(leftxy, rightxy) = case curv {
      "[" -> #(newxy, #(newx + 1, newy))
      "]" -> #(#(newx - 1, newy), newxy)
      _ -> panic
    }
    let #(_, grid) = gotta_move(#(leftxy, grid), move)
    let #(_, grid) = gotta_move(#(rightxy, grid), move)
    // pp.debug([
    //   #("le", leftxy, d.get(grid, leftxy)),
    //   #("ri", rightxy, d.get(grid, rightxy)),
    //   #("xy", xy, d.get(grid, xy)),
    //   #("ne", newxy, d.get(grid, newxy)),
    // ])
    case d.get(grid, leftxy), d.get(grid, rightxy) {
      Ok("."), Ok(".") -> gotta_move(#(xy, grid), move)
      _, _ -> state
    }
  }
  case d.get(grid, newxy) {
    Ok("#") -> state
    Ok(".") -> {
      let newgrid = grid |> d.insert(xy, ".") |> d.insert(newxy, robot_or_box)
      #(newxy, newgrid)
    }
    Ok("O") -> moveline()
    Ok("[" as v) | Ok("]" as v) ->
      case move {
        ">" | "<" -> moveline()
        "^" | "v" -> movetree(v)
        _ -> panic
      }
    _ -> panic
  }
}

fn scale(grid) {
  grid
  |> d.to_list
  |> l.flat_map(fn(kv) {
    let #(#(x, y), v) = kv
    let xy1 = #(x * 2, y)
    let xy2 = #(x * 2 + 1, y)
    case v {
      "#" | "." -> [#(xy1, v), #(xy2, v)]
      "O" -> [#(xy1, "["), #(xy2, "]")]
      "@" -> [#(xy1, "@"), #(xy2, ".")]
      _ -> panic
    }
  })
  |> d.from_list
}

fn parse(file) {
  let assert Ok(#(rawg, rawm)) = file |> u.slurp |> string.split_once("\n\n")
  let lines = string.split(rawg, "\n") |> l.map(string.split(_, ""))
  let parse_line = fn(d, line, y) {
    line |> l.index_fold(d, fn(d, char, x) { d.insert(d, #(x, y), char) })
  }
  let grid = lines |> l.index_fold(d.new(), parse_line)
  let moves = rawm |> string.split("") |> l.filter(fn(s) { s != "\n" })
  #(grid, moves)
}

fn find_robot(grid) {
  let assert Ok(robot) =
    grid |> d.keys |> l.find(fn(k) { d.get(grid, k) == Ok("@") })
  robot
}
