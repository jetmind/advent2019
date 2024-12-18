import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string
import u

type Pos {
  Pos(x: Int, y: Int)
}

type Direction {
  UP
  DOWN
  LEFT
  RIGHT
}

type Board {
  Board(
    width: Int,
    height: Int,
    guard: Pos,
    dir: Direction,
    obstr: set.Set(Pos),
    seen: set.Set(Pos),
    seendir: set.Set(#(Pos, Direction)),
  )
}

fn parse(file) {
  let lines = file |> u.lines
  let assert [line1, ..] = lines
  let h = lines |> list.length
  let w = line1 |> string.length
  let init = Board(w, h, Pos(0, 0), UP, set.new(), set.new(), set.new())
  file
  |> u.lines
  |> list.index_fold(init, fn(board, line, y) {
    line
    |> string.split("")
    |> list.index_fold(board, fn(board, char, x) {
      case char {
        "^" -> Board(..board, guard: Pos(x, y))
        "#" -> Board(..board, obstr: set.insert(board.obstr, Pos(x, y)))
        _ -> board
      }
    })
  })
}

fn is_outside(w, h, pos) {
  let Pos(x, y) = pos
  x < 0 || y < 0 || x >= w || y >= h
}

fn move(board: Board, dir) {
  let Pos(x, y) = board.guard
  let pos = case dir {
    UP -> Pos(x, y - 1)
    DOWN -> Pos(x, y + 1)
    LEFT -> Pos(x - 1, y)
    RIGHT -> Pos(x + 1, y)
  }
  case is_outside(board.width, board.height, pos) {
    True -> None
    False -> Some(pos)
  }
}

fn nextdir(dir) {
  case dir {
    UP -> RIGHT
    RIGHT -> DOWN
    DOWN -> LEFT
    LEFT -> UP
  }
}

fn path(board) {
  let Board(dir: dir, obstr: obstr, seen: seen, seendir: seendir, ..) = board
  case move(board, dir) {
    None -> Some(seen)
    Some(pos) ->
      case set.contains(seendir, #(pos, dir)), set.contains(obstr, pos) {
        True, _ -> None
        _, True -> path(Board(..board, dir: nextdir(dir)))
        _, False -> {
          let s = set.insert(seen, pos)
          let sd = set.insert(seendir, #(pos, dir))
          path(Board(..board, guard: pos, seen: s, seendir: sd))
        }
      }
  }
}

fn solve1(board) {
  board |> path |> option.unwrap(set.new()) |> set.size
}

fn solve2(board) {
  board
  |> path
  |> option.unwrap(set.new())
  |> set.to_list
  |> list.map(fn(pos) {
    path(Board(..board, obstr: set.insert(board.obstr, pos)))
  })
  |> list.count(option.is_none)
}

pub fn run() {
  "input/06.ex" |> parse |> solve1 |> io.debug
  "input/06.in" |> parse |> solve1 |> io.debug
  "input/06.ex" |> parse |> solve2 |> io.debug
  "input/06.in" |> parse |> solve2 |> io.debug
  Nil
}
