import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/string
import pprint as pp
import u

type Item {
  Hole(n: Int)
  File(n: Int, index: Int)
}

type InputDense =
  dict.Dict(Int, Item)

type InputSparse =
  dict.Dict(Int, option.Option(Int))

pub fn run() {
  "2333133121414131402" |> solve1 |> pp.debug
  "input/09.in" |> u.slurp |> solve1 |> pp.debug
  "2333133121414131402" |> solve2 |> pp.debug
  "input/09.in" |> u.slurp |> solve2 |> pp.debug
  Nil
}

fn solve1(input) {
  let d = input |> parse |> unfold
  let t = dict.size(d) - 1
  d |> moveblocks(0, t) |> checksum
}

fn solve2(input) {
  let disk = parse(input)
  let tail = dict.size(disk) - 1
  list.range(tail, 0)
  |> list.fold(disk, fn(disk, tail) { movefiles(disk, 0, tail) })
  // |> pr
  |> unfold
  |> checksum
}

fn parse(input: String) -> InputDense {
  input
  |> string.split("")
  |> list.index_map(fn(n, i) {
    let n = u.to_int(n)
    let file_index = i / 2
    case i % 2 == 0 {
      True -> #(i, File(n, file_index))
      False -> #(i, Hole(n))
    }
  })
  |> dict.from_list
}

fn to_list(disk: InputDense) -> List(Item) {
  disk
  |> dict.to_list
  |> list.sort(fn(a, b) { int.compare(pair.first(a), pair.first(b)) })
  |> list.map(pair.second)
}

fn unfold(disk: InputDense) -> InputSparse {
  to_list(disk)
  |> list.map(fn(item) {
    case item {
      File(n, i) -> list.repeat(Some(i), n)
      Hole(n) -> list.repeat(None, n)
    }
  })
  |> list.flatten
  |> list.index_fold(dict.new(), fn(acc, item, index) {
    acc |> dict.insert(index, item)
  })
}

fn pr(disk: InputDense) -> String {
  to_list(disk)
  |> list.map(fn(item) {
    case item {
      File(n, i) -> string.repeat(int.to_string(i), n)
      Hole(n) -> string.repeat(".", n)
    }
  })
  |> string.concat
}

fn swap(a, x, y) {
  let assert Ok(xv) = dict.get(a, x)
  let assert Ok(yv) = dict.get(a, y)
  a |> dict.insert(y, xv) |> dict.insert(x, yv)
}

fn moveblocks(d: InputSparse, head: Int, tail: Int) -> InputSparse {
  case head >= tail {
    True -> d
    False -> {
      let assert Ok(hv) = dict.get(d, head)
      let assert Ok(tv) = dict.get(d, tail)
      case hv, tv {
        Some(_), _ -> moveblocks(d, head + 1, tail)
        None, None -> moveblocks(d, head, tail - 1)
        None, Some(_) -> moveblocks(swap(d, head, tail), head + 1, tail - 1)
      }
    }
  }
}

fn checksum(d: InputSparse) {
  dict.fold(d, 0, fn(sum, i, item) {
    case item {
      Some(n) -> sum + n * i
      None -> sum
    }
  })
}

fn shiftinsert(d, idx, item) {
  d
  |> dict.to_list
  |> list.map(fn(kv) {
    case kv {
      #(k, v) if k >= idx -> #(k + 1, v)
      _ -> kv
    }
  })
  |> dict.from_list
  |> dict.insert(idx, item)
}

fn movefiles(disk: InputDense, head: Int, tail: Int) -> InputDense {
  case head >= tail {
    True -> disk
    False -> {
      let assert Ok(hitem) = dict.get(disk, head)
      let assert Ok(titem) = dict.get(disk, tail)
      case hitem, titem {
        _, Hole(..) -> movefiles(disk, head, tail - 1)
        File(..), _ -> movefiles(disk, head + 1, tail)
        Hole(hole), File(file, ..) if hole == file -> disk |> swap(head, tail)
        Hole(hole), File(file, ..) if hole > file -> {
          disk
          |> dict.insert(head, Hole(file))
          |> swap(head, tail)
          |> shiftinsert(head + 1, Hole(hole - file))
        }
        _, _ -> movefiles(disk, head + 1, tail)
      }
    }
  }
}
