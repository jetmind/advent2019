import gleam/dict
import gleam/function
import gleam/int
import gleam/io
import gleam/list as l
import gleam/order
import gleam/pair
import gleam/regexp
import gleam/set
import pngleam
import pprint as pp
import simplifile
import u

pub fn run() {
  let solve1 = u.time(_, solve1(_, 101, 103))
  let solve2 = u.time(_, solve2(_, 101, 103))
  // "input/14.ex" |> here_come_the_robots |> solve1(11, 7) |> pp.debug
  "input/14.in" |> here_come_the_robots |> solve1 |> pp.debug
  "input/14.in" |> here_come_the_robots |> solve2 |> pp.debug
  Nil
}

fn here_come_the_robots(file) {
  let assert Ok(re) = regexp.from_string("-?[\\d]+")
  let parse1 = fn(line) {
    let assert [x, y, vx, vy] =
      regexp.scan(re, line) |> l.map(fn(m) { u.to_int(m.content) })
    #(x, y, vx, vy)
  }
  file |> u.lines |> l.map(parse1)
}

fn solve1(robots, w, h) {
  robots |> move(100, w, h) |> safety_factor(w, h)
}

fn solve2(robots, w, h) {
  let assert Ok(#(sec, _longest_hline)) =
    l.range(1, 10_000)
    |> l.map(fn(sec) {
      let longest = robots |> move(sec, w, h) |> longest_hline
      #(sec, longest)
    })
    |> l.reduce(fn(pair1, pair2) {
      case int.compare(pair.second(pair1), pair.second(pair2)) {
        order.Lt -> pair2
        _ -> pair1
      }
    })
  let res = robots |> move(sec, w, h)
  res |> render_text(w, h)
  res |> render_png(w, h)
  sec
}

fn longest_hline(robots) {
  let assert Ok(max) =
    robots
    |> l.group(pair.first)
    |> dict.values
    |> l.map(l.map(_, pair.second))
    |> l.map(longest_segment)
    |> l.reduce(int.max)
  max
}

fn longest_segment(ints) {
  let ints = ints |> l.sort(int.compare)
  let shifted = l.drop(ints, 1)
  let max =
    l.map2(shifted, ints, int.subtract)
    |> l.group(function.identity)
    |> dict.values
    |> l.map(l.length)
    |> l.reduce(int.max)
  case max {
    Ok(max) -> max
    Error(Nil) -> -1
  }
}

fn safety_factor(coords, w, h) {
  let #(a, b, c, d) =
    coords
    |> l.fold(#(0, 0, 0, 0), fn(counts, coord) {
      let #(x, y) = coord
      let #(mx, my) = #(w / 2, h / 2)
      let #(tl, tr, bl, br) = counts
      case x == mx || y == my, x < mx, y < my {
        True, _, _ -> counts
        _, True, True -> #(tl + 1, tr, bl, br)
        _, False, True -> #(tl, tr + 1, bl, br)
        _, True, False -> #(tl, tr, bl + 1, br)
        _, False, False -> #(tl, tr, bl, br + 1)
      }
    })
  a * b * c * d
}

fn move(robots, seconds, w, h) {
  robots
  |> l.map(fn(robot) {
    let #(x, y, vx, vy) = robot
    let x = case { x + vx * seconds } % w {
      x if x < 0 -> x + w
      x -> x
    }
    let y = case { y + vy * seconds } % h {
      y if y < 0 -> y + h
      y -> y
    }
    #(x, y)
  })
}

fn render_text(points, w, h) {
  let points = set.from_list(points)
  let yrange = l.range(0, h - 1)
  let xrange = l.range(0, w - 1)
  l.map(yrange, fn(y) {
    l.map(xrange, fn(x) {
      let p = #(x, y)
      let c = case set.contains(points, p) {
        True -> "#"
        False -> "."
      }
      io.print(c)
    })
    io.print("\n")
  })
  io.print("\n")
  Nil
}

fn render_png(points, w, h) {
  let points = set.from_list(points)
  let yrange = l.range(0, h - 1)
  let xrange = l.range(0, w - 1)
  let png =
    l.map(yrange, fn(y) {
      l.fold(xrange, <<>>, fn(bits, x) {
        let b = case set.contains(points, #(x, y)), x * y % 3 {
          False, _ -> 0x000000
          True, 0 -> 0xFF0000
          True, 1 -> 0x00FF00
          True, _ -> 0x0000FF
        }
        <<bits:bits, b:size(24)>>
      })
    })
    |> pngleam.from_packed(
      width: w,
      height: h,
      color_info: pngleam.rgb_8bit,
      compression_level: pngleam.default_compression,
    )
  let assert Ok(_) = simplifile.write_bits("day14.png", png)
  Nil
}
