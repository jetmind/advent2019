import argv
import day01
import day02
import day03
import day04
import day05
import day06
import day07
import day08
import day09
import day10
import day11
import day12
import day2023_09
import gleam/int
import gleam/io
import gleam/list

const days = [
  day01.run, day02.run, day03.run, day04.run, day05.run, day06.run, day07.run,
  day08.run, day09.run, day10.run, day11.run, day12.run,
]

fn runall() {
  list.each(days, fn(f) {
    f()
    io.println("---")
  })
}

fn runday(day) {
  let err = fn() { io.println_error("unknown day: " <> day) }
  case int.parse(day) {
    Error(Nil) -> err()
    Ok(n) ->
      case days |> list.drop(n - 1) |> list.first {
        Error(Nil) -> err()
        Ok(f) -> f()
      }
  }
}

pub fn main() {
  case argv.load().arguments {
    ["all"] -> runall()
    [day] -> runday(day)
    ["2024", day] -> runday(day)
    ["2023", "9"] -> day2023_09.run()
    _ -> io.println_error("USAGE: gleam run [year] <day|all>")
  }
}
