import argv
import day01
import day02
import day03
import day04
import gleam/int
import gleam/io

pub fn main() {
  case argv.load().arguments {
    [day] ->
      case int.parse(day) {
        Ok(1) -> day01.run()
        Ok(2) -> day02.run()
        Ok(3) -> day03.run()
        Ok(4) -> day04.run()
        _ -> io.println("invalid day " <> day)
      }
    _ -> io.println("USAGE: gleam run <day>")
  }
}
