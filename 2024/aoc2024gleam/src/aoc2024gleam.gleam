import argv
import day01
import day02
import day03
import day04
import day05
import day06
import day07
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
        Ok(5) -> day05.run()
        Ok(6) -> day06.run()
        Ok(7) -> day07.run()
        _ -> io.println("invalid day " <> day)
      }
    _ -> io.println("USAGE: gleam run <day>")
  }
}
