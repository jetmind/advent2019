import argv
import day01
import gleam/int
import gleam/io

pub fn main() {
  case argv.load().arguments {
    [day] ->
      case int.parse(day) {
        Ok(1) -> day01.run()
        _ -> io.println("invalid day " <> day)
      }
    _ -> io.println("USAGE: gleam run <day>")
  }
}
