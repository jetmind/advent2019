import gleam/int
import gleam/list
import gleam/regexp as re
import gleam/string as s
import simplifile as f

pub fn lines(filename) {
  filename |> slurp |> s.trim |> s.split(on: "\n")
}

pub fn slurp(filename) {
  let assert Ok(content) = f.read(from: filename)
  content
}

pub fn to_int(s: String) -> Int {
  case int.parse(s) {
    Ok(n) -> n
    Error(Nil) -> panic as { "can't parse int from " <> s }
  }
}

pub fn to_int_list(line: String) -> List(Int) {
  let assert Ok(re) = re.from_string("[\\s,]+")
  line |> re.split(with: re) |> list.map(to_int)
}
