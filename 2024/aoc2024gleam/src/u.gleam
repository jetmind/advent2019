import file_streams/file_stream as fs
import file_streams/file_stream_error as fse
import gleam/int
import gleam/list
import gleam/string as s

fn lines_loop(f, res) {
  case fs.read_line(f) {
    Ok(line) -> lines_loop(f, [s.trim(line), ..res])
    Error(fse.Eof) -> list.reverse(res)
    _ -> panic as "can't read file"
  }
}

pub fn lines(filename) {
  let assert Ok(f) = fs.open_read(filename)
  lines_loop(f, [])
}

pub fn slurp(filename) {
  filename |> lines |> s.join("\n")
}

pub fn to_int(s: String) -> Int {
  case int.parse(s) {
    Ok(n) -> n
    Error(Nil) -> panic as { "can't parse int from " <> s }
  }
}

pub fn to_int_list(line: String) -> List(Int) {
  line |> s.split(",") |> list.map(to_int)
}
