open Base
open Stdio

let parse_line line =
  let r = Str.regexp {|\([0-9]+\)-\([0-9]+\),\([0-9]+\)-\([0-9]+\)|} in
  let _ = Str.string_match r line 0 in
  let parse_group n = Str.matched_group n line |> Int.of_string in
  ((parse_group 1, parse_group 2), (parse_group 3, parse_group 4))

let parse_lines = List.map ~f:parse_line

let fully_contains ((start1, end1), (start2, end2)) =
  (start2 >= start1 && end2 <= end1) || (start1 >= start2 && end1 <= end2)

let overlaps ((start1, end1), (start2, end2)) =
  not (start2 > end1 || start1 > end2)

let input = In_channel.read_all "input04.txt" |> String.split_lines |> parse_lines
let res1 = input |> List.count ~f:fully_contains
let res2 = input |> List.count ~f:overlaps

let () = printf "%i\n%i\n" res1 res2
