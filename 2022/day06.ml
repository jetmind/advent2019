open Base
open Stdio

let is_distinct chars =
  List.length chars = (chars |> Set.of_list (module Char) |> Set.length)

let marker_position marker_length s =
  let rec distinct_position' idx chars =
    let chunk = List.take chars marker_length in
    if is_distinct chunk
    then idx + marker_length
    else distinct_position' (idx + 1) (List.tl_exn chars) in
  distinct_position' 0 (String.to_list s)

let res1 = In_channel.read_all "input06.txt" |> marker_position 4
let res2 = In_channel.read_all "input06.txt" |> marker_position 14
let () = printf "%i %i" res1 res2
