open Base
open Stdio

let parse_line line = line |> String.to_array |> Array.map ~f:(fun x -> x |> String.of_char |> Int.of_string)
let parse input = input |> String.split_lines |> List.map ~f:parse_line |> Array.of_list

let is_every_lower height a =
  match Array.max_elt ~compare:Int.compare a with
  | None   -> true
  | Some x -> x < height

let sub_left grid x y =
  let x_max = Array.length grid.(0) in
  let pos = x + 1 in
  let len = x_max - pos in
  if len > 0 then Array.sub grid.(y) ~pos ~len else [||]

let sub_right grid x y =
  let pos = 0 in
  let len = x in
  Array.sub grid.(y) ~pos ~len

let is_visible grid x y =
  let h = grid.(y).(x) in
  let tran = Array.transpose_exn grid in
  is_every_lower h (sub_left grid x y) || is_every_lower h (sub_right grid x y) ||
  is_every_lower h (sub_left tran y x) || is_every_lower h (sub_right tran y x)

let count_visible grid =
  let open Array in
  let count = ref 0 in
  for x = 0 to (length grid - 1) do
    for y = 0 to (length grid.(0) - 1) do
      if is_visible grid x y then Int.incr count
    done
  done;
  !count

let score height a =
  match Array.findi a ~f:(fun _ x -> x >= height) with
  | None        -> Array.length a
  | Some (i, _) -> i + 1

let scenic_score grid x y =
  let h = grid.(y).(x) in
  let tran = Array.transpose_exn grid in
  score h (sub_left grid x y) *
  score h (sub_left tran y x) *
  score h (Array.rev (sub_right grid x y)) *
  score h (Array.rev (sub_right tran y x))

let max_scenic_score grid =
  let open Array in
  let score = ref 0 in
  for x = 0 to (length grid - 1) do
    for y = 0 to (length grid.(0) - 1) do
      let s = scenic_score grid x y in
      if s > !score then score := s
    done
  done;
  !score

let inp = In_channel.read_all "input08.txt" |> parse
let () = inp |> count_visible |> printf "\n%i\n"
let () = inp |> max_scenic_score |> printf "%i\n"
