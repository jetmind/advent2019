open Base
open Stdio

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare, hash, equal]
  end
  include T
  include Base.Comparator.Make(T)
end

let parse s =
  let parse_line l =
    let p = Re2.create_exn "[0-9]+" in
    let m = Re2.find_all_exn p l in
    List.map ~f:Int.of_string m in
  List.map ~f:parse_line (String.split_lines s)

let rec all_points acc = function
  | x1::y1::x2::y2::rest when x1 = x2 ->
    let s = min y1 y2 in
    let e = max y1 y2 in
    let r = List.range ~start:`inclusive ~stop:`inclusive s e in
    let points = List.map ~f:(fun y -> (x1, y)) r in
    let acc' = Hash_set.(union acc (of_list (module IntPair) points)) in
    all_points acc' (x2::y2::rest)
  | x1::y1::x2::y2::rest when y1 = y2 ->
    let s = min x1 x2 in
    let e = max x1 x2 in
    let r = List.range ~start:`inclusive ~stop:`inclusive s e in
    let points = List.map ~f:(fun x -> (x, y1)) r in
    let acc' = Hash_set.(union acc (of_list (module IntPair) points)) in
    all_points acc' (x2::y2::rest)
  | _ -> acc

let solve1 lines =
  let bag = List.fold ~init:(Hash_set.create (module IntPair)) ~f:all_points lines in
  let ymax = Hash_set.fold ~init:0 ~f:(fun acc (_, y) -> max acc y) bag in
  (* print_endline (Int.to_string ymax); *)
  let rec move n =
    let sand = ref (500, 0) in
    let stuck = ref false in
    let fell = ref false in
    while not !stuck && not !fell do
      (* printf "stuck=%b, fell=%b, sand=%s\n" !stuck !fell ([%show: int * int] !sand); *)
      let x, y  = !sand in
      let down  = (x, y + 1) in
      let left  = (x - 1, y + 1) in
      let right = (x + 1, y + 1) in
      if y >= ymax then fell := true
      else if not (Hash_set.mem bag down)  then sand := down
      else if not (Hash_set.mem bag left)  then sand := left
      else if not (Hash_set.mem bag right) then sand := right
      else stuck := true;
    done;
    if !stuck then Hash_set.add bag !sand;
    if !fell then n else move (n + 1)
    in
  move 0

let solve2 lines =
  let bag = List.fold ~init:(Hash_set.create (module IntPair)) ~f:all_points lines in
  let ymax = Hash_set.fold ~init:0 ~f:(fun acc (_, y) -> max acc y) bag in
  let start = (500, 0) in
  let floor = 2 + ymax in
  let rec move n =
    let sand = ref start in
    let stuck = ref false in
    while not !stuck do
      (* printf "stuck=%b, fell=%b, sand=%s\n" !stuck !fell ([%show: int * int] !sand); *)
      let x, y  = !sand in
      let down  = (x, y + 1) in
      let left  = (x - 1, y + 1) in
      let right = (x + 1, y + 1) in
      if y + 1 >= floor then stuck := true
      else if not (Hash_set.mem bag down)  then sand := down
      else if not (Hash_set.mem bag left)  then sand := left
      else if not (Hash_set.mem bag right) then sand := right
      else stuck := true;
    done;
    if !stuck then Hash_set.add bag !sand;
    if IntPair.equal !sand start then n + 1 else move (n + 1)
    in
  move 0

let ex = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

(* let () = ex |> parse |> build |> Hash_set.to_list |> List.sort ~compare:IntPair.compare |> [%show: (int * int) list] |> print_endline *)
let inp = In_channel.read_all "input14.txt"
let () = ex  |> parse |> solve1 |> Int.to_string |> print_endline
let () = inp |> parse |> solve1 |> Int.to_string |> print_endline

let () = ex  |> parse |> solve2 |> Int.to_string |> print_endline
let () = inp |> parse |> solve2 |> Int.to_string |> print_endline
