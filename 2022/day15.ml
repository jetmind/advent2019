open Base
open Stdio
open Deez

type sensor = { sensor: int * int; beacon: int * int; dist: int } [@@deriving show]

let distance (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

let parse input =
  let lines = String.split_lines input in
  let pattern = Re2.create_exn "x=([-0-9]+), y=([-0-9]+)" in
  let get_int m n = Int.of_string (Re2.Match.get_exn ~sub:(`Index n) m) in
  let parse1 line =
    match Re2.get_matches_exn pattern line with
    | [m1; m2] ->
      let sensor = (get_int m1 1, get_int m1 2) in
      let beacon = (get_int m2 1, get_int m2 2) in
      let dist   = distance sensor beacon in
      { sensor; beacon; dist }
    | _ -> failwith "invalid"
  in List.map ~f:parse1 lines

let solve1 row sensors =
  let open List in
  let xs = sensors |> concat_map ~f:(fun {sensor=(x,_); dist; _} -> [x + dist; x - dist]) in
  let xmin = xs |> fold ~f:min ~init:Int.max_value in
  let xmax = xs |> fold ~f:max ~init:0 in
  let incircle point {sensor; beacon; dist} =
    if distance sensor point <= dist && not (IntPair.equal point beacon)
    then Some point
    else None
  in
  range ~start:`inclusive ~stop:`inclusive xmin xmax
  |> fold ~init:(Set.empty (module IntPair))
    ~f:(fun acc x ->
        let points = filter_map ~f:(incircle (x, row)) sensors in
        Set.union acc (Set.of_list (module IntPair) points))
  |> Set.length

let perimeter mi ma {sensor = (x, y); dist; _} =
  let open List in
  let rplus  = range ~start:`inclusive ~stop:`exclusive in
  let rminus = range ~start:`inclusive ~stop:`exclusive ~stride:(-1) in
  let inbounds (x, y) = x >= mi && x <= ma && y >= mi && y <= ma in
  let d = dist + 1 in
  let zf a b = filter ~f:inbounds (zip_exn a b) in
  let topleft  = zf (rplus (x - d) x) (rminus y (y - d)) in
  let topright = zf (rplus x (x + d)) (rplus (y - d) y) in
  let btmleft  = zf (rminus x (x - d)) (rminus (y + d) y) in
  let btmright = zf (rminus (x + d) x) (rplus y (y + d)) in
  concat [topleft; topright; btmleft; btmright]

let solve2 mi ma sensors =
  let open List in
  let to_set = Set.of_list (module IntPair) in
  let union_all = Set.union_list (module IntPair) in
  let points = sensors |> map ~f:(perimeter mi ma)|> map ~f:to_set |> union_all in
  let check point = for_all sensors ~f:(fun {sensor; dist; _} -> distance sensor point > dist) in
  Set.filter points ~f:check |> Set.to_list |> map ~f:(fun (x, y) -> (x, y, x * 4000000 + y))

let ex = In_channel.read_all "input15ex.txt"
let inp = In_channel.read_all "input15.txt"
let () = ex  |> parse |> solve1 10        |> Int.to_string |> print_endline
let () = inp |> parse |> solve1 2000000   |> Int.to_string |> print_endline
let () = ex  |> parse |> solve2 0 20      |> [%show: (int * int * int) list] |> print_endline
let () = inp |> parse |> solve2 0 4000000 |> [%show: (int * int * int) list] |> print_endline
