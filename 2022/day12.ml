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
  let lines = s |> String.split_lines |> Array.of_list in
  let grid = Array.map ~f:String.to_array lines in
  grid

let find_points (grid : char array array) (char : char) =
  let res = ref [] in
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid.(y) - 1 do
      if Char.(grid.(y).(x) = char)
      then res := (x, y) :: !res
    done
  done;
  !res

let find_point grid char = find_points grid char |> List.hd_exn

let minpath (grid : char array array) start goal =
  let xmax = Array.length grid.(0) in
  let ymax = Array.length grid in
  let elevation (x, y) = Char.to_int (match grid.(y).(x) with | 'S' -> 'a' | 'E' -> 'z' | c -> c) in
  let inbounds = fun (x, y) -> x >= 0 && x < xmax && y >= 0 && y < ymax in
  let q = Queue.create () in
  Queue.enqueue q (start, 0);
  let seen = Set.of_list (module IntPair) [start] in
  let rec bfs = fun seen ->
    match Queue.dequeue q with
    | None -> None
    | Some (point, n) when IntPair.equal point goal -> Some n
    | Some ((x, y) as point, n) ->
    let moves =
      [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
      |> List.filter
         ~f:(fun move ->
              not (Set.mem seen move) &&
              inbounds move &&
              elevation move - elevation point <= 1)
    in
    let seen' = Set.union seen (Set.of_list (module IntPair) moves) in
    Queue.enqueue_all q (List.map ~f:(fun m -> (m, n + 1)) moves);
    bfs seen' in
  bfs seen

let minpath1 grid =
  let start = find_point grid 'S' in
  let goal  = find_point grid 'E' in
  minpath grid start goal |> Option.value_exn

let minpath2 grid =
  let starts = (find_points grid 'S') @ (find_points grid 'a') in
  let goal = find_point grid 'E' in
  starts |> List.filter_map ~f:(fun s -> minpath grid s goal)
         |> List.min_elt ~compare:Int.compare
         |> Option.value_exn

let inp = "input12.txt" |> In_channel.read_all |> parse
(* let () = inp |> [%show: char array array] |> print_endline*)
let () = inp |> minpath1 |> printf "%i\n"
let () = inp |> minpath2 |> printf "%i\n"
