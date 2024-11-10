open Base
open Stdio

type move = L | R | D | U (* [@@deriving show] *)

let repeat n v = List.init n ~f:(fun _ -> v)

let parse_move line =
  let (direction, n) = String.lsplit2_exn line ~on:' ' in
  let n = Int.of_string n in
  let v = match direction with "L" -> L | "R" -> R | "D" -> D | "U" -> U | _ -> failwith "invalid input" in
  repeat n v

let parse_moves lines = lines |> String.split_lines |> List.map ~f:parse_move |> List.concat

let move_head (hx, hy) = function
  | L -> (hx - 1, hy) | R -> (hx + 1, hy)
  | D -> (hx, hy - 1) | U -> (hx, hy + 1)

let move_tail (hx, hy) (tx, ty) =
  let (dx, dy) = (hx - tx, hy - ty) in
  let (tx, ty) = match (dx, dy) with
    | (dx, _) when dx >  1 -> (tx + dx - 1, hy)
    | (dx, _) when dx < -1 -> (tx + dx + 1, hy)
    | (_, dy) when dy >  1 -> (hx, ty + dy - 1)
    | (_, dy) when dy < -1 -> (hx, ty + dy + 1)
    | _                    -> (tx, ty) in
  (tx, ty)

let get_positions moves =
  let rec get_positions' head tail = function
    | []      -> []
    | m :: ms ->
      let new_head = move_head head m in
      let new_tail = move_tail new_head tail in
      (new_head, new_tail) :: (get_positions' new_head new_tail ms) in
  ((0, 0), (0, 0)) :: get_positions' (0, 0) (0, 0) moves

(* bonkers! *)
module IntPair = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare]
  end
  include T
  include Base.Comparator.Make(T)
end
let to_set = Set.of_list (module IntPair)

let get_tail_visited_count positions = positions |> List.map ~f:(fun (_, t) -> t) |> to_set |> Set.count ~f:(Fn.const true)
let inp = In_channel.read_all "input09.txt" |> parse_moves

(* let () = inp |> [%show: move list] |> printf "\n%s\n" *)
(* let () = inp |> get_positions |> [%show: ((int * int) * (int * int)) list] |> printf "\n%s\n" *)
let () = inp |> get_positions |> get_tail_visited_count |> printf "\n%i\n"
