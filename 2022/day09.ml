open Base
open Stdio

type move = L | R | D | U (* [@@deriving show] *)

(* bonkers! *)
module IntPair = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare, hash]
  end
  include T
  include Base.Comparator.Make(T)
end

let to_set = Set.of_list (module IntPair)

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

let sign x = if x < 0 then -1 else 1

let move_tail (hx, hy) (tx, ty) =
  let (dx, dy) = (hx - tx, hy - ty) in
  let (tx, ty) = match (dx, dy) with
    | (dx, dy) when abs dx > 1 &&
                    abs dy > 1 -> (tx + dx - sign dx, ty + dy - sign dy)
    | (dx, _)  when abs dx > 1 -> (tx + dx - sign dx, hy)
    | (_, dy)  when abs dy > 1 -> (hx, ty + dy - sign dy)
    | _                        -> (tx, ty) in
  (tx, ty)

let fold_knots (h, knots) knot =
  let new_knot = move_tail h knot in
  (new_knot, new_knot :: knots)

let get_positions n moves =
  let rec get_positions' head knots = function
    | []      -> []
    | m :: ms ->
      let new_head = move_head head m in
      let (_, new_knots) = List.fold ~init:(new_head, []) ~f:fold_knots knots in
      let new_knots = List.rev new_knots in
      let new_list = new_head :: new_knots in
      new_list :: (get_positions' new_head new_knots ms) in
  let start = repeat n (0, 0) in
  start :: get_positions' (List.hd_exn start) (List.tl_exn start) moves

let _display (allpositions : (int * int) list list) =
  let (xmin, xmax) = List.(allpositions |> concat |> fold ~init:(Int.max_value, 0) ~f:(fun (xmin, xmax) (x, _) -> (min xmin x), (max xmax x))) in
  let (ymin, ymax) = List.(allpositions |> concat |> fold ~init:(Int.max_value, 0) ~f:(fun (ymin, ymax) (_, y) -> (min ymin y), (max ymax y))) in
  let display1 positions =
    let index = positions
                |> List.mapi ~f:(fun i x -> (i, x))
                |> Hashtbl.group (module IntPair)
                  ~get_key: (fun (_, pos) -> pos)
                  ~get_data:(fun (i,   _) -> i)
                  ~combine: (fun pos _ -> pos) in
    let s = ref "" in
    for y = ymax downto ymin do
      for x = xmin to xmax do
        let exists = Hashtbl.find index (x, y) in
        match exists with
        | Some 0 -> s := !s ^ "H"
        | Some i -> s := !s ^ Int.to_string i
        | None   -> if x = 0 && y = 0 then s := !s ^ "s" else s := !s ^ "."
      done;
      s := !s ^ "\n"
    done;
    !s in
  allpositions |> List.map ~f:display1 |> String.concat ~sep:"\n"

let get_tail_visited_count positions = positions |> List.map ~f:List.last_exn |> to_set |> Set.count ~f:(Fn.const true)
let inp = In_channel.read_all "input09.txt" |> parse_moves

(* let () = inp |> [%show: move list] |> printf "\n%s\n" *)
(* let () = inp |> get_positions 10 |> [%show: (int * int) list list] |> printf "\n%s\n" *)
(* let () = inp |> get_positions 10 |> _display |> printf "\n%s\n" *)
let () = inp |> get_positions 2 |> get_tail_visited_count |> printf "%i\n"
let () = inp |> get_positions 10 |> get_tail_visited_count |> printf "%i\n"
