open Base
open Stdio

type valve = { id:      string;
               tunnels: string list;
               rate:    int;
               is_open: bool ref; } [@@deriving show]

let parse lines =
  let vp = Re2.create_exn "[A-Z]{2}" in
  let rp = Re2.create_exn "\d+" in
  let parse1 line =
    let valves = Re2.find_all_exn vp line in
    let rate = Re2.find_first_exn rp line in
    { id      = List.hd_exn valves;
      tunnels = List.tl_exn valves;
      rate    = Int.of_string rate;
      is_open = ref false; }
  in List.map lines ~f:parse1

let exinp = In_channel.read_lines "input16ex.txt"
let () = exinp |> parse |> [%show: valve list] |> print_endline
