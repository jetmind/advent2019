open Base
open Stdio

type cpu_t = { x: int;
               cycles: int;
             } [@@deriving show]

let initcpu : cpu_t = { x = 1; cycles = 1 }

let eval ({x; cycles}, history) line =
  match String.split line ~on:' ' with
  | ["addx"; arg] ->
    let n = Int.of_string arg in
    let current = { x = x + n; cycles = cycles + 2 } in
    let history' = current :: { x = x; cycles = cycles + 1 } :: history in
    (current, history')
  | ["noop"] ->
    let current = { x = x; cycles = cycles + 1 } in
    (current, current :: history)
  | _ -> failwith ("invalid input: " ^ line)

(* let _ = "input10ex1.txt" |> In_channel.read_all |> String.split_lines |> List.fold ~init:(initcpu, [initcpu]) ~f:eval |> [%show: cpu_t * cpu_t list] |> printf "%s\n" *)
let () = "input10.txt" |> In_channel.read_all |> String.split_lines
         |> List.fold ~init:(initcpu, [initcpu]) ~f:eval
         |> snd
         |> List.sum (module Int) ~f:(fun {x; cycles} -> match cycles with 20 | 60 | 100 | 140 | 180 | 220 -> x * cycles | _ -> 0)
         |> printf "%i\n"
