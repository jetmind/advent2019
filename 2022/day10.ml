open Base
open Stdio

let x        = ref 1
let cycle    = ref 1
let strength = ref 0
let screen   = ref ""

let tick () =
  let pixel = (!cycle - 1) % 40 in
  printf "cycle=%i, pixel=%i\n" !cycle pixel;
  if pixel = !x || pixel = !x - 1 || pixel = !x + 1
  then screen := !screen ^ "#"
  else screen := !screen ^ ".";
  if !cycle % 40 = 0 then screen := !screen ^ "\n";
  cycle := !cycle + 1

let eval line =
  match String.split line ~on:' ' with
  | ["addx"; arg] ->
    let n = Int.of_string arg in
    for i = 1 to 2 do
      tick ();
      if i = 2 then x := !x + n;
      match !cycle with
      | 20 | 60 | 100 | 140 | 180 | 220 -> strength := !strength + !x * !cycle
      | _                               -> ()
    done;
  | ["noop"] -> tick ();
  | _        -> failwith ("invalid input: " ^ line)

let () = "input10.txt" |> In_channel.read_all |> String.split_lines |> List.iter ~f:eval
let () = printf "%i\n" !strength
let () = printf "%s\n" !screen
