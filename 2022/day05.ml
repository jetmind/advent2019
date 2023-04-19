open Base
open Stdio

let parse_crates s =
  let crates = s |> String.split_lines |> List.rev in
  let strlen = crates |> List.hd_exn |> String.length in
  let num_stacks = Float.of_int strlen /. 4. |> Float.round_up |> Float.to_int in
  let stacks = Array.init num_stacks ~f:(fun _ -> Stack.create ()) in
  crates |> List.tl_exn |> List.iter
    ~f:(fun line ->
        for i = 0 to num_stacks - 1 do
          let crate = String.get line (i * 4 + 1) in
          if (Char.compare crate ' ') <> 0 then Stack.push stacks.(i) crate
        done);
  stacks

let parse_moves s =
  s |> String.split_lines |> List.map ~f:(fun line ->
      let _ = Str.string_match (Str.regexp {|move \([0-9]+\) from \([0-9]+\) to \([0-9]+\)|}) line 0 in
      let group n = Int.of_string (Str.matched_group n line) in
      (group 1, (group 2) - 1, (group 3) - 1))

let parse s =
  let split = Str.split (Str.regexp "\n\n") s in
  match split with
  | [crates; moves] -> (parse_crates crates, parse_moves moves)
  | _ -> failwith "invalid input"

let get_top stacks =
  stacks |> Array.map ~f:Stack.pop_exn |> Array.to_list |> String.of_char_list

let move1 (stacks, moves) =
  moves |> List.iter ~f:(fun (move_cnt, move_from, move_to) ->
      for _ = 1 to move_cnt do
        Stack.pop_exn stacks.(move_from) |> Stack.push stacks.(move_to)
      done);
  stacks

let move2 (stacks, moves) =
  moves |> List.iter ~f:(fun (move_cnt, move_from, move_to) ->
      List.init move_cnt ~f:Fn.id
      |> List.map ~f:(fun _ -> Stack.pop_exn stacks.(move_from))
      |> List.rev
      |> List.iter ~f:(Stack.push stacks.(move_to)));
  stacks

let res1 = In_channel.read_all "input05.txt" |> parse |> move1
let res2 = In_channel.read_all "input05.txt" |> parse |> move2

let () = printf "%s\n%s" (get_top res1) (get_top res2)
