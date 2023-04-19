open Base
open Stdio

type char_set = (char, Char.comparator_witness) Set.t

type rucksack = {left: char_set; right: char_set}

let parse_line line =
  let l = String.to_list line in
  let middle = List.length l / 2 in
  let (l, r) = List.split_n l middle in
  let left = Set.of_list (module Char) l in
  let right = Set.of_list (module Char) r in
  {left; right}

let common_item_type sack = Set.inter sack.left sack.right

let item_priority i = match i with
  | 'a'..'z' -> Char.to_int i - 96
  | 'A'..'Z' -> Char.to_int i - 38
  | _ -> failwith "invalid input"

let res1 =
  In_channel.read_all "input03.txt"
  |> String.split_lines
  |> List.map ~f:parse_line
  |> List.map ~f:common_item_type
  |> List.map ~f:(Set.sum (module Int) ~f:item_priority)
  |> List.sum (module Int) ~f:Fn.id


let line_to_set line = line |> String.to_list |> Set.of_list (module Char)

let rec group_priority_sum = function
  | [] -> 0
  | a :: b :: c :: rest ->
    let prio = Set.inter a b |> Set.inter c |> Set.sum (module Int) ~f:item_priority in
    prio + group_priority_sum rest
  | _ -> failwith "invalid input"

let res2 =
  In_channel.read_all "input03.txt"
  |> String.split_lines
  |> List.map ~f:line_to_set
  |> group_priority_sum

let () = printf "%i\n%i" res1 res2
