open Base
open Stdio

type monkey_id_t = int [@@deriving show]

type monkey_t = {
  id:        monkey_id_t;
  items:     int list;
  op:        int -> int;
  test:      int -> bool;
  divider:   int;
  on_true:   monkey_id_t;
  on_false:  monkey_id_t;
  inspect_n: int;
} [@@deriving show]

let parse_ints line =
  let open Re2 in
  let p = create_exn "[0-9]+" in
  find_all_exn p line |> List.map ~f:Int.of_string

let parse_int line =
  line |> parse_ints |> List.hd_exn

let parse_op line =
  let open Re2 in
  let open Re2.Match in
  let p = create_exn "old (.) (old|[0-9]+)" in
  let m = List.hd_exn (get_matches_exn p line) in
  let opstr = get_exn ~sub:(`Index 1) m in
  let arg   = get_exn ~sub:(`Index 2) m in
  let op = match opstr with
           | "+" -> Int.( + )
           | "*" -> Int.( * )
           | _   -> failwith ("unknown op: " ^ opstr) in
  let gety x = match arg with
               | "old" -> x
               | arg   -> Int.of_string arg in
  (fun x -> op x (gety x))

let parse_test line =
  let d = parse_int line in
  (fun x -> x % d = 0)

let parse_monkey lines =
  match lines with
    | [lbl_l; items_l; op_l; test_l; true_l; false_l] ->
      { id        = parse_int lbl_l;
        items     = parse_ints items_l;
        op        = parse_op op_l;
        test      = parse_test test_l;
        divider   = parse_int test_l;
        on_true   = parse_int true_l;
        on_false  = parse_int false_l;
        inspect_n = 0;
      }
    | _ -> failwith "can't parse monkey"

let parse_all s =
  let open List in
  let monkeys = s |> String.split_lines
                  |> filter ~f:(fun line -> String.(line <> ""))
                  |> chunks_of ~length:6
                  |> map ~f:parse_monkey in
  let map = Map.of_list_with_key (module Int) ~get_key:(fun {id; _} -> id) monkeys in
  match map with
  | `Duplicate_key _ -> failwith "invalid input"
  | `Ok m -> m

let remove_first_item monkey =
  match monkey with
  | None   -> failwith "no such monkey"
  | Some m -> let tail = match m.items with
    | [] -> failwith "monkey is empty"
    | _ :: xs -> xs in
  { m with items = tail}

let add_last_item item monkey =
  match monkey with
  | None   -> failwith "no such monkey"
  | Some m -> { m with items = List.append m.items [item]}

let inspect_inc monkey =
  match monkey with
  | None   -> failwith "no such monkey"
  | Some m -> { m with inspect_n = m.inspect_n + 1 }

let throw monkey_map from_id to_id worry_level =
  let m1 = Map.update monkey_map from_id ~f:remove_first_item in
  let m2 = Map.update m1 to_id ~f:(add_last_item worry_level) in
  m2

let inspect monkey_map monkey_id =
  (* divider trick to keep worry level from growing indefinitely
     https://jactl.io/blog/2023/04/17/advent-of-code-2022-day11.html *)
  let divider_product = Map.fold ~init:1 ~f:(fun ~key:_ ~data acc -> acc * data.divider) monkey_map in
  let m = Map.find_exn monkey_map monkey_id in
  match m.items with
  | []        -> failwith "monkey is empty"
  | item :: _ ->
    let worry_level = m.op item % divider_product in
    let throw_to_id = if m.test worry_level then m.on_true else m.on_false in
    let m = Map.update monkey_map monkey_id ~f:inspect_inc in
    throw m monkey_id throw_to_id worry_level

let rec run round_num curr_monkey_id monkey_map =
  let open Map in
  let monkey_count = length monkey_map in
  match round_num with
  | 0 -> monkey_map
  | n ->
    let m = Map.find_exn monkey_map curr_monkey_id in
    match m.items with
    | [] ->
      let next_monkey_id = (m.id + 1) % monkey_count in
      let next_round = if next_monkey_id = 0 then (n - 1) else n in
      run next_round next_monkey_id monkey_map
    | _ ->
      let monkey_map' = inspect monkey_map curr_monkey_id in
      run round_num curr_monkey_id monkey_map'

let calc_monkey_business monkey_map =
  let open List in
  let sorted = monkey_map |> Map.to_alist |> map ~f:snd |> map ~f:(fun m -> m.inspect_n) |> sort ~compare:Int.descending in
  nth_exn sorted 0 * nth_exn sorted 1

let show m =
  m |> Map.to_alist |> [%show: (int * monkey_t) list]

let before = "input11.txt" |> In_channel.read_all |> parse_all
let after  = before |> run 10000 0
let _ = before |> show |> print_endline
let _ = after  |> show |> print_endline
let _ = after  |> calc_monkey_business |> printf "%i\n"
