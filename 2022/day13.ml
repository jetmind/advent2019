open Base
open Stdio

type packet =
  | Int of int
  | List of packet list
  [@@deriving show, equal]

let parse_packet s =
  let rec parse_next = function
    | '[' :: rest ->
      let items, rest = parse_list rest in
      List items, rest
    | '0'..'9' as d :: rest ->
      let item, rest = parse_num 0 (d :: rest) in
      Int item, rest
    | ',' :: rest -> parse_next rest
    | ']' :: _ -> failwith "unmatched ]"
    | x -> failwith (String.concat ["parse: invalid input "; (String.of_char_list x)])

  and parse_num = fun acc input ->
    match input with
    | '0'..'9' as d :: rest -> parse_num (10 * acc + Char.get_digit_exn d) rest
    | rest -> acc, rest

  and parse_list = function
    | ']' :: rest -> [], rest
    | tokens ->
      let item, rest = parse_next tokens in
      let items, rest = parse_list rest in
      item :: items, rest
  in
  let chars = String.to_list s in
  let parsed, _ = parse_next chars in parsed

let rec cmp = function
  | Int a, Int b when a < b -> `Lt
  | Int a, Int b when a > b -> `Gt
  | Int a, Int b when a = b -> `Eq
  | List a, Int b -> cmp (List a, List [(Int b)])
  | Int a, List b -> cmp (List [(Int a)], List b)
  | List [], List[] -> `Eq
  | List [], List (_ :: _) -> `Lt
  | List (_ :: _), List [] -> `Gt
  | List (a :: ax), List (b :: bx) ->
    let res = match cmp (a, b) with
    | `Eq -> cmp (List ax, List bx)
    | res -> res
    in res
  | p -> failwith (String.concat [([%show: packet * packet] p); "deez nuts"])

let solve1 str =
  let open List in
  str |> String.split_lines |> filter ~f:(Fn.non String.is_empty) |> chunks_of ~length:2
  |> map ~f:(fun l -> parse_packet (List.nth_exn l 0), parse_packet (List.nth_exn l 1))
  |> map ~f:cmp
  |> filter_mapi ~f:(fun i res -> match res with `Lt -> Some (i + 1) | _ -> None)
  |> sum (module Int) ~f:Fn.id

let solve2 str =
  let open List in
  let inp = str |> String.split_lines |> filter ~f:(Fn.non String.is_empty) |> map ~f:parse_packet in
  let d1 = parse_packet "[[2]]" in
  let d2 = parse_packet "[[6]]" in
  (d1::d2::inp)
  |> sort ~compare:(fun a b -> match cmp (a, b) with | `Eq -> 0 | `Lt -> -1 | `Gt -> 1)
  |> filter_mapi ~f:(fun i p -> if equal_packet p d1 || equal_packet p d2 then Some (i + 1) else None)
  |> List.fold ~init:1 ~f:( * )

let inpex = "input13ex.txt" |> In_channel.read_all
let inp1 = "input13.txt" |> In_channel.read_all
let () = inpex |> solve1 |> Int.to_string |> print_endline
let () = inp1 |> solve1 |> Int.to_string |> print_endline
let () = inpex |> solve2 |> Int.to_string |> print_endline
let () = inp1 |> solve2 |> Int.to_string |> print_endline
