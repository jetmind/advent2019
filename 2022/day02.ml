open Base
open Stdio

type hand = Rock | Paper | Scissors
type round_outcome = Draw | Win | Lose

let char_to_hand = function
  | "A" -> Rock | "B" -> Paper | "C" -> Scissors
  | "X" -> Rock | "Y" -> Paper | "Z" -> Scissors
  | _ -> failwith "invalid input"

let outcome = function
  | (Rock, Rock)  | (Paper, Paper)    | (Scissors, Scissors) -> Draw
  | (Paper, Rock) | (Scissors, Paper) | (Rock, Scissors)     -> Lose
  | (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock)     -> Win

let outcome_score = function | Lose -> 0 | Draw -> 3 | Win -> 6

let hand_score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let total_score (their_hand, my_hand) =
  let o = outcome (their_hand, my_hand) in
  hand_score my_hand + outcome_score o

let parse1 s = String.split_lines s
               |> List.map ~f:(String.lsplit2_exn ~on:' ')
               |> List.map ~f:(fun (a, b) -> (char_to_hand a, char_to_hand b))

let char_to_outcome = function | "X" -> Lose | "Y" -> Draw | "Z" -> Win | _ -> failwith "invalid input"

let parse_round (a, b) = match (char_to_hand a, char_to_outcome b) with
  | (a, Draw) -> (a, a)
  | (Rock, Win) -> (Rock, Paper)
  | (Paper, Win) -> (Paper, Scissors)
  | (Scissors, Win) -> (Scissors, Rock)
  | (Rock, Lose) -> (Rock, Scissors)
  | (Paper, Lose) -> (Paper, Rock)
  | (Scissors, Lose) -> (Scissors, Paper)

let parse2 s = String.split_lines s
               |> List.map ~f:(String.lsplit2_exn ~on:' ')
               |> List.map ~f:parse_round

let res1 = In_channel.read_all "input02.txt" |> parse1 |> List.map ~f:total_score |> List.fold ~f:(+) ~init:0
let res2 = In_channel.read_all "input02.txt" |> parse2 |> List.map ~f:total_score |> List.fold ~f:(+) ~init:0

let () = printf "%i\n%i" res1 res2
