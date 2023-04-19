open Base
open Stdio

let res' = In_channel.read_all "input01.txt"
          |> Str.split (Str.regexp "\n\n")
          |> List.map ~f:(fun s -> Str.split (Str.regexp "\n") s)
          |> List.map ~f:(fun x -> List.map ~f:Int.of_string x)
          |> List.map ~f:(fun x -> List.fold ~f:(+) ~init:0 x)

let res1 = List.fold ~f:max ~init:0 res'
let res2 = List.take (List.sort ~compare:Poly.descending res') 3
           |> List.fold ~f:(+) ~init:0

let () =
  printf "\n%i\n" res1;
  printf "%i\n" res2
