open Base
open Stdio

type fs_path = string list

type fs_tree =
  | File of {name: string; size: int}
  | Dir  of {name: string; files: fs_tree list}
[@@deriving show]

let rec add (files : fs_tree list) path node =
  match path with
  | [] -> node :: files
  | dirname :: path_rest ->
    match files with
    | (Dir {name; files}) :: rest
      when (String.equal name dirname) -> Dir {name; files=add files path_rest node} :: rest
    | file :: rest                      -> file :: add rest path node
    | []                               -> failwith "invalid path"

let rec build_fs (current_path : fs_path) (tree : fs_tree list) = function
  | [] -> tree
  | line :: lines ->
    match String.split ~on:' ' line with
    | ["$"; "cd"; "/"]  -> build_fs [] tree lines
    | ["$"; "cd"; ".."] -> build_fs (List.tl_exn current_path) tree lines
    | ["$"; "cd"; name] -> build_fs (name :: current_path) tree lines
    | ["$"; "ls"]       -> build_fs current_path tree lines
    | ["dir"; name] ->
      let new_tree = add tree (List.rev current_path) (Dir {name; files=[]}) in
      build_fs current_path new_tree lines
    | [size; name] ->
      let size = Int.of_string size in
      let new_tree = add tree (List.rev current_path) (File {name; size}) in
      build_fs current_path new_tree lines
    | _ -> failwith "invalid input"

let build_root lines = Dir {name="/"; files=build_fs [] [] lines}

let sum = List.sum (module Int)

let rec tree_size = function
  | File {size; _} -> size
  | Dir {files; _} -> sum ~f:tree_size files

let rec total_size_filtered tree =
  match tree with
  | Dir {files; _} -> let size = tree_size tree in
    let size = if size <= 100_000 then size else 0 in
    size + sum ~f:total_size_filtered files
  | File _ -> 0

let rec dir_sizes tree =
  match tree with
  | File _ -> []
  | Dir {files; _} -> tree_size tree :: List.concat (List.map ~f:dir_sizes files)

let dir_to_delete need_space sizes =
  match sizes |> List.filter ~f:(fun size -> size >= need_space) |> List.min_elt ~compare:Int.compare with
  | Some x -> x
  | None -> failwith "no dir to delete"

let tree = In_channel.read_all "input07.txt" |> String.split_lines |> build_root
let () = tree |> show_fs_tree |> printf "\n%s"
(* let () = tree |> tree_size |> printf "\n%i" *)
let () = tree |> total_size_filtered |> printf "\n%i\n"

let () =
  let total_size = tree_size tree in
  let free = 70_000_000 - total_size in
  let need = 30_000_000 - free in
  dir_sizes tree |> dir_to_delete need |> printf "%i\n"
