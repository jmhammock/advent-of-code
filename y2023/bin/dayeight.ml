open Base
open Stdio

let rec parse_coords s acc =
  try
    let pat = Str.regexp "\\([A-Z]+\\)" in
    let _ = Str.search_forward pat s 0 in
    let matched = Str.matched_group 1 s in
    parse_coords (Str.replace_first pat "" s) (matched :: acc)
  with Stdlib.Not_found -> acc |> List.rev

let rec traverse l m acc = 
  match l with
  | [] -> assert false
  | hd :: tl -> 
    match acc with 
    | ((dest, (left, right)), c) ->
      let open List.Assoc in
      if String.equal dest "ZZZ" then c 
      else match hd with
      | 'L' -> traverse (tl @ [hd]) m ((left, find_exn ~equal:String.equal m left), c + 1)
      | 'R' -> traverse (tl @ [hd]) m ((right, find_exn ~equal:String.equal m right), c + 1)
      | _ -> assert false

let solve f =
  let parts = Str.split (Str.regexp "\n\n") f in
  let lr = String.to_list (List.hd_exn parts) in
  let map = String.split_lines (List.nth_exn parts 1)
    |> List.map ~f:(fun x -> 
      match parse_coords x [] with
      | [dest; left; right] -> (dest, (left, right)) 
      | _ -> assert false
      ) in
  let start = List.Assoc.find_exn ~equal:String.equal map "AAA" in
  traverse lr map (("AAA", start), 0) |> Int.to_string |> print_endline  