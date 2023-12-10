open Base
open Stdio

let parse ~prefix input =
  let s = input 
    |> String.chop_prefix ~prefix in
  match s with
  | None -> []
  | Some s -> 
    let pat = Str.regexp " +" in
    s |> String.strip
    |> Str.split pat
    |> List.map ~f:Int.of_string

let parse_time = parse ~prefix:"Time:"
let parse_distance = parse ~prefix:"Distance:"

let records l = 
  l |> List.map ~f:(fun (t, d) -> 
    List.range 1 t 
    |> List.fold_left ~init:0 ~f:(fun acc x ->
      let distance = x * (t - x) in
      if distance > d then (acc + 1) else acc))
    
let solve l =
  let time = 
    match l |> List.hd with
    | None -> []
    | Some l -> l |> parse_time in
  let distance =
    match l |> List.last with
    | None -> []
    | Some l -> l |> parse_distance in
  match List.zip time distance with
  | Unequal_lengths -> raise (Failure "Invalid input")
  | Ok l -> l |> records |> List.fold_left ~init:1 ~f:( * ) |> Int.to_string |> print_endline
