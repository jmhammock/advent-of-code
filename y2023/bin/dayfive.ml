open Base
open Stdio

type map_entry = {
  dest: int;
  source: int;
  range: int;
}

let parse_seeds l =
  match l with
  | [] -> raise (Failure "No seeds")
  | hd :: _ ->
    match String.chop_prefix hd ~prefix:"seeds: " with
      | Some s -> s |> String.split ~on:' ' |> List.map ~f:Int.of_string
      | None -> raise (Failure "Invalid seeds")

let parse_maps l =
  match l with
  | [] -> raise (Failure "No maps")
  | _ :: tl ->
    tl |> List.map ~f:(fun s -> 
      match s |> String.split_lines with 
      | [] -> raise (Failure "Invalid map")
      | _ :: tl -> 
        tl |> List.map ~f:(fun l -> 
          match String.split l ~on:' ' with
          | [dest; source; range] -> {dest = Int.of_string dest; source = Int.of_string source; range = Int.of_string range}
          | _ -> raise (Failure "Invalid map")
        ))

let is_in_range m source  =
  source >= m.source && source < m.source + m.range

let rec filter_destination l source acc =
  match l with 
  | [] -> acc
  | hd :: tl -> 
    match hd |> List.filter ~f:(fun m -> is_in_range m source) with
    | [] -> filter_destination tl source acc
    | hd :: _ -> filter_destination tl (source - (hd.source - hd.dest)) (source - (hd.source - hd.dest))

let solve s =
  let parts = String.substr_replace_all s ~pattern:"\n\n" ~with_:"X" |> String.split ~on:'X' in
  let seeds = parse_seeds parts in
  let maps = parse_maps parts in
  try
    let locations = seeds
      |> List.map ~f:(fun s -> filter_destination maps s s) 
      |> List.min_elt ~compare:Int.compare in
    match locations with
    | Some l -> printf "Best location: %d\n" l
    | None -> raise (Failure "No locations")
  with Failure s -> printf "%s\n" s