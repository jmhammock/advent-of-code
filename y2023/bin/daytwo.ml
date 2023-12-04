module M = Map.Make(String)

type cubes = {
  red: int option;
  blue: int option;
  green: int option;
}

type game = {
  id: int;
  hints: cubes list;
}

let game_id_of_string s =
  Scanf.sscanf s "Game %d" (fun id -> id)

let index_of_game_id s =
  String.index s ':'

let hints_of_string s =
  let start = index_of_game_id s + 1 in
  String.sub s start (String.length s - start)

let cubes_of_hint s =
  let t = s |> String.split_on_char ','
    |> List.map (fun s -> s |> String.trim)
    |> List.map (fun s -> Scanf.sscanf s "%d %s" (fun n c -> (c, n)))
    |> List.to_seq
    |> M.of_seq in
  {
    red = M.find_opt "red" t;
    blue = M.find_opt "blue" t;
    green = M.find_opt "green" t;
  }

let hint_of_string s =
  s |> String.split_on_char ';'
    |> List.map (fun s -> s |> String.trim)
    |> List.map cubes_of_hint

let game_of_string s =
  let id = game_id_of_string s in
  let hints = hints_of_string s |> hint_of_string in
  { id; hints }

let max_cubes l =
  l |> List.fold_left(fun acc x -> {
    red = max acc.red x.red;
    blue = max acc.blue x.blue;
    green = max acc.green x.green;
  }) { red = None; blue = None; green = None }

let valid_cubes c =
  match c with
    | { red = Some r; blue = Some b; green = Some g } -> r <= 12 && b <= 14 && g <= 13
    | { red = Some r; blue = Some b; green = None } -> r <= 12 && b <= 14
    | { red = Some r; blue = None; green = Some g } -> r <= 12 && g <= 13
    | { red = None; blue = Some b; green = Some g } -> b <= 14 && g <= 13
    | { red = Some r; blue = None; green = None } -> r <= 12
    | { red = None; blue = Some b; green = None } -> b <= 14
    | { red = None; blue = None; green = Some g } -> g <= 13
    | _ -> false

let solve l =
  l |> List.map game_of_string
    |> List.filter(fun x -> x.hints |> List.for_all valid_cubes)
    |> List.map(fun x -> x.id)
    |> List.fold_left(+)0
    |> string_of_int
    |> print_endline

let solve2 l =
  l |> List.map game_of_string
    |> List.map(fun x -> x.hints |> max_cubes)
    |> List.map(fun cubes -> Option.get cubes.red * Option.get cubes.blue * Option.get cubes.green)
    |> List.fold_left(+)0
    |> string_of_int
    |> print_endline