module SS = Set.Make(String)

let remove_front s =
  s |> String.split_on_char ':' 
    |> List.tl
    |> List.hd 
    |> String.trim

let get_cards s =
  let cards = s 
    |> String.split_on_char '|'
    |> List.map(fun x -> String.split_on_char ' ' x) in
  (List.hd cards, List.hd (List.tl cards))


let intersect (a, b) =
  SS.inter (SS.of_list a) (SS.of_list b)
  |> SS.elements |> List.filter (fun x -> x <> "")

let score l =
  if l = [] then 0
  else
  let rec aux l acc =
    match l with
    | [] -> acc
    | _::[] -> acc
    | _::t -> aux t (acc * 2)
  in aux l 1

let process s =
  s |> remove_front
    |> get_cards 
    |> intersect
    |> score

let solve l =
  l |> List.map process
    |> List.fold_left (+) 0 
    |> string_of_int 
    |> print_endline 