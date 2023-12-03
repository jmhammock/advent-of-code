let reverse s =
  s |> String.to_seq
    |> List.of_seq
    |> List.rev
    |> List.to_seq
    |> String.of_seq

let lookup = 
  [ "one", "1"; "two", "2"; "three", "3"; 
    "four", "4"; "five", "5"; "six", "6"; 
    "seven", "7"; "eight", "8"; "nine", "9"; ]

let lookup_number s =
  List.find_map (fun (word, value) -> 
    if word = s || word = (reverse s) then Some value else None) lookup

let rec find_word_number chars =
  match chars with
  | [] -> None
  | _  -> 
    (match lookup_number (chars |> List.to_seq |> String.of_seq) with
    | Some number -> Some number
    | None -> chars |> List.rev |> List.tl |> List.rev |> find_word_number)

let rec first_number s =
  let l = s 
    |> String.to_seq
    |> List.of_seq in 
  match l with
  | [] -> '0'
  | h :: rest -> 
    (match h with
    | '0' .. '9' -> h
    | _ -> 
      (match find_word_number l with
      | Some number -> number.[0]
      | None -> rest |> List.to_seq |> String.of_seq |> first_number))

let last_number s =
  s |> reverse
    |> first_number

let first_and_last s =
  let first = first_number s in
  let last = last_number s in
  [first; last]
    |> List.to_seq
    |> String.of_seq

let solve l =
  l |> List.map first_and_last
    |> List.map int_of_string
    |> List.fold_left (+) 0
    |> string_of_int
    |> print_endline
