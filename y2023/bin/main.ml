let read_file file_name =
  let input_channel = open_in file_name in
  let rec read_lines lines =
    try
      let line = input_line input_channel in
      read_lines (line :: lines)
    with End_of_file -> 
      close_in input_channel;
      List.rev lines
  in
  read_lines []

let reverse s =
  s |> String.to_seq
    |> List.of_seq
    |> List.rev
    |> List.to_seq
    |> String.of_seq

let rec first_number s =
  let l = s 
    |> String.to_seq
    |> List.of_seq in 
  match l with
  | [] -> '0'
  | h :: rest -> 
    (match h with
    | '0' .. '9' -> h
    | _ -> rest |> List.to_seq |> String.of_seq |> first_number)

let last_number s =
  s |> reverse
    |> first_number

let first_and_last s =
  let first = first_number s in
  let last = last_number s in
  [first; last]
    |> List.to_seq
    |> String.of_seq

let rec sum list =
  match list with
  | [] -> 0
  | h::t -> h + sum t

let () = 
  let file_name = Sys.argv.(1) in
  read_file file_name 
    |> List.map first_and_last
    |> List.map int_of_string 
    |> sum
    |> string_of_int
    |> print_endline
