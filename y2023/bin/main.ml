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

let only_numbers line =
  let chars = List.init (String.length line) (String.get line) in
  let rec only_numbers' chars numbers =
    match chars with
    | [] -> numbers
    | c :: rest -> 
      if c >= '0' && c <= '9' then
        only_numbers' rest (c :: numbers)
      else
        only_numbers' rest numbers
  in
  only_numbers' chars []

let char_of_list chars = 
  List.rev chars |> List.to_seq |> String.of_seq

let first_and_last_char chars =
  match chars with
  | [] -> ['0']
  | [c] -> [c;c]
  | [_; _] -> 
    chars
  | c :: rest -> 
    [c; List.hd (List.rev rest)]

let rec sum list =
  match list with
  | [] -> 0
  | h::t -> h + sum t

let () = 
  let file_name = Sys.argv.(1) in
  read_file file_name 
    |> List.map only_numbers
    |> List.map first_and_last_char
    |> List.map char_of_list
    |> List.map int_of_string 
    |> sum
    |> string_of_int
    |> print_endline
