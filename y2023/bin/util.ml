open Stdio

let read_file_lines file_name =
  let input_channel = In_channel.create file_name in
  let rec read_lines lines =
    try
      let line = In_channel.input_line_exn input_channel in
      read_lines (line :: lines)
    with End_of_file -> 
      In_channel.close input_channel;
      List.rev lines 
  in
  read_lines []

let read_file file_name = 
  In_channel.read_all file_name