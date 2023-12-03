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

let () = 
  let file_name = Sys.argv.(1) in
  let lines = read_file file_name in  
  Daytwo.solve lines