open Stdio

let read_lines file_name =
  In_channel.with_file file_name ~f:In_channel.input_lines

let read_file file_name = 
  In_channel.read_all file_name 