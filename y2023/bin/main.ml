open Util

let () = 
  let file_name = Sys.argv.(1) in
  try
    (* let lines = Util.read_lines file_name in *)
    let file = read_file file_name in
    Dayeight.solve file 
  with Failure msg ->
    print_endline msg 