let () = 
  let file_name = Sys.argv.(1) in
  try
    let lines = Util.read_lines file_name in
    Daynine.solve lines 
  with Failure msg ->
    print_endline msg 