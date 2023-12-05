let () = 
  let file_name = Sys.argv.(1) in
  let lines = Util.read_file file_name in
  Dayfour.solve lines 