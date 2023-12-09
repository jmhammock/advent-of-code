let () = 
  let file_name = Sys.argv.(1) in
  (* let lines = Util.read_file_lines file_name in *)
  let content = Util.read_file file_name in
  Dayfive.solve content