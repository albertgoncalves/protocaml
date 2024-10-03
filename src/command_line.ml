let () =
  for i = 0 to Array.length Sys.argv - 1 do
    Printf.printf "%s\n" Sys.argv.(i)
  done
