let counter : int ref = ref 0

let f (x : string) : string =
  let c : int = !counter in
  incr counter;
  Printf.sprintf "%s%d" x c

let () : unit =
  print_endline (f "1");
  print_endline (f "1");
  print_endline (f "1");
  flush stdout
