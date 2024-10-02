let epilogue (l : int) : int -> string list = function
  | 0 when l = 0 -> []
  | 0 -> [Printf.sprintf "drop %d" l]
  | r ->
    let n : int = 3 + l + r in
    let xs : string list = [
      (Printf.sprintf "drop %d" n);
      "copy 1";
      "copy 0";
    ] in
    let rec loop (xs : string list) (i : int) : string list =
      if i = n then
        (Printf.sprintf "swap %d" i) :: xs
      else
        loop
          ("rsrv 1" :: (Printf.sprintf "swap %d" i) :: xs)
          (i + 1) in
    loop xs (2 + l) |> List.rev

let () : unit =
  assert ((epilogue 0 0) = []);
  assert ((epilogue 1 0) = ["drop 1"]);
  assert ((epilogue 2 0) = ["drop 2"]);
  assert ((epilogue 3 0) = ["drop 3"]);
  assert ((epilogue 0 1) = [
      "copy 0";
      "copy 1";
      "drop 4";
      "swap 2";
      "rsrv 1";
      "swap 3";
      "rsrv 1";
      "swap 4";
    ]);
  assert ((epilogue 1 1) = [
      "copy 0";
      "copy 1";
      "drop 5";
      "swap 3";
      "rsrv 1";
      "swap 4";
      "rsrv 1";
      "swap 5";
    ]);
  assert ((epilogue 1 3) = [
      "copy 0";
      "copy 1";
      "drop 7";
      "swap 3";
      "rsrv 1";
      "swap 4";
      "rsrv 1";
      "swap 5";
      "rsrv 1";
      "swap 6";
      "rsrv 1";
      "swap 7";
    ]);
  assert ((epilogue 4 3) = [
      "copy 0";
      "copy 1";
      "drop 10";
      "swap 6";
      "rsrv 1";
      "swap 7";
      "rsrv 1";
      "swap 8";
      "rsrv 1";
      "swap 9";
      "rsrv 1";
      "swap 10";
    ]);
  Printf.printf "Done!\n"
