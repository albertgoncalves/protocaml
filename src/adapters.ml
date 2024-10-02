(* NOTE: See `https://adventofcode.com/2020/day/10`. *)

let items : int array =
  [|
    0;   1;   2;   3;   4;   7;   8;   11;  12;  13;  14;  15;  18;  19;
    20;  23;  24;  27;  28;  29;  30;  31;  34;  37;  38;  39;  40;  41;
    44;  45;  46;  47;  48;  51;  54;  55;  56;  59;  60;  61;  62;  63;
    66;  67;  68;  69;  70;  73;  74;  75;  76;  77;  80;  81;  82;  83;
    84;  87;  88;  89;  90;  91;  94;  95;  96;  97;  98;  101; 104; 105;
    108; 109; 110; 111; 112; 115; 118; 119; 120; 121; 124; 125; 126; 129;
    130; 131; 132; 133; 136; 137; 140;
  |]

let memo : (int, int) Hashtbl.t =
  Hashtbl.create (Array.length items)

let rec cost (i : int) : int =
  if i = 0 then (
    1
  ) else (
    match Hashtbl.find_opt memo i with
    | Some x -> x
    | None ->
      (
        let rec loop (j : int) (x : int) : int =
          if (j < 0) || (3 < (items.(i) - items.(j))) then (
            x
          ) else (
            (loop (j - 1) (cost j)) + x
          ) in
        let x : int = loop (i - 1) 0 in
        Hashtbl.add memo i x;
        x
      )
  )

let () : unit =
  assert ((cost ((Array.length items) - 1)) = 3100448333024);
  Printf.printf "Done!\n"
