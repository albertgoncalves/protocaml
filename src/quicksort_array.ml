let swap (xs : 'a array) (i : int) (j : int) =
  let t : 'a = xs.(i) in
  xs.(i) <- xs.(j);
  xs.(j) <- t

let partition (xs : 'a array) (low : int) (high : int) : int =
  let pivot : 'a = xs.(high) in
  let i : int ref = low - 1 |> ref in
  for j = low to (high - 1) do
    if xs.(j) <= pivot then (
      incr i;
      swap xs !i j
    )
  done;
  incr i;
  swap xs !i high;
  !i

let rec quicksort (xs : 'a array) (low : int) (high : int) : unit =
  if low < high then (
    let i : int = partition xs low high in
    quicksort xs low (i - 1);
    quicksort xs (i + 1) high
  )

let show (xs : int array) : unit =
  Printf.printf "[ ";
  Array.iter (Printf.printf "%d ") xs;
  Printf.printf "]\n"

let () : unit =
  let xs : int array =
    [|7; 0; 9; 3; 2; 4; 7; 1; 1; 0; 5; 2; 8; 9; 6; 3; 3; 2; 2|] in
  let n : int = Array.length xs in
  show xs;
  quicksort xs 0 (n - 1);
  show xs;
