let rec quicksort : 'a list -> 'a list = function
  | [] -> []
  | x :: xs ->
    let (a, b) : ('a list * 'a list) = List.partition ((>) x) xs in
    (quicksort a) @ [x] @ (quicksort b)

let rec quickselect (k : int) : 'a list -> 'a option = function
  | [] -> None
  | x :: xs ->
    let (a, b) : ('a list * 'a list) = List.partition ((>) x) xs in
    let l : int = List.length a in
    if k < l then
      quickselect k a
    else if l < k then
      quickselect ((k - l) - 1) b
    else
      Some x

let show (xs : int list) : unit =
  Printf.printf "[ ";
  List.iter (Printf.printf "%d ") xs;
  Printf.printf "]\n"

let () : unit =
  let xs : int list =
    [7; 0; 9; 3; 2; 4; 7; 1; 1; 0; 5; 2; 8; 9; 6; 3; 3; 2; 2] in
  show xs;
  quicksort xs |> show;
  let n : int = List.length xs in
  List.iter
    (fun n -> quickselect n xs |> Option.get |> (Printf.printf "%d\n"))
    [0; n / 2; n - 1]
