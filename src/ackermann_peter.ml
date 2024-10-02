let rec ackermann : (int * int) -> int = function
  | (0, n) -> n + 1
  | (m, 0) -> ackermann (m - 1, 1)
  | (m, n) -> ackermann (m - 1, ackermann (m, n - 1))

let () : unit =
  ackermann (3, 9)
  |> Printf.printf "%d\n"
