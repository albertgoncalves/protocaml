(* NOTE: See `https://www.cs.cornell.edu/courses/cs3110/2018sp/l/12-streams/notes.html` *)

type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let head (Cons (x, _) : 'a lazylist) : 'a =
  x

let tail (Cons (_, xs) : 'a lazylist) : 'a lazylist =
  xs ()

let rec take (n : int) (xs : 'a lazylist) : 'a list =
  if n <= 0 then
    []
  else
    head xs :: take (n - 1) (tail xs)

let rec drop (n : int) (xs : 'a lazylist) : 'a lazylist =
  if n <= 0 then
    xs
  else
    drop (n - 1) (tail xs)

let rec zip_with
    (f : 'a -> 'b -> 'c)
    (Cons (l, ls) : 'a lazylist)
    (Cons (r, rs) : 'b lazylist) : 'c lazylist =
  Cons (f l r, fun () -> zip_with f (ls ()) (rs ()))

let rec fibs : int lazylist =
  Cons (0, fun () -> Cons (1, fun () -> zip_with (+) fibs (tail fibs)))

let () : unit =
  Printf.printf "[ ";
  fibs
  |> drop 5
  |> take 10
  |> List.iter (Printf.printf "%d ");
  Printf.printf "]\n";
