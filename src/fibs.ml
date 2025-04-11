type 'a stream = Stream of ('a Lazy.t) * ('a stream Lazy.t)

let head (Stream (x, _)) = Lazy.force x

let tail (Stream (_, xs)) = Lazy.force xs

let rec drop n xs =
  if n <= 0 then
    xs
  else
    (drop[@tailcall]) (n - 1) (tail xs)

let rec zip_with f xs ys =
  Stream (lazy (f (head xs) (head ys)), lazy (zip_with f (tail xs) (tail ys)))

let () =
  let rec fibs = Stream (lazy 0, lazy (Stream (lazy 1, lazy (zip_with (+) fibs (tail fibs))))) in
  let x = head (drop 50 fibs) in
  Printf.printf "%d\n%!" x;
  assert (x = 12586269025)
