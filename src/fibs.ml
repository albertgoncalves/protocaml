module Lazy = CamlinternalLazy

type 'a t = Cons of 'a * ('a t Lazy.t)

let head (Cons (x, _) : 'a t) : 'a = x

let tail (Cons (_, xs) : 'a t) : 'a t = Lazy.force xs

let rec drop (n : int) (xs : 'a t) : 'a t =
    if n <= 0 then
        xs
    else
        (drop[@tailcall]) (n - 1) (tail xs)

let rec zip_with
        (f : 'a -> 'a -> 'a)
        (Cons (x, xs) : 'a t)
        (Cons (y, ys) : 'a t) : 'a t =
    Cons (f x y, lazy (zip_with f (Lazy.force xs) (Lazy.force ys)))

let rec fibs : int t =
    Cons (0, lazy (Cons (1, lazy (zip_with (+) fibs (tail fibs)))))

let () : unit =
    let x : int = head (drop 50 fibs) in
    assert (x = 12586269025);
    Printf.printf "%d\n%!" x
