type 'a my_lazy = {
  mutable thunk : unit -> 'a
}

type 'a stream = Stream of ('a * 'a stream my_lazy)

let undefined () : 'a =
  exit 1

let new_lazy (f : unit -> 'a) : 'a my_lazy =
  let t : 'a my_lazy = { thunk = undefined } in
  t.thunk <- (
    fun () ->
      let x : 'a = f () in
      t.thunk <- (fun () -> x);
      x
  );
  t

let force (l : 'a my_lazy) : 'a =
  l.thunk ()

let new_stream (x : 'a) (f : unit -> 'a stream) : 'a stream =
  Stream (x, new_lazy f)

let head (Stream (x, _) : 'a stream) : 'a =
  x

let tail (Stream (_, l) : 'a stream) : 'a stream =
  force l

let rec zip_with
    (f : 'a -> 'a -> 'a) (a : 'a stream) (b : 'a stream) : 'a stream =
  new_stream (f (head a) (head b)) (fun () -> zip_with f (tail a) (tail b))

let rec drop (n : int) (s : 'a stream) : 'a stream =
  if n = 0 then
    s
  else
    drop (n - 1) (tail s)

let () : unit =
  let fibs : int stream ref = ref (new_stream 0 undefined) in
  fibs := (
    new_stream 0 (
      fun () -> new_stream 1 (
          fun () -> zip_with (+) !fibs (tail !fibs)
        )
    )
  );
  Printf.printf "%d\n" (head (drop 50 !fibs))
