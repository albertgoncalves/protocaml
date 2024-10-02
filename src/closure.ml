let closure () : unit -> int =
  let x : int ref = ref 0 in
  (
    fun () ->
      x := !x + 1;
      !x
  )

let () : unit =
  let f : unit -> int = closure () in
  ignore (closure () ());
  ignore (f ());
  ignore (f ());
  Printf.printf "%d\n" (f ())
