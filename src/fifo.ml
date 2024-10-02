module type FIFO_t = sig
  type _ t
  val construct : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val print : ('a -> string) -> 'a t -> unit
end

module FIFO : FIFO_t = struct
  type 'a _node = {
    value : 'a;
    mutable next : 'a _node option;
  }

  type 'a t = {
    mutable first : 'a _node option;
    mutable last : 'a _node option;
  }

  let construct () : 'a t = {
    first = None;
    last = None;
  }

  let push (queue : 'a t) (v : 'a) : unit =
    let next : 'a _node option = Some {
        value = v;
        next = None;
      } in
    match (queue.first, queue.last) with
    | (Some f, None) ->
      (
        f.next <- next;
        queue.last <- next
      )
    | (_, Some l) ->
      (
        l.next <- next;
        queue.last <- next
      )
    | _ -> queue.first <- next

  let pop (queue : 'a t) : 'a option =
    let f (node : 'a _node) : 'a =
      let v : 'a = node.value in
      queue.first <- node.next;
      if queue.first == queue.last then
        queue.last <- None;
      v in
    Option.map f queue.first

  let print (to_string : 'a -> string) (queue : 'a t) : unit =
    let rec f : 'a _node option -> unit = function
      | None -> ()
      | Some node ->
        (
          Printf.fprintf stdout " %s" (to_string node.value);
          f node.next
        ) in
    Printf.fprintf stdout "FIFO.t   : [";
    f queue.first;
    Printf.fprintf stdout "]\n"
end

let () : unit =
  let queue : int FIFO.t = FIFO.construct () in
  let f () : unit =
    match FIFO.pop queue with
    | None -> Printf.fprintf stdout "FIFO.pop : None\n"
    | Some v -> Printf.fprintf stdout "FIFO.pop : Some %d\n" v in
  for i = 0 to 10 do
    FIFO.push queue i
  done;
  FIFO.print string_of_int queue;
  for _ = 4 downto 0 do
    f ()
  done;
  FIFO.print string_of_int queue;
  for _ = 6 downto 0 do
    f ()
  done;
  FIFO.print string_of_int queue;
  for i = 11 to 20 do
    FIFO.push queue i
  done;
  FIFO.print string_of_int queue;
  while FIFO.pop queue <> None; do
    ()
  done;
  FIFO.print string_of_int queue;
  flush stdout
