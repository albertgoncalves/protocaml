module type LinkedList_t = sig
  type _ t
  val construct : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val pop_at : 'a t -> int -> 'a option
  val print : ('a -> string) -> 'a t -> unit
end

module LinkedList : LinkedList_t = struct
  type 'a _node = {
    value : 'a;
    mutable next : 'a _node option;
  }

  type 'a t = {
    mutable head : 'a _node option;
  }

  let construct () : 'a t = {
    head = None;
  }

  let push (list_ : 'a t) (v : 'a) : unit =
    let next : 'a _node option = Some {
        value = v;
        next = list_.head;
      } in
    list_.head <- next

  let pop (list_ : 'a t) : 'a option =
    let f (node : 'a _node) : 'a =
      let v : 'a = node.value in
      list_.head <- node.next;
      v in
    Option.map f list_.head

  type 'a nopt = 'a _node option

  let pop_at (list_ : 'a t) (i : int) : 'a option =
    let rec f : (int * 'a nopt * 'a nopt * 'a nopt) -> 'a option = function
      | (0, None, Some c, (Some _ as n)) ->
        (
          list_.head <- n;
          Some c.value
        )
      | (0, Some p, Some c, (Some _ as n)) ->
        (
          p.next <- n;
          Some c.value
        )
      | (0, _, _, _) -> None
      | (i, _, (Some _ as c), (Some n as n')) -> f (i - 1, c, n', n.next)
      | _ -> None in
    let f' (node : 'a _node) : 'a option =
      f (i, None, (Some node), node.next) in
    Option.bind list_.head f'

  let print (to_string : 'a -> string) (list_ : 'a t) : unit =
    let rec f : 'a _node option -> unit = function
      | None -> ()
      | Some node ->
        (
          Printf.fprintf stdout " %s" (to_string node.value);
          f node.next
        ) in
    Printf.fprintf stdout "LinkedList.t        : [";
    f list_.head;
    Printf.fprintf stdout "]\n"
end

let () : unit =
  let list_ : int LinkedList.t = LinkedList.construct () in
  for i = 0 to 10 do
    LinkedList.push list_ i
  done;
  LinkedList.print string_of_int list_;
  let f (i : int) : unit =
    match LinkedList.pop_at list_ i with
    | None -> Printf.fprintf stdout "LinkedList.pop_at %d : None\n" i
    | Some v ->
      Printf.fprintf stdout "LinkedList.pop_at %d : Some %d\n" i v in
  f 4;
  f 2;
  f 7;
  f 7;
  f 1;
  f 0;
  LinkedList.print string_of_int list_;
  let v : int option ref = ref (LinkedList.pop list_) in
  while !v <> None do
    let v' : int = Option.get !v in
    Printf.fprintf stdout "LinkedList.pop      : Some %d\n" v';
    v := LinkedList.pop list_
  done;
  LinkedList.print string_of_int list_;
  LinkedList.push list_ 11;
  LinkedList.print string_of_int list_;
  flush stdout
