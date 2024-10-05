(* NOTE: See `https://gist.github.com/kseo/9383472`. *)

type term' =
  | Ident of string
  | Lambda of string * term'
  | Apply of term' * term'
  | Let of string * term' * term'
  | LetRecs of (string * term') list * term'

let rec term_to_string =
  function
  | Ident name -> name
  | Lambda (arg, body) -> Printf.sprintf "\\%s -> %s" arg (term_to_string body)
  | Apply (fn, arg) -> Printf.sprintf "(%s %s)" (term_to_string fn) (term_to_string arg)
  | Let (ident, value, body) ->
    Printf.sprintf "let %s = %s in %s" ident (term_to_string value) (term_to_string body)
  | LetRecs (pairs, body) ->
    Printf.sprintf
      "let rec %s in %s"
      (String.concat " and "
       @@ List.map
         (fun (ident, value) -> Printf.sprintf "%s = %s" ident (term_to_string value))
         pairs)
      (term_to_string body)

type type' =
  | Var of int
  | Op of (string * type' list)

let rec type_to_string (links : (int, type') Hashtbl.t) : type' -> string =
  function
  | Var k ->
    (
      match Hashtbl.find_opt links k with
      | Some instance -> type_to_string links instance
      | None -> Printf.sprintf "__%d__" k
    )
  | Op (op_name, []) -> op_name
  | Op (op_name, [a; b]) ->
    Printf.sprintf "(%s %s %s)" (type_to_string links a) op_name (type_to_string links b)
  | Op (op_name, types) ->
    Printf.sprintf "%s %s" op_name @@ String.concat " " @@ List.map (type_to_string links) types

type links' = (int, type') Hashtbl.t

type env' = (string * type') list

let op_int : type' =
  Op ("int", [])

let op_bool : type' =
  Op ("bool", [])

let op_fn (from_type : type') (to_type : type') : type' =
  Op ("->", [from_type; to_type])

module IdSet =
  Set.Make (struct
    type t = int
    let compare = compare
  end)

let fail (msg : string) : 'a =
  raise @@ Failure msg

let global_k : int ref = ref 0

let next_var () : (int * type') =
  let k = !global_k in
  let type_ = Var k in
  incr global_k;
  (k, type_)

let rec prune (links : links'): type' -> type' =
  function
  | Var k as type_ ->
    (
      match Hashtbl.find_opt links k with
      | None -> type_
      | Some type_ ->
        (
          Hashtbl.remove links k;
          let type_ = prune links type_ in
          Hashtbl.add links k type_;
          type_
        )
    )
  | type_ -> type_

let rec occurs (links : links') (other : int) (type_ : type') : bool =
  match prune links type_ with
  | Var k when k = other -> true
  | Var _ -> false
  | Op (_, types) -> any_occurs links other types

and any_occurs (links : links') (other : int) : type' list -> bool =
  List.exists @@ occurs links other

let is_integer_literal (name : string) : bool =
  try
    (
      ignore @@ int_of_string name;
      true
    )
  with
    Failure _ -> false

let is_generic (links : links') (other : int) (non_generics : IdSet.t) : bool =
  not @@ any_occurs links other @@ List.map (fun k -> Var k) @@ IdSet.to_list non_generics

let fresh (links : (int, type') Hashtbl.t) (non_generics : IdSet.t) : type' -> type' =
  let generics : (int, int) Hashtbl.t = Hashtbl.create 8 in
  let rec loop (type_ : type') : type' =
    match prune links type_ with
    | Var k as type_ ->
      if is_generic links k non_generics then
        match Hashtbl.find_opt generics k with
        | Some k -> Var k
        | None ->
          (
            let (other, type_) = next_var () in
            Hashtbl.add generics k other;
            type_
          )
      else
        type_
    | Op (op, types) -> Op (op, List.map loop types)
  in
  loop

let ident_to_type (links : links') (ident : string) (env : env') (non_generics : IdSet.t) : type' =
  match List.find_opt (fun (other, _) -> ident = other) env with
  | Some (_, type_) -> fresh links non_generics type_
  | None ->
    if is_integer_literal ident then
      op_int
    else
      fail @@ "Undefined symbol " ^ ident

let rec unify (links : links') (type0 : type') (type1 : type') : unit =
  match (prune links type0, prune links type1) with
  | ((Var k as type0), type1) | (type1, (Var k as type0)) ->
    if type0 <> type1 then (
      if occurs links k type1 then (
        fail "Recursive unification"
      );
      Hashtbl.add links k type1;
    )
  | (Op (op0, types0), Op (op1, types1)) ->
    (
      if (op0 <> op1 || List.length types0 <> List.length types1) then (
        fail @@ Printf.sprintf
          "Type mismatch %s != %s"
          (type_to_string links type0)
          (type_to_string links type1)
      );
      List.iter2 (unify links) types0 types1
    )

let rec term_to_type (links : links') (term : term') (env : env') (non_generics : IdSet.t) : type' =
  match term with
  | Ident ident -> ident_to_type links ident env non_generics
  | Apply (fn, arg) ->
    (
      let fn_type = term_to_type links fn env non_generics in
      let arg_type = (term_to_type links arg env non_generics) in
      let ret_type = snd @@ next_var () in
      unify links (op_fn arg_type ret_type) fn_type;
      ret_type
    )
  | Lambda (arg, body) ->
    let (arg_k, arg_type) = next_var () in
    op_fn arg_type
    @@ term_to_type links body ((arg, arg_type) :: env)
    @@ IdSet.add arg_k non_generics
  | Let (ident, value, body) ->
    term_to_type links body ((ident, term_to_type links value env non_generics) :: env) non_generics
  | LetRecs (pairs, body) ->
    let (rec_env, rec_non_generics, pairs) =
      List.fold_left
        (fun (env, non_generics, pairs) (ident, value) ->
           let (k, type_) = next_var () in
           ((ident, type_) :: env, IdSet.add k non_generics, (value, type_) :: pairs))
        (env, non_generics, [])
        pairs
    in
    (
      List.iter
        (fun (value, type_) -> unify links type_ @@ term_to_type links value rec_env rec_non_generics)
        pairs;
      term_to_type links body rec_env non_generics
    )

let try_term_to_type (env : env') (k : int) (term : term') : unit =
  let links : links' = Hashtbl.create 16 in
  global_k := k;
  Printf.printf
    "%s : %s\n"
    (term_to_string term)
    (try
       type_to_string links (term_to_type links term env IdSet.empty)
     with
       Failure msg -> msg)

let () =
  let var0 = snd @@ next_var () in
  let var1 = snd @@ next_var () in
  let pair_ty = Op ("*", [var0; var1]) in
  let var2 = snd @@ next_var () in

  let env =
    [("pair", op_fn var0 (op_fn var1 pair_ty));
     ("true", op_bool);
     ("false", op_bool);
     ("cond", op_fn op_bool (op_fn var2 (op_fn var2 var2)));
     ("zero", op_fn op_int op_bool);
     ("pred", op_fn op_int op_int);
     ("times", op_fn op_int (op_fn op_int op_int))]
  in

  List.iter (try_term_to_type env !global_k)
    [LetRecs
       ([("factorial",
          Lambda
            ("n",
             Apply
               (Apply (Apply (Ident "cond", Apply (Ident "zero", Ident "n")), Ident "1"),
                Apply
                  (Apply (Ident "times", Ident "n"),
                   Apply (Ident "factorial", Apply (Ident "pred", Ident "n"))))))],
        Apply (Ident "factorial", Ident "5"));
     Lambda
       ("x",
        Apply
          (Apply (Ident "pair", Apply (Ident "x", Ident "3")),
           Apply (Ident "x", Ident "true")));
     Apply (Apply (Ident "pair", Apply (Ident "f", Ident "4")), Apply (Ident "f", Ident "true"));
     Let
       ("f",
        Lambda ("x", Ident "x"),
        Apply
          (Apply
             (Ident "pair",
              Apply (Apply (Ident "pair", Apply (Ident "f", Ident "4")), Ident "false")),
           Apply (Ident "f", Ident "true")));
     Lambda ("f", Apply (Ident "f", Ident "f"));
     Let ("g", Lambda ("f", Ident "5"), Apply (Ident "g", Ident "g"));
     Lambda
       ("g",
        Let
          ("f",
           Lambda ("x", Ident "g"),
           Apply
             (Apply (Ident "pair", Apply (Ident "f", Ident "3")),
              Apply (Ident "f", Ident "true"))));
     Lambda ("f", Lambda ("g", Lambda ("arg", Apply (Ident "g", Apply (Ident "f", Ident "arg")))));
     LetRecs
       ([("f", Lambda ("x", Apply (Ident "g", Ident "x")));
         ("g", Lambda ("x", Apply (Ident "f", Ident "x")))],
        Let
          ("x",
           Lambda ("y", Apply (Ident "f", Ident "0")),
           Apply (Apply (Ident "pair", Ident "g"), Ident "x")));
     LetRecs
       ([("f", Lambda ("x", Apply (Ident "g", Ident "x")));
         ("g", Lambda ("x", Apply (Ident "f", Ident "x")))],
        Apply
          (Apply (Ident "pair", Apply (Ident "g", Ident "true")), Apply (Ident "f", Ident "0")));
     Let
       ("f", Lambda ("x", Ident "x"),
        Let
          ("x",
           Apply
             (Apply (Ident "pair", Apply (Ident "f", Ident "true")),
              Apply (Ident "f", Ident "0")),
           Ident "f"))]
