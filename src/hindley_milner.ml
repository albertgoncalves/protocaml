(* NOTE: See `https://gist.github.com/kseo/9383472`. *)

type term =
  | Ident of string
  | Lambda of string * term
  | Apply of term * term
  | Let of string * term * term
  | LetRecs of (string * term) list * term

let rec term_to_string =
  function
  | Ident name -> name
  | Lambda (arg, body) -> Printf.sprintf "\\%s -> %s" arg (term_to_string body)
  | Apply (fn, arg) -> Printf.sprintf "(%s %s)" (term_to_string fn) (term_to_string arg)
  | Let (ident, value, usage) ->
    Printf.sprintf "let %s = %s in %s" ident (term_to_string value) (term_to_string usage)
  | LetRecs (pairs, usage) ->
    Printf.sprintf
      "let rec %s in %s"
      (String.concat " and "
       @@ List.map
         (fun (ident, value) -> Printf.sprintf "%s = %s" ident (term_to_string value))
         pairs)
      (term_to_string usage)

type type' =
  | TypeVariable of { id : int; mutable instance : type' option }
  | TypeOperator of (string * type' list)

let rec type_to_string : type' -> string =
  function
  | TypeVariable { instance = Some instance; _ } -> type_to_string instance
  | TypeVariable { id; instance = None } -> Printf.sprintf "__%d__" id
  | TypeOperator (op_name, []) -> op_name
  | TypeOperator (op_name, [a; b]) ->
    Printf.sprintf "(%s %s %s)" (type_to_string a) op_name (type_to_string b)
  | TypeOperator (op_name, types) ->
    Printf.sprintf "%s %s" op_name @@ String.concat " " @@ List.map type_to_string types

let int_type : type' =
  TypeOperator ("int", [])

let bool_type : type' =
  TypeOperator ("bool", [])

let fun_type (from_type : type') (to_type : type') : type' =
  TypeOperator ("->", [from_type; to_type])

module TypeSet =
  Set.Make (struct
    type t = type'
    let compare = compare
  end)

let fail (msg : string) : 'a =
  raise @@ Failure msg

let next_variable_id : int ref = ref 0

let make_variable () : type' =
  let new_var = TypeVariable { id = !next_variable_id; instance = None } in
  incr next_variable_id;
  new_var

let rec prune : type' -> type' =
  function
  | TypeVariable ({ instance = Some instance; _ } as var) ->
    let new_instance = prune instance in
    var.instance <- Some new_instance;
    new_instance
  | t -> t

let rec occurs_in_type (var : type') (t : type') : bool =
  match prune t with
  | pruned when pruned = var -> true
  | TypeOperator (_, types) -> occurs_in var types
  | _ -> false

and occurs_in (t : type') : type' list -> bool =
  List.exists (occurs_in_type t)

let is_integer_literal (name : string) : bool =
  try
    (
      ignore @@ int_of_string name;
      true
    )
  with
    Failure _ -> false

let is_generic (var : type') (non_generic : TypeSet.t) : bool =
  not @@ occurs_in var @@ TypeSet.to_list non_generic

let fresh (non_generic : TypeSet.t) : type' -> type' =
  let table : (type', type') Hashtbl.t = Hashtbl.create 8 in
  let rec loop (t : type') : type' =
    match prune t with
    | TypeVariable _ as p ->
      if is_generic p non_generic then
        match Hashtbl.find_opt table p with
        | None ->
          (
            let new_var = make_variable () in
            ignore @@ Hashtbl.add table p new_var;
            new_var
          )
        | Some var -> var
      else
        p
    | TypeOperator (op_name, types) -> TypeOperator (op_name, List.map loop types)
  in
  loop

let get_type (name : string) (env : (string * type') list) (non_generic : TypeSet.t) : type' =
  match List.find_opt (fun (s, _) -> s = name) env with
  | Some (_, var) -> fresh non_generic var
  | None ->
    if is_integer_literal name then
      int_type
    else
      fail @@ "Undefined symbol " ^ name

let rec unify (t1 : type') (t2 : type') : unit =
  match (prune t1, prune t2) with
  | ((TypeVariable var as a), b) | (b, (TypeVariable var as a)) ->
    if a <> b then (
      if occurs_in_type a b then (
        fail "Recursive unification"
      );
      var.instance <- Some b
    )
  | ((TypeOperator (name1, types1) as a), (TypeOperator (name2, types2) as b)) ->
    if (name1 <> name2 || List.length types1 <> List.length types2) then (
      fail @@ Printf.sprintf "Type mismatch %s != %s" (type_to_string a) (type_to_string b)
    );
    List.iter2 unify types1 types2

let rec analyse (term : term) (env : (string * type') list) (non_generic : TypeSet.t) : type' =
  match term with
  | Ident name -> get_type name env non_generic
  | Apply (fn, arg) ->
    let fun_ty = analyse fn env non_generic in
    let arg_ty = analyse arg env non_generic in
    let ret_ty = make_variable () in
    unify (fun_type arg_ty ret_ty) fun_ty;
    ret_ty
  | Lambda (arg, body) ->
    let arg_ty = make_variable () in
    let ret_ty = analyse body ((arg, arg_ty) :: env) (TypeSet.add arg_ty non_generic) in
    fun_type arg_ty ret_ty
  | Let (ident, value, usage) ->
    analyse usage ((ident, analyse value env non_generic) :: env) non_generic
  | LetRecs (pairs, usage) ->
    let (env'', non_generic'', pairs'') =
      List.fold_left
        (fun (env', non_generic', pairs') (ident, value) ->
           let new_ty = make_variable () in
           ((ident, new_ty) :: env', TypeSet.add new_ty non_generic', (value, new_ty) :: pairs'))
        (env, non_generic, [])
        pairs
    in
    (
      List.iter (fun (value, new_ty) -> unify new_ty (analyse value env'' non_generic'')) pairs'';
      analyse usage env'' non_generic
    )

let try_exp (env : (string * type') list) (id : int) (term : term) : unit =
  next_variable_id := id;
  Printf.printf
    "%s : %s\n"
    (term_to_string term)
    (try
       type_to_string (analyse term env TypeSet.empty)
     with
       Failure msg -> msg)

let () =
  let var1 = make_variable () in
  let var2 = make_variable () in
  let pair_ty = TypeOperator ("*", [var1; var2]) in
  let var3 = make_variable () in

  let env =
    [("pair", fun_type var1 (fun_type var2 pair_ty));
     ("true", bool_type);
     ("false", bool_type);
     ("cond", fun_type bool_type (fun_type var3 (fun_type var3 var3)));
     ("zero", fun_type int_type bool_type);
     ("pred", fun_type int_type int_type);
     ("times", fun_type int_type (fun_type int_type int_type))]
  in

  List.iter (try_exp env !next_variable_id)
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
