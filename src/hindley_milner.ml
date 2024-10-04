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
  | TypeVariable of int
  | TypeOperator of (string * type' list)

let rec type_to_string (map : (int, type') Hashtbl.t) : type' -> string =
  function
  | TypeVariable id ->
    (
      match Hashtbl.find_opt map id with
      | Some instance -> type_to_string map instance
      | None -> Printf.sprintf "__%d__" id
    )
  | TypeOperator (op_name, []) -> op_name
  | TypeOperator (op_name, [a; b]) ->
    Printf.sprintf "(%s %s %s)" (type_to_string map a) op_name (type_to_string map b)
  | TypeOperator (op_name, types) ->
    Printf.sprintf "%s %s" op_name @@ String.concat " " @@ List.map (type_to_string map) types

let int_type : type' =
  TypeOperator ("int", [])

let bool_type : type' =
  TypeOperator ("bool", [])

let fun_type (from_type : type') (to_type : type') : type' =
  TypeOperator ("->", [from_type; to_type])

module TypeSet =
  Set.Make (struct
    type t = int
    let compare = compare
  end)

let fail (msg : string) : 'a =
  raise @@ Failure msg

let next_variable_id : int ref = ref 0

let make_variable () : (type' * int) =
  let id = !next_variable_id in
  let new_var = TypeVariable id in
  incr next_variable_id;
  (new_var, id)

let rec prune (map : (int, type') Hashtbl.t): type' -> type' =
  function
  | TypeVariable id as t0 ->
    (
      match Hashtbl.find_opt map id with
      | None -> t0
      | Some t1 ->
        (
          let new_instance = prune map t1 in
          Hashtbl.add map id new_instance;
          new_instance
        )
    )
  | t -> t

let rec occurs_in_type (map : (int, type') Hashtbl.t) (var : int) (t : type') : bool =
  match prune map t with
  | TypeVariable id when id = var -> true
  | TypeOperator (_, types) -> occurs_in map var types
  | _ -> false

and occurs_in (map : (int, type') Hashtbl.t) (t : int) : type' list -> bool =
  List.exists (occurs_in_type map t)

let is_integer_literal (name : string) : bool =
  try
    (
      ignore @@ int_of_string name;
      true
    )
  with
    Failure _ -> false

let is_generic (map : (int, type') Hashtbl.t) (var : int) (non_generic : TypeSet.t) : bool =
  not @@ occurs_in map var @@ List.map (fun id -> TypeVariable id) @@ TypeSet.to_list non_generic

let fresh (map : (int, type') Hashtbl.t) (non_generic : TypeSet.t) : type' -> type' =
  let table : (int, type') Hashtbl.t = Hashtbl.create 8 in
  let rec loop (t : type') : type' =
    match prune map t with
    | TypeVariable id as p ->
      if is_generic map id non_generic then
        match Hashtbl.find_opt table id with
        | None ->
          (
            let new_var = fst @@ make_variable () in
            Hashtbl.add table id new_var;
            new_var
          )
        | Some var -> var
      else
        p
    | TypeOperator (op_name, types) -> TypeOperator (op_name, List.map loop types)
  in
  loop

let get_type (map : (int, type') Hashtbl.t) (name : string) (env : (string * type') list)
    (non_generic : TypeSet.t) : type' =
  match List.find_opt (fun (s, _) -> s = name) env with
  | Some (_, var) -> fresh map non_generic var
  | None ->
    if is_integer_literal name then
      int_type
    else
      fail @@ "Undefined symbol " ^ name

let rec unify (map : (int, type') Hashtbl.t) (t1 : type') (t2 : type') : unit =
  match (prune map t1, prune map t2) with
  | ((TypeVariable id as a), b) | (b, (TypeVariable id as a)) ->
    if a <> b then (
      if occurs_in_type map id b then (
        fail "Recursive unification"
      );
      Hashtbl.add map id b;
    )
  | ((TypeOperator (name1, types1) as a), (TypeOperator (name2, types2) as b)) ->
    if (name1 <> name2 || List.length types1 <> List.length types2) then (
      fail @@ Printf.sprintf "Type mismatch %s != %s" (type_to_string map a) (type_to_string map b)
    );
    List.iter2 (unify map) types1 types2

let rec analyse (map : (int, type') Hashtbl.t) (term : term) (env : (string * type') list)
    (non_generic : TypeSet.t) : type' =
  match term with
  | Ident name -> get_type map name env non_generic
  | Apply (fn, arg) ->
    let fun_ty = analyse map fn env non_generic in
    let arg_ty = analyse map arg env non_generic in
    let ret_ty = fst @@ make_variable () in
    unify map (fun_type arg_ty ret_ty) fun_ty;
    ret_ty
  | Lambda (arg, body) ->
    let (arg_ty, arg_id) = make_variable () in
    let ret_ty = analyse map body ((arg, arg_ty) :: env) (TypeSet.add arg_id non_generic) in
    fun_type arg_ty ret_ty
  | Let (ident, value, usage) ->
    analyse map usage ((ident, analyse map value env non_generic) :: env) non_generic
  | LetRecs (pairs, usage) ->
    let (env'', non_generic'', pairs'') =
      List.fold_left
        (fun (env', non_generic', pairs') (ident, value) ->
           let (new_ty, new_id) = make_variable () in
           ((ident, new_ty) :: env', TypeSet.add new_id non_generic', (value, new_ty) :: pairs'))
        (env, non_generic, [])
        pairs
    in
    (
      List.iter
        (fun (value, new_ty) -> unify map new_ty (analyse map value env'' non_generic''))
        pairs'';
      analyse map usage env'' non_generic
    )

let try_exp (env : (string * type') list) (id : int) (term : term) : unit =
  let map : (int, type') Hashtbl.t = Hashtbl.create 16 in
  next_variable_id := id;
  Printf.printf
    "%s : %s\n"
    (term_to_string term)
    (try
       type_to_string map (analyse map term env TypeSet.empty)
     with
       Failure msg -> msg)

let () =
  let var1 = fst @@ make_variable () in
  let var2 = fst @@ make_variable () in
  let pair_ty = TypeOperator ("*", [var1; var2]) in
  let var3 = fst @@ make_variable () in

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
