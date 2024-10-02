(* NOTE: See `https://gist.github.com/kseo/9383472`. *)

type term =
  | Ident of string
  | Lambda of string * term
  | Apply of term * term
  | Let of string * term * term
  | LetRec of string * term * term

let rec term_to_string = function
  | Ident(name) -> name
  | Lambda(arg, body) -> Printf.sprintf "\\%s -> %s" arg (term_to_string body)
  | Apply(fn, arg) -> Printf.sprintf "(%s %s)" (term_to_string fn) (term_to_string arg)
  | Let(ident, value, usage) ->
    Printf.sprintf "let %s = %s in %s" ident (term_to_string value) (term_to_string usage)
  | LetRec(ident, value, usage) ->
    Printf.sprintf "letrec %s = %s in %s" ident (term_to_string value) (term_to_string usage)

exception TypeError of string
exception ParseError of string

type tyvar =
  {
    tyvar_id : int;
    mutable tyvar_instance : ty option;
    mutable tyvar_name : string option;
  }

and tyop =
  {
    tyop_name : string;
    tyop_types : ty list;
  }

and ty =
  | TypeVariable of tyvar
  | TypeOperator of tyop

module TypeSet =
  Set.Make(struct
    type t = ty
    let compare = compare
  end)

let next_variable_id : int ref = ref 0

let make_variable () : ty =
  let new_var = { tyvar_id = !next_variable_id; tyvar_instance = None; tyvar_name = None } in
  incr next_variable_id;
  TypeVariable(new_var)

let next_unique_name : int ref = ref 0

let variable_name (var : tyvar) : string =
  match var with
  | { tyvar_name = Some(name); _ } -> name
  | { tyvar_name = None; _ } ->
    let new_var_name = Printf.sprintf "__%d__" !next_unique_name in
    var.tyvar_name <- Some(new_var_name);
    incr next_unique_name;
    new_var_name

let rec type_to_string : ty -> string = function
  | TypeVariable({ tyvar_instance = Some(instance); _ }) -> type_to_string instance
  | TypeVariable({ tyvar_instance = None; _ } as var) -> variable_name var
  | TypeOperator({ tyop_name; tyop_types = [] }) -> tyop_name
  | TypeOperator({ tyop_name; tyop_types = [a; b] }) ->
    Printf.sprintf "(%s %s %s)" (type_to_string a) tyop_name (type_to_string b)
  | TypeOperator({ tyop_name; tyop_types }) ->
    List.map type_to_string tyop_types
    |> String.concat " "
    |> Printf.sprintf "%s %s" tyop_name

type env = (string * ty) list

let make_fun_type (from_type : ty) (to_type : ty) : ty =
  TypeOperator({ tyop_name = "->"; tyop_types = [from_type; to_type] })

let int_type : ty =
  TypeOperator({ tyop_name = "int"; tyop_types = [] })

let bool_type : ty =
  TypeOperator({ tyop_name = "bool"; tyop_types = [] })

let rec prune t : ty =
  match t with
  | TypeVariable({ tyvar_instance = Some(instance); _ } as var) ->
    let new_instance = prune instance in
    var.tyvar_instance <- Some(new_instance);
    new_instance
  | _ -> t

let rec occurs_in_type (var : ty) (t : ty) : bool =
  match prune t with
  | pruned when pruned = var -> true
  | TypeOperator({ tyop_types; _ }) -> occurs_in var tyop_types
  | _ -> false

and occurs_in (t : ty) (types : ty list) : bool =
  List.exists (occurs_in_type t) types

let is_integer_literal (name : string) : bool =
  try (
    ignore (int_of_string name);
    true
  )
  with Failure(_) -> false

let is_generic (var : ty) (non_generic : TypeSet.t) : bool =
  not (occurs_in var (TypeSet.to_list non_generic))

let fresh (t : ty) (non_generic : TypeSet.t) : ty =
  let table : (ty, ty) Hashtbl.t  = Hashtbl.create 8 in
  let rec freshrec (tp : ty) : ty =
    match prune tp with
    | TypeVariable(_) as p ->
      if is_generic p non_generic then
        match Hashtbl.find_opt table p with
        | None ->
          (
            let new_var = make_variable () in
            ignore (Hashtbl.add table p new_var);
            new_var
          )
        | Some(var) -> var
      else
        p
    | TypeOperator({ tyop_types; _ } as op) ->
      TypeOperator({ op with tyop_types = List.map freshrec tyop_types })
  in
  freshrec t

let get_type (name : string) (env : env) (non_generic : TypeSet.t) : ty =
  match List.find_opt (fun (s, _) -> s = name) env with
  | Some((_, var)) -> fresh var non_generic
  | None ->
    if is_integer_literal name then
      int_type
    else
      raise (ParseError ("Undefined symbol " ^ name))

let rec unify (t1 : ty) (t2 : ty) : unit =
  match (prune t1, prune t2) with
  | ((TypeVariable(v) as a), b) ->
    if a <> b then (
      if occurs_in_type a b then
        raise (TypeError "Recursive unification");
      v.tyvar_instance <- Some(b)
    )
  | ((TypeOperator(_) as a), (TypeVariable(_) as b)) -> unify b a
  | ((TypeOperator({ tyop_name = name1; tyop_types = types1 }) as a),
     (TypeOperator({ tyop_name = name2; tyop_types = types2 }) as b)) ->
    if (name1 <> name2 || List.length types1 <> List.length types2) then
      raise
        (TypeError (Printf.sprintf "Type mismatch %s != %s" (type_to_string a) (type_to_string b)));
    ignore (List.map2 unify types1 types2)

let analyse (term' : term) (env' : env) : ty =
  let rec analyserec term env non_generic : ty =
    match term with
    | Ident(name) -> get_type name env non_generic
    | Apply(fn, arg) ->
      let fun_ty = analyserec fn env non_generic in
      let arg_ty = analyserec arg env non_generic in
      let ret_ty = make_variable () in
      unify (make_fun_type arg_ty ret_ty) fun_ty;
      ret_ty
    | Lambda(arg, body) ->
      let arg_ty = make_variable () in
      let ret_ty = analyserec body ((arg, arg_ty) :: env) (TypeSet.add arg_ty non_generic) in
      make_fun_type arg_ty ret_ty
    | Let(v, defn, body) ->
      analyserec body ((v, analyserec defn env non_generic) :: env) non_generic
    | LetRec(v, defn, body) ->
      let new_ty = make_variable () in
      let new_env = ((v, new_ty) :: env) in
      let defn_ty = analyserec defn new_env (TypeSet.add new_ty non_generic) in
      unify new_ty defn_ty;
      analyserec body new_env non_generic
  in
  analyserec term' env' TypeSet.empty

let try_exp (env : env) (term : term) : unit =
  Printf.printf "%s : " (term_to_string term);
  (try
     type_to_string (analyse term env)
   with
     ParseError(msg) | TypeError(msg) -> msg)
  |> Printf.printf "%s\n"

let () =
  let var1 = make_variable () in
  let var2 = make_variable () in
  let pair_ty = TypeOperator({ tyop_name = "*"; tyop_types = [var1; var2] }) in
  let var3 = make_variable () in

  let my_env =
    [ ("pair", make_fun_type var1 (make_fun_type var2 pair_ty));
      ("true", bool_type);
      ("false", bool_type);
      ("cond", make_fun_type bool_type (make_fun_type var3 (make_fun_type var3 var3)));
      ("zero", make_fun_type int_type bool_type);
      ("pred", make_fun_type int_type int_type);
      ("times", make_fun_type int_type (make_fun_type int_type int_type))
    ]
  in
  [
    LetRec(
      "factorial",
      Lambda(
        "n",
        Apply(
          Apply(Apply(Ident("cond"), Apply(Ident("zero"), Ident("n"))), Ident("1")),
          Apply(
            Apply(Ident("times"), Ident("n")),
            Apply(Ident("factorial"), Apply(Ident("pred"), Ident("n")))))),
      Apply(Ident("factorial"), Ident("5")));
    Lambda(
      "x",
      Apply(
        Apply(Ident("pair"), Apply(Ident("x"), Ident("3"))),
        Apply(Ident("x"), Ident("true"))));
    Apply(Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))), Apply(Ident("f"), Ident("true")));
    Let(
      "f",
      Lambda("x", Ident("x")),
      Apply(
        Apply(
          Ident("pair"),
          Apply(Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))), Ident("false"))),
        Apply(Ident("f"), Ident("true"))));
    Lambda("f", Apply(Ident("f"), Ident("f")));
    Let("g", Lambda("f", Ident("5")), Apply(Ident("g"), Ident("g")));
    Lambda(
      "g",
      Let(
        "f",
        Lambda("x", Ident("g")),
        Apply(
          Apply(Ident("pair"), Apply(Ident("f"), Ident("3"))),
          Apply(Ident("f"), Ident("true")))));
    Lambda("f", Lambda("g", Lambda("arg", Apply(Ident("g"), Apply(Ident("f"), Ident("arg"))))));
  ]
  |> List.iter (try_exp my_env)
