(* NOTE: See `https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm`. *)

type term =
    | Var of string
    | Term of string * term list

type substitution = (string * term) list

let rec occurs (x : string) (t : term) : bool =
    match t with
        | Var y -> x = y
        | Term (_, s) -> List.exists (occurs x) s

let rec substitute (s : term) (x : string) (t : term) : term =
    match t with
        | Var y ->
            if x = y then
                s
            else
                t
        | Term (f, u) -> Term (f, List.map (substitute s x) u)

let apply (s : substitution) (t : term) : term =
    List.fold_right (fun (x, u) -> substitute u x) s t

let rec unify_one (s : term) (t : term) : substitution =
    match (s, t) with
        | (Var x, Var y) ->
            if x = y then
                []
            else
                [(x, t)]
        | (Term (f, sc), Term (g, tc)) ->
            if (f = g) && (List.length sc = List.length tc) then
                unify (List.combine sc tc)
            else
                failwith "Not unifiable: head symbol conflict"
        | ((Var x, (Term (_, _) as t)) | ((Term (_, _) as t), Var x)) ->
            if occurs x t then
                failwith "Not unifiable: circularity"
            else
                [(x, t)]

and unify (s : (term * term) list) : substitution =
    match s with
        | [] -> []
        | (x, y) :: t ->
            let t2 = unify t in
            List.append (unify_one (apply t2 x) (apply t2 y)) t2

let rec term_to_string : term -> string = function
    | Var x -> Printf.sprintf "Var %s" x
    | Term (x, xs) ->
        List.map term_to_string xs
        |> String.concat ", "
        |> Printf.sprintf "Term (%s, [%s])" x

let sub_to_string (s : substitution) : string list =
    List.map (fun (x, xs) -> Printf.sprintf "(%s, %s)" x (term_to_string xs)) s

let pair_to_string ((a, b) : (term * term)) : string =
    Printf.sprintf "(%s, %s)" (term_to_string a) (term_to_string b)

let () : unit =
    let terms : (term * term) list =
        [
            (Var "a", Var "d");
            (Term ("f", [Var "a"; Var "b"]), Term ("f", [Var "c"; Var "d"]));
        ] in
    Printf.printf "TERMS\n";
    List.iter (fun ab -> pair_to_string ab |> Printf.printf "  %s\n") terms;
    try
        Printf.printf "\nUNIFIERS\n";
        unify terms
        |> sub_to_string
        |> List.iter (Printf.printf "  %s\n")
    with Failure (x) ->
        Printf.printf "%s\n" x
