(* NOTE: See `https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm`. *)

type term =
    | Var of string
    | Term of string * term list

type substitution = (string * term) list

let rec occurs (x : string) : term -> bool = function
    | Var y -> x = y
    | Term (_, s) -> List.exists (occurs x) s

let rec substitute (s : term) (x : string) : term -> term = function
    | Var y when x = y -> s
    | Var _ as t -> t
    | Term (f, u) -> Term (f, List.map (substitute s x) u)

let apply (s : substitution) (t : term) : term =
    List.fold_right (fun (x, u) -> substitute u x) s t

let rec unify_one : (term * term) -> substitution = function
    | (Var x, Var y) when x = y -> []
    | (Var x, (Var _ as t)) -> [(x, t)]
    | (Term (f, sc), Term (g, tc))
        when (f = g) && (List.length sc = List.length tc) ->
        unify (List.combine sc tc)
    | (Term _, Term _) -> failwith "Not unifiable: head symbol conflict"
    | ((Var x, (Term _ as t)) | ((Term _ as t), Var x)) when occurs x t ->
        failwith "Not unifiable: circularity"
    | ((Var x, (Term _ as t)) | ((Term _ as t), Var x)) -> [(x, t)]

and unify : (term * term) list -> substitution = function
    | [] -> []
    | (x, y) :: t ->
        let t' = unify t in
        List.rev_append (unify_one ((apply t' x), (apply t' y))) t'

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
            (Var "d", Var "a");
            (Var "a", Term ("f", [Var "e"]));
            (Term ("f", [Var "a"; Var "b"]), Term ("f", [Var "c"; Var "d"]));
        ] in
    Printf.printf "TERMS\n";
    List.iter (fun ab -> pair_to_string ab |> Printf.printf "  %s\n") terms;
    try
        Printf.printf "\nUNIFIERS\n";
        unify terms |> sub_to_string |> List.iter (Printf.printf "  %s\n")
    with Failure (x) ->
        Printf.printf "%s\n" x
