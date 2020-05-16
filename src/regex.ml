(* NOTE: See
    `https://deniskyashif.com/2019/02/17/implementing-a-regular-expression-engine/`. *)

type t = {
    mutable is_end : bool;
    transition : (char, t) Hashtbl.t;
    epsilon_transitions : (t) Stack.t;
}

type pair_t = {
    state_start : t;
    state_end : t;
}

let create (is_end : bool) : t =
    {
        is_end = is_end;
        transition = Hashtbl.create 1;
        epsilon_transitions = Stack.create ();
    }

let add_epsilon_transition (state_from : t) (state_to : t) : unit =
    Stack.push state_to state_from.epsilon_transitions

let add_transition (state_from : t) (state_to : t) (token : char) : unit =
    Hashtbl.add state_from.transition token state_to

let from_epsilon () : pair_t =
    let state_start : t = create false in
    let state_end : t = create true in
    add_epsilon_transition state_start state_end;
    {
        state_start = state_start;
        state_end = state_end;
    }

let from_token (token : char) : pair_t =
    let state_start : t = create false in
    let state_end : t = create true in
    add_transition state_start state_end token;
    {
        state_start = state_start;
        state_end = state_end;
    }

let concat (first : pair_t) (second : pair_t) : pair_t =
    add_epsilon_transition first.state_end second.state_start;
    first.state_end.is_end <- false;
    {
        state_start = first.state_start;
        state_end = second.state_end;
    }

let union (first : pair_t) (second : pair_t) : pair_t =
    let state_start = create false in
    add_epsilon_transition state_start first.state_start;
    add_epsilon_transition state_start second.state_start;
    let state_end = create true in
    add_epsilon_transition first.state_end state_end;
    first.state_end.is_end <- false;
    add_epsilon_transition second.state_end state_end;
    second.state_end.is_end <- false;
    {
        state_start = state_start;
        state_end = state_end;
    }

let closure (nfa : pair_t) : pair_t =
    let state_start = create false in
    let state_end = create true in
    add_epsilon_transition state_start state_end;
    add_epsilon_transition state_start nfa.state_start;
    add_epsilon_transition nfa.state_end state_end;
    add_epsilon_transition nfa.state_end nfa.state_start;
    nfa.state_end.is_end <- false;
    {
        state_start = state_start;
        state_end = state_end;
    }

let rec find_state (stack : (t) Stack.t) (state : t) : bool =
    match Stack.pop_opt stack with
        | None -> false
        | Some x ->
            if x = state then
                true
            else
                find_state stack state

let rec add_next_state (state : t) (next_states : (t) Stack.t)
    (prev_states : (t) Stack.t) : unit =
    if Stack.is_empty state.epsilon_transitions then
        Stack.push state next_states
    else
        Stack.copy state.epsilon_transitions
        |> Stack.to_seq
        |> List.of_seq
        |> List.rev
        |> List.iter
            (
                fun x ->
                    if find_state (Stack.copy prev_states) x then
                        ()
                    else
                        (
                            Stack.push x prev_states;
                            add_next_state x next_states prev_states
                        )
            )

let rec find_true (stack : (t) Stack.t) : bool =
    match Stack.pop_opt stack with
        | None -> false
        | Some x ->
            if x.is_end then
                true
            else
                find_true stack

let to_nfa (expression : string) : pair_t =
    let f (stack : (pair_t) Stack.t) (token : char) : unit =
        if token = '*' then
            Stack.push (closure (Stack.pop stack)) stack
        else if token = '|' then
            (
                let right : pair_t = Stack.pop stack in
                let left : pair_t = Stack.pop stack in
                Stack.push (union left right) stack
            )
        else if token = '.' then
            (
                let right : pair_t = Stack.pop stack in
                let left : pair_t = Stack.pop stack in
                Stack.push (concat left right) stack
            )
        else
            Stack.push (from_token token) stack in
    if expression = "" then
        from_epsilon ()
    else
        let stack : (pair_t) Stack.t = Stack.create () in
        String.iter (f stack) expression;
        Stack.pop stack

let search (nfa : pair_t) (expression : string) : bool =
    let states : (t) Stack.t ref = ref (Stack.create ()) in
    add_next_state nfa.state_start !states (Stack.create ());
    let f (token : char) : unit =
        let next_states : (t) Stack.t = Stack.create () in
        Stack.to_seq !states
        |> List.of_seq
        |> List.rev
        |> List.iter
            (
                fun x ->
                    match Hashtbl.find_opt x.transition token with
                        | None -> ()
                        | Some y ->
                            add_next_state y  next_states (Stack.create ())
            );
        states := next_states in
    String.iter f expression;
    find_true !states

let () : unit =
    (* NOTE: Converted from `(a|b)*c`. *)
    let postfix_expression : string = "ab|*c." in
    let nfa : pair_t = to_nfa postfix_expression in
    [|
        "c";
        "ac";
        "bc";
        "abc";
        "bac";
        "aabbc";
        "cc";
        "cab";
        "cba";
        "cc";
    |]
    |> Array.iter (fun x -> Printf.fprintf stdout "%s\t%b\n" x (search nfa x));
    flush stdout
