(* NOTE: See
    `https://deniskyashif.com/2019/02/17/implementing-a-regular-expression-engine/`. *)

type 'a _Stack = {
    contents : 'a option array;
    mutable index : int;
}

let new_stack () : 'a _Stack =
    {
        contents = Array.make 5 None;
        index = 0;
    }

let push (x : 'a) (xs : 'a _Stack) : unit =
    xs.contents.(xs.index) <- Some x;
    xs.index <- xs.index + 1

let pop (xs : 'a _Stack) : 'a =
    if xs.index <> 0 then
        (
            xs.index <- xs.index - 1;
            let x : 'a option = xs.contents.(xs.index) in
            xs.contents.(xs.index) <- None;
            match x with
                | Some y -> y
                | None -> exit 1
        )
    else
        exit 1

let pop_opt (xs : 'a _Stack) : 'a option =
    if xs.index <> 0 then
        (
            xs.index <- xs.index - 1;
            let x : 'a option = xs.contents.(xs.index) in
            xs.contents.(xs.index) <- None;
            x
        )
    else
        None

type _State = {
    mutable is_end : bool;
    transition : (char, _State) Hashtbl.t;
    epsilon_transitions : _State _Stack;
}

let new_state (is_end : bool) : _State =
    {
        is_end = is_end;
        transition = Hashtbl.create 1;
        epsilon_transitions = new_stack ();
    }

let add_epsilon_transition (state_from : _State) (state : _State) : unit =
    push state state_from.epsilon_transitions

let add_transition (state_from : _State) (state : _State) (token : char)
    : unit =
    Hashtbl.add state_from.transition token state

type _StatePair = {
    state_start : _State;
    state_end : _State;
}

let from_epsilon () : _StatePair =
    let state_start : _State = new_state false in
    let state_end : _State = new_state true in
    add_epsilon_transition state_start state_end;
    {
        state_start = state_start;
        state_end = state_end;
    }

let from_token (token : char) : _StatePair =
    let state_start : _State = new_state false in
    let state_end : _State = new_state true in
    add_transition state_start state_end token;
    {
        state_start = state_start;
        state_end = state_end;
    }

let concat (first : _StatePair) (second : _StatePair) : _StatePair =
    add_epsilon_transition first.state_end second.state_start;
    first.state_end.is_end <- false;
    {
        state_start = first.state_start;
        state_end = second.state_end;
    }

let union (first : _StatePair) (second : _StatePair) : _StatePair =
    let state_start : _State = new_state false in
    add_epsilon_transition state_start first.state_start;
    add_epsilon_transition state_start second.state_start;
    let state_end : _State = new_state true in
    add_epsilon_transition first.state_end state_end;
    first.state_end.is_end <- false;
    add_epsilon_transition second.state_end state_end;
    second.state_end.is_end <- false;
    {
        state_start = state_start;
        state_end = state_end;
    }

let closure (nfa : _StatePair) : _StatePair =
    let state_start : _State = new_state false in
    let state_end : _State = new_state true in
    add_epsilon_transition state_start state_end;
    add_epsilon_transition state_start nfa.state_start;
    add_epsilon_transition nfa.state_end state_end;
    add_epsilon_transition nfa.state_end nfa.state_start;
    nfa.state_end.is_end <- false;
    {
        state_start = state_start;
        state_end = state_end;
    }

let rec find_state (stack : _State _Stack) (s : _State) : bool =
    match pop_opt stack with
        | None -> false
        | Some x ->
            if x = s then
                true
            else
                find_state stack s

let rec add_next_state (s : _State) (next_states : _State _Stack)
    (prev_states : _State _Stack) : unit =
    if s.epsilon_transitions.index = 0 then
        push s next_states
    else
        Array.iter
            (
                fun x ->
                    match x with
                        | None -> ()
                        | Some y ->
                            if find_state prev_states y then
                                ()
                            else
                                (
                                    push y prev_states;
                                    add_next_state y next_states prev_states
                                )
            )
            s.epsilon_transitions.contents

let rec find_true (stack : _State _Stack) : bool =
    match pop_opt stack with
        | None -> false
        | Some x ->
            if x.is_end then
                true
            else
                find_true stack

let to_nfa (expression : string) : _StatePair =
    let f (stack : _StatePair _Stack) (token : char) : unit =
        if token = '*' then
            push (closure (pop stack)) stack
        else if token = '|' then
            (
                let right : _StatePair = pop stack in
                let left : _StatePair = pop stack in
                push (union left right) stack
            )
        else if token = '.' then
            (
                let right : _StatePair = pop stack in
                let left : _StatePair = pop stack in
                push (concat left right) stack
            )
        else
            push (from_token token) stack in
    if expression = "" then
        from_epsilon ()
    else
        let stack : _StatePair _Stack = new_stack () in
        String.iter (f stack) expression;
        pop stack

let search (nfa : _StatePair) (expression : string) : bool =
    let states : _State _Stack ref = ref (new_stack ()) in
    add_next_state nfa.state_start !states (new_stack ());
    let f (token : char) : unit =
        let next_states : _State _Stack = new_stack () in
        Array.iter
            (
                fun x ->
                    match x with
                        | None -> ()
                        | Some y ->
                            match Hashtbl.find_opt y.transition token with
                                | None -> ()
                                | Some z ->
                                    add_next_state z next_states (new_stack ())
            )
            !states.contents;
        states := next_states in
    String.iter f expression;
    find_true !states

let () : unit =
    (* NOTE: Converted from `(a|b)*c`. *)
    let postfix_expression : string = "ab|*c." in
    let nfa : _StatePair = to_nfa postfix_expression in
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
