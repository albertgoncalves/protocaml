(* NOTE: See
    `https://deniskyashif.com/2019/02/17/implementing-a-regular-expression-engine/`. *)

module ArrayStack = struct
    type 'a t = {
        contents : 'a option array;
        mutable index : int;
    }

    let make () : 'a t = {
        contents = Array.make 5 None;
        index = 0;
    }

    let push (x : 'a) (xs : 'a t) : unit =
        xs.contents.(xs.index) <- Some x;
        xs.index <- xs.index + 1

    let pop (xs : 'a t) : 'a =
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

    let pop_opt (xs : 'a t) : 'a option =
        if xs.index <> 0 then
            (
                xs.index <- xs.index - 1;
                let x : 'a option = xs.contents.(xs.index) in
                xs.contents.(xs.index) <- None;
                x
            )
        else
            None
end

module State = struct
    type t = {
        mutable is_end : bool;
        mutable transition : t_transition option;
        epsilon_transitions : t ArrayStack.t;
    }

    and t_transition = {
        token : char;
        state : t;
    }

    type t_pair = {
        start : t;
        end_ : t;
    }

    let make (is_end : bool) : t = {
        is_end;
        transition = None;
        epsilon_transitions = ArrayStack.make ();
    }

    let add_epsilon_transition (state_from : t) (state : t) : unit =
        ArrayStack.push state state_from.epsilon_transitions

    let add_transition (state_from : t) (state : t) (token : char) : unit =
        state_from.transition <- Some {
            token;
            state;
        }

    let from_epsilon () : t_pair =
        let start : t = make false in
        let end_ : t = make true in
        add_epsilon_transition start end_;
        {
            start;
            end_;
        }

    let from_token (token : char) : t_pair =
        let start : t = make false in
        let end_ : t = make true in
        add_transition start end_ token;
        {
            start;
            end_;
        }

    let concat (first : t_pair) (second : t_pair) : t_pair =
        add_epsilon_transition first.end_ second.start;
        first.end_.is_end <- false;
        {
            start = first.start;
            end_ = second.end_;
        }

    let union (first : t_pair) (second : t_pair) : t_pair =
        let start : t = make false in
        add_epsilon_transition start first.start;
        add_epsilon_transition start second.start;
        let end_ : t = make true in
        add_epsilon_transition first.end_ end_;
        first.end_.is_end <- false;
        add_epsilon_transition second.end_ end_;
        second.end_.is_end <- false;
        {
            start;
            end_;
        }

    let closure (nfa : t_pair) : t_pair =
        let start : t = make false in
        let end_ : t = make true in
        add_epsilon_transition start end_;
        add_epsilon_transition start nfa.start;
        add_epsilon_transition nfa.end_ end_;
        add_epsilon_transition nfa.end_ nfa.start;
        nfa.end_.is_end <- false;
        {
            start;
            end_;
        }

    let rec find_state (stack : t ArrayStack.t) (state : t) : bool =
        match ArrayStack.pop_opt stack with
            | None -> false
            | Some x ->
                if x = state then
                    true
                else
                    find_state stack state

    let rec find_true (stack : t ArrayStack.t) : bool =
        match ArrayStack.pop_opt stack with
            | None -> false
            | Some x ->
                if x.is_end then
                    true
                else
                    find_true stack

    let rec add_next_state
            (state : t)
            (next_states : t ArrayStack.t)
            (prev_states : t ArrayStack.t) : unit =
        if state.epsilon_transitions.ArrayStack.index = 0 then
            ArrayStack.push state next_states
        else
            for i = 0 to state.epsilon_transitions.ArrayStack.index - 1 do
                let x : t =
                    Option.get
                        state.epsilon_transitions.ArrayStack.contents.(i) in
                if find_state prev_states x then
                    ()
                else
                    (
                        ArrayStack.push x prev_states;
                        add_next_state x next_states prev_states
                    )
            done

    let to_nfa (postfix_expression : string) : t_pair =
        let f (stack : t_pair ArrayStack.t) (token : char) : unit =
            if token = '*' then
                ArrayStack.push (closure (ArrayStack.pop stack)) stack
            else if token = '|' then
                let right : t_pair = ArrayStack.pop stack in
                let left : t_pair = ArrayStack.pop stack in
                ArrayStack.push (union left right) stack
            else if token = '.' then
                let right : t_pair = ArrayStack.pop stack in
                let left : t_pair = ArrayStack.pop stack in
                ArrayStack.push (concat left right) stack
            else
                ArrayStack.push (from_token token) stack in
        if postfix_expression = "" then
            from_epsilon ()
        else
            let stack : t_pair ArrayStack.t = ArrayStack.make () in
            String.iter (f stack) postfix_expression;
            ArrayStack.pop stack

    let search (nfa : t_pair) (candidate : string) : bool =
        let states : t ArrayStack.t ref = ref (ArrayStack.make ()) in
        add_next_state nfa.start !states (ArrayStack.make ());
        let f (token : char) : unit =
            let next_states : t ArrayStack.t = ArrayStack.make () in
            for i = 0 to !states.ArrayStack.index - 1 do
                let x : t = Option.get  !states.ArrayStack.contents.(i) in
                match x.transition with
                    | None -> ()
                    | Some y ->
                        if y.token = token then
                            add_next_state
                                y.state
                                next_states
                                (ArrayStack.make ())
                        else
                            ()
            done;
            states := next_states in
        String.iter f candidate;
        find_true !states
end

let () : unit =
    (* NOTE: Converted from `(a|b)*c`. *)
    let postfix_expression : string = "ab|*c." in
    let nfa : State.t_pair = State.to_nfa postfix_expression in
    Array.iter
        (fun x -> Printf.printf "%s\t%b\n" x (State.search nfa x))
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
        |];
    flush stdout
