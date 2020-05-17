(* NOTE: See
    `https://deniskyashif.com/2019/02/17/implementing-a-regular-expression-engine/`. *)

module ArrayStack = struct
    type 'a t = {
        mutable contents : 'a option array;
        mutable capacity : int;
        mutable index : int;
    }

    let make () : 'a t =
        let capacity : int = 2 in
        {
            contents = Array.make capacity None;
            capacity;
            index = 0;
        }

    let grow (xs : 'a t) : unit =
        let capacity = xs.capacity * 2 in
        let contents = Array.make capacity None in
        Array.blit xs.contents 0 contents 0 xs.capacity;
        xs.capacity <- capacity;
        xs.contents <- contents

    let push (x : 'a) (xs : 'a t) : unit =
        xs.contents.(xs.index) <- Some x;
        xs.index <- xs.index + 1;
        if xs.index = xs.capacity then
            grow xs
        else
            ()

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

    let contains (xs : 'a t) (f : 'a -> bool) : bool =
        let n : int = xs.index in
        if n = 0 then
            false
        else
            (
                let result : bool ref = ref false in
                let i : int ref = ref 0 in
                while (not !result) && (!i < n) do
                    if f (Option.get xs.contents.(!i)) then
                        result := true
                    else
                        ();
                    incr i
                done;
                !result
            )
end

module State = struct
    type t = {
        mutable is_end : bool;
        mutable transition : transition option;
        epsilon_transitions : t ArrayStack.t;
    }

    and transition = {
        token : char;
        state : t;
    }

    type link = {
        first : t;
        last : t;
    }

    let make (is_end : bool) : t = {
        is_end;
        transition = None;
        epsilon_transitions = ArrayStack.make ();
    }

    let add_epsilon_transition (state_from : t) (state : t) : unit =
        ArrayStack.push state state_from.epsilon_transitions

    let closure (nfa : link) : link =
        let first : t = make false in
        let last : t = make true in
        add_epsilon_transition first last;
        add_epsilon_transition first nfa.first;
        add_epsilon_transition nfa.last last;
        add_epsilon_transition nfa.last nfa.first;
        nfa.last.is_end <- false;
        {
            first;
            last;
        }

    let union (a : link) (b : link) : link =
        let first : t = make false in
        add_epsilon_transition first a.first;
        add_epsilon_transition first b.first;
        let last : t = make true in
        add_epsilon_transition a.last last;
        a.last.is_end <- false;
        add_epsilon_transition b.last last;
        b.last.is_end <- false;
        {
            first;
            last;
        }

    let concat (a : link) (b : link) : link =
        add_epsilon_transition a.last b.first;
        a.last.is_end <- false;
        {
            first = a.first;
            last = b.last;
        }

    let from_token (token : char) : link =
        let first : t = make false in
        let last : t = make true in
        first.transition <- Some {
            token;
            state = last;
        };
        {
            first;
            last;
        }

    let to_nfa (postfix_expression : string) : link =
        let f (stack : link ArrayStack.t) (token : char) : unit =
            if token = '*' then
                ArrayStack.push (closure (ArrayStack.pop stack)) stack
            else if token = '|' then
                let b : link = ArrayStack.pop stack in
                let a : link = ArrayStack.pop stack in
                ArrayStack.push (union a b) stack
            else if token = '.' then
                let b : link = ArrayStack.pop stack in
                let a : link = ArrayStack.pop stack in
                ArrayStack.push (concat a b) stack
            else
                ArrayStack.push (from_token token) stack in
        if postfix_expression = "" then
            (
                let first : t = make false in
                let last : t = make true in
                add_epsilon_transition first last;
                {
                    first;
                    last;
                }
            )
        else
            let stack : link ArrayStack.t = ArrayStack.make () in
            String.iter (f stack) postfix_expression;
            ArrayStack.pop stack

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
                if ArrayStack.contains prev_states (fun y -> x = y) then
                    ()
                else
                    (
                        ArrayStack.push x prev_states;
                        add_next_state x next_states prev_states
                    )
            done

    let search (nfa : link) (candidate : string) : bool =
        let states : t ArrayStack.t ref = ref (ArrayStack.make ()) in
        add_next_state nfa.first !states (ArrayStack.make ());
        let f (token : char) : unit =
            let next_states : t ArrayStack.t = ArrayStack.make () in
            for i = 0 to !states.ArrayStack.index - 1 do
                let x : t = Option.get !states.ArrayStack.contents.(i) in
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
        ArrayStack.contains !states (fun x -> x.is_end)
end

let () : unit =
    (* NOTE: Converted from `(a|b)*c`. *)
    let postfix_expression : string = "ab|*c." in
    let nfa : State.link = State.to_nfa postfix_expression in
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
