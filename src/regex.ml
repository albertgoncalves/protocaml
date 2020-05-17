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
        let capacity : int = xs.capacity * 2 in
        let contents : 'a option array = Array.make capacity None in
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

    let peek (xs : 'a t) : 'a =
        if xs.index <> 0 then
            match xs.contents.(xs.index - 1) with
                | Some x -> x
                | None -> exit 1
        else
            exit 1

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

    let closure (nfa : link) : link =
        let first : t = make false in
        let last : t = make true in
        ArrayStack.push last first.epsilon_transitions;
        ArrayStack.push nfa.first first.epsilon_transitions;
        ArrayStack.push last nfa.last.epsilon_transitions;
        ArrayStack.push nfa.first nfa.last.epsilon_transitions;
        nfa.last.is_end <- false;
        {
            first;
            last;
        }

    let union (a : link) (b : link) : link =
        let first : t = make false in
        ArrayStack.push a.first first.epsilon_transitions;
        ArrayStack.push b.first first.epsilon_transitions;
        let last : t = make true in
        ArrayStack.push last a.last.epsilon_transitions;
        a.last.is_end <- false;
        ArrayStack.push last b.last.epsilon_transitions;
        b.last.is_end <- false;
        {
            first;
            last;
        }

    let concat (a : link) (b : link) : link =
        ArrayStack.push b.first a.last.epsilon_transitions;
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
                ArrayStack.push last first.epsilon_transitions;
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

let insert_infix (input : string) : string =
    let n : int = String.length input in
    let m : int = n - 1 in
    let output : Buffer.t = Buffer.create ((n * 2) - 1) in
    for i = 0 to n - 1 do
        let token : char = input.[i] in
        Buffer.add_char output token;
        if (token = '(') || (token = '|') || (m <= i) then
            ()
        else
            let peek : char = input.[i + 1] in
            if (peek = '*') || (peek = '|') || (peek = ')') then
                ()
            else
                (* NOTE: In this implementation, `.` is a concatenation
                   operator; it is not a wildcard. *)
                Buffer.add_char output '.'
    done;
    Buffer.contents output

module Ops = struct
    let table : (char, int) Hashtbl.t =
        let table : (char, int) Hashtbl.t = Hashtbl.create 3 in
        Hashtbl.add table '|' 0;
        Hashtbl.add table '.' 1;
        Hashtbl.add table '*' 2;
        table

    let precedence (op : char) : int = Hashtbl.find table op
end

let to_postfix (input : string) : string =
    let n : int = String.length input in
    let output : Buffer.t = Buffer.create n in
    let stack : char ArrayStack.t = ArrayStack.make () in
    for i = 0 to n - 1 do
        let token : char = input.[i] in
        if (token = '.') || (token = '|') || (token = '*') then
            let f () : bool =
                if stack.ArrayStack.index = 0 then
                    false
                else
                    let peek : char = ArrayStack.peek stack in
                    (peek <> '(') && (
                        (Ops.precedence token) <= (Ops.precedence peek)
                    ) in
            while f () do
                Buffer.add_char output (ArrayStack.pop stack)
            done;
            ArrayStack.push token stack
        else if (token = '(') then
            ArrayStack.push token stack
        else if (token = ')') then
            (
                while (ArrayStack.peek stack) <> '(' do
                    Buffer.add_char output (ArrayStack.pop stack)
                done;
                ignore (ArrayStack.pop stack)
            )
        else
            Buffer.add_char output token
    done;
    while stack.ArrayStack.index <> 0 do
        Buffer.add_char output (ArrayStack.pop stack)
    done;
    Buffer.contents output

let () : unit =
    let expression : string = to_postfix (insert_infix "(a|b)*c") in
    let nfa : State.link = State.to_nfa expression in
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
            "cabc";
            "cba";
            "cc";
        |];
    flush stdout
