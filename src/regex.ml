(* NOTE: See
    `https://deniskyashif.com/2019/02/17/implementing-a-regular-expression-engine/`. *)

module ArrayStack = struct
    type 'a t = {
        mutable contents : 'a option array;
        mutable capacity : int;
        mutable index : int;
    }

    let make () : 'a t =
        let capacity : int = 4 in
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
                    | Some x' -> x'
                    | None ->
                        Printf.sprintf "ArrayStack.pop @ index = %d" xs.index
                        |> failwith
            )
        else
            failwith "ArrayStack.pop @ empty"

    let peek (xs : 'a t) : 'a =
        if xs.index <> 0 then
            match xs.contents.(xs.index - 1) with
                | Some x -> x
                | None ->
                    Printf.sprintf "ArrayStack.peek @ index = %d" xs.index
                    |> failwith
        else
            failwith "ArrayStack.peek @ empty"

    let exists (xs : 'a t) (f : 'a -> bool) : bool =
        let n : int = xs.index in
        if n = 0 then
            false
        else
            (
                let result : bool ref = ref false in
                let i : int ref = ref 0 in
                while (not !result) && (!i < n) do
                    if Option.get xs.contents.(!i) |> f then
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
        mutable token_transition : token_transition option;
        epsilon_transitions : t ArrayStack.t;
    }

    and token_transition = {
        token : char;
        state : t;
    }

    type link = {
        first : t;
        last : t;
    }

    let make () : t = {
        is_end = false;
        token_transition = None;
        epsilon_transitions = ArrayStack.make ();
    }

    let maybe_any (nfa : link) : link =
        let l : link = {
            first = make ();
            last = make ();
        } in
        ArrayStack.push l.last l.first.epsilon_transitions;
        ArrayStack.push nfa.first l.first.epsilon_transitions;
        ArrayStack.push l.last nfa.last.epsilon_transitions;
        ArrayStack.push nfa.first nfa.last.epsilon_transitions;
        l

    let maybe_one (nfa : link) : link =
        let l : link = {
            first = make ();
            last = make ();
        } in
        ArrayStack.push l.last l.first.epsilon_transitions;
        ArrayStack.push nfa.first l.first.epsilon_transitions;
        ArrayStack.push l.last nfa.last.epsilon_transitions;
        l

    let at_least_one (nfa : link) : link =
        let l : link = {
            first = make ();
            last = make ();
        } in
        ArrayStack.push nfa.first l.first.epsilon_transitions;
        ArrayStack.push l.last nfa.last.epsilon_transitions;
        ArrayStack.push nfa.first nfa.last.epsilon_transitions;
        l

    let either (a : link) (b : link) : link =
        let l : link = {
            first = make ();
            last = make ();
        } in
        ArrayStack.push a.first l.first.epsilon_transitions;
        ArrayStack.push b.first l.first.epsilon_transitions;
        ArrayStack.push l.last a.last.epsilon_transitions;
        ArrayStack.push l.last b.last.epsilon_transitions;
        l

    let concat (a : link) (b : link) : link =
        ArrayStack.push b.first a.last.epsilon_transitions;
        {
            first = a.first;
            last = b.last;
        }

    let from_token (token : char) : link =
        let l : link = {
            first = make ();
            last = make ();
        } in
        l.first.token_transition <- Some {
            token;
            state = l.last;
        };
        l

    let to_nfa (postfix_expression : string) : link =
        if postfix_expression = "" then
            (
                let l : link = {
                    first = make ();
                    last = make ();
                } in
                l.last.is_end <- true;
                ArrayStack.push l.last l.first.epsilon_transitions;
                l
            )
        else
            let stack : link ArrayStack.t = ArrayStack.make () in
            for i = 0 to (String.length postfix_expression) - 1 do
                let token : char = postfix_expression.[i] in
                if token = '*' then
                    ArrayStack.push (ArrayStack.pop stack |> maybe_any) stack
                else if token = '?' then
                    ArrayStack.push (ArrayStack.pop stack |> maybe_one) stack
                else if token = '+' then
                    ArrayStack.push
                        (ArrayStack.pop stack |> at_least_one)
                        stack
                else if token = '|' then
                    let b : link = ArrayStack.pop stack in
                    let a : link = ArrayStack.pop stack in
                    ArrayStack.push (either a b) stack
                else if token = '.' then
                    let b : link = ArrayStack.pop stack in
                    let a : link = ArrayStack.pop stack in
                    ArrayStack.push (concat a b) stack
                else
                    ArrayStack.push (from_token token) stack
            done;
            let result : link = ArrayStack.pop stack in
            result.last.is_end <- true;
            result

    let rec add_next_state
            (state : t)
            (next_states : t ArrayStack.t)
            (prev_states : t ArrayStack.t) : unit =
        if state.epsilon_transitions.ArrayStack.index = 0 then
            ArrayStack.push state next_states
        else
            for i = 0 to state.epsilon_transitions.ArrayStack.index - 1 do
                let next_state : t =
                    Option.get
                        state.epsilon_transitions.ArrayStack.contents.(i) in
                if ArrayStack.exists prev_states ((=) next_state) then
                    ()
                else
                    (
                        ArrayStack.push next_state prev_states;
                        add_next_state next_state next_states prev_states
                    )
            done

    let search (nfa : link) (candidate : string) : bool =
        let states : t ArrayStack.t ref = ref (ArrayStack.make ()) in
        add_next_state nfa.first !states (ArrayStack.make ());
        for i = 0 to (String.length candidate) - 1 do
            let token : char = candidate.[i] in
            let next_states : t ArrayStack.t = ArrayStack.make () in
            for i = 0 to !states.ArrayStack.index - 1 do
                let state : t = Option.get !states.ArrayStack.contents.(i) in
                match state.token_transition with
                    | None -> ()
                    | Some transition' ->
                        if transition'.token = token then
                            add_next_state
                                transition'.state
                                next_states
                                (ArrayStack.make ())
                        else
                            ()
            done;
            states := next_states
        done;
        ArrayStack.exists !states (fun state -> state.is_end)
end

module Ops = struct
    let table : (char, int) Hashtbl.t =
        let table : (char, int) Hashtbl.t = Hashtbl.create 5 in
        Hashtbl.add table '|' 0;
        Hashtbl.add table '.' 1;
        Hashtbl.add table '*' 2;
        Hashtbl.add table '+' 3;
        Hashtbl.add table '?' 4;
        table

    let precedence (op : char) : int = Hashtbl.find table op
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
            if Array.mem peek [|'|'; '*'; '+'; '?'; ')'|] then
                ()
            else
                (* NOTE: In this implementation, `.` is a concatenation
                   operator; it is not a wildcard. *)
                Buffer.add_char output '.'
    done;
    Buffer.contents output

let to_postfix (input : string) : string =
    let n : int = String.length input in
    let output : Buffer.t = Buffer.create n in
    let stack : char ArrayStack.t = ArrayStack.make () in
    for i = 0 to n - 1 do
        let token : char = input.[i] in
        if Array.mem token [|'.'; '|'; '*'; '+'; '?'|] then
            let f () : bool =
                if stack.ArrayStack.index = 0 then
                    false
                else
                    let peek : char = ArrayStack.peek stack in
                    (peek <> '(')
                    && ((Ops.precedence token) <= (Ops.precedence peek)) in
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

let test (pattern : string) (expression : string) (expected : bool) : unit =
    let nfa : State.link =
        insert_infix pattern |> to_postfix |> State.to_nfa in
    if State.search nfa expression = expected then
        Printf.printf "."
    else
        Printf.printf
            "\nTest failed @ \027[1m(\"%s\", \"%s\", %b)\027[0m\n"
            pattern
            expression
            expected

let () : unit =
    Array.iter
        (fun (a, b, c) -> test a b c)
        [|
            ("(a|b)*c", "c", true);
            ("(a|b)*c", "ac", true);
            ("(a|b)*c", "bc", true);
            ("(a|b)*c", "abc", true);
            ("(a|b)*c", "bac", true);
            ("(a|b)*c", "aabbc", true);
            ("(a|b)*c", "cc", false);
            ("(a|b)*c", "cab", false);
            ("(a|b)*c", "cabc", false);
            ("(a|b)*c", "cba", false);
            ("(a|b)*c", "cc", false);
            ("(a|b)?c", "c", true);
            ("(a|b)?c", "ac", true);
            ("(a|b)?c", "bc", true);
            ("(a|b)?c", "abc", false);
            ("(a|b)?c", "bac", false);
            ("(a|b)?c", "aabbc", false);
            ("(a|b)?c", "cc", false);
            ("(a|b)?c", "cab", false);
            ("(a|b)?c", "cabc", false);
            ("(a|b)?c", "cba", false);
            ("(a|b)?c", "cc", false);
            ("abc", "abc", true);
            ("abc", "abcd", false);
            ("a", "", false);
            ("a", "a", true);
            ("a", "aaaaaaa", false);
            ("a*", "", true);
            ("a*", "a", true);
            ("a*", "aaaaaaa", true);
            ("aa*", "", false);
            ("aa*", "a", true);
            ("aa*", "aaaaaaa", true);
            ("a?", "a", true);
            ("a?", "aa", false);
            ("a?", "", true);
            ("a?", "b", false);
            ("a?", "ba", false);
            ("a?", "ab", false);
            ("a+", "a", true);
            ("a+", "aa", true);
            ("a+", "", false);
            ("a+", "b", false);
            ("a+", "ba", false);
            ("a+", "ab", false);
            ("(a|b)+", "a", true);
            ("(a|b)+", "b", true);
            ("(a|b)+", "ab", true);
            ("(a|b)+", "ba", true);
            ("(a|b)+", "", false);
            ("(a|b)+(c|d)+", "ac", true);
            ("(a|b)+(c|d)+", "ad", true);
            ("(a|b)+(c|d)+", "bc", true);
            ("(a|b)+(c|d)+", "bd", true);
            ("(a|b)+(c|d)+", "abcd", true);
            ("(a|b)+(c|d)+", "bbdc", true);
            ("(a|b)+(c|d)+", "bcd", true);
            ("(a|b)+(c|d)+", "bdc", true);
            ("(ab+)cde", "abbcde", true);
        |];
    Printf.printf "\n%!"
