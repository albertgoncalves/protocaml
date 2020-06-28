(* NOTE: See
    `https://deniskyashif.com/2019/02/17/implementing-a-regular-expression-engine/`. *)

let exists (stack : 'a Stack.t) (f : 'a -> bool) : bool =
    Stack.fold (fun b x -> b || (f x)) false stack

module State = struct
    type t = {
        mutable is_end : bool;
        mutable token_transition : token_transition option;
        mutable epsilon_next : t option;
        mutable epsilon_next_split : t option;
    }

    and token_transition = {
        token : char;
        state : t;
    }

    type link = {
        first : t;
        last : t;
    }

    let create () : t = {
        is_end = false;
        token_transition = None;
        epsilon_next = None;
        epsilon_next_split = None;
    }

    let maybe_any (nfa : link) : link =
        let l : link = {
            first = create ();
            last = create ();
        } in
        l.first.epsilon_next <- Some l.last;
        l.first.epsilon_next_split <- Some nfa.first;
        nfa.last.epsilon_next <- Some l.last;
        nfa.last.epsilon_next_split <- Some nfa.first;
        l

    let maybe_one (nfa : link) : link =
        let l : link = {
            first = create ();
            last = create ();
        } in
        l.first.epsilon_next <- Some l.last;
        l.first.epsilon_next_split <- Some nfa.first;
        nfa.last.epsilon_next <- Some l.last;
        l

    let at_least_one (nfa : link) : link =
        let l : link = {
            first = create ();
            last = create ();
        } in
        l.first.epsilon_next <- Some nfa.first;
        nfa.last.epsilon_next <- Some l.last;
        nfa.last.epsilon_next_split <- Some nfa.first;
        l

    let either (a : link) (b : link) : link =
        let l : link = {
            first = create ();
            last = create ();
        } in
        l.first.epsilon_next <- Some a.first;
        l.first.epsilon_next_split <- Some b.first;
        a.last.epsilon_next <- Some l.last;
        b.last.epsilon_next <- Some l.last;
        l

    let concat (a : link) (b : link) : link =
        a.last.epsilon_next <- Some b.first;
        {
            first = a.first;
            last = b.last;
        }

    let from_token (token : char) : link =
        let l : link = {
            first = create ();
            last = create ();
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
                    first = create ();
                    last = create ();
                } in
                l.last.is_end <- true;
                l.first.epsilon_next <- Some l.last;
                l
            )
        else
            let stack : link Stack.t = Stack.create () in
            for i = 0 to (String.length postfix_expression) - 1 do
                let token : char = postfix_expression.[i] in
                if token = '*' then
                    Stack.push (Stack.pop stack |> maybe_any) stack
                else if token = '?' then
                    Stack.push (Stack.pop stack |> maybe_one) stack
                else if token = '+' then
                    Stack.push (Stack.pop stack |> at_least_one) stack
                else if token = '|' then
                    let b : link = Stack.pop stack in
                    let a : link = Stack.pop stack in
                    Stack.push (either a b) stack
                else if token = '.' then
                    let b : link = Stack.pop stack in
                    let a : link = Stack.pop stack in
                    Stack.push (concat a b) stack
                else
                    Stack.push (from_token token) stack
            done;
            let result : link = Stack.pop stack in
            result.last.is_end <- true;
            result

    let rec add_next_state
            (state : t)
            (next_states : t Stack.t)
            (prev_states : t Stack.t) : unit =
        match (state.epsilon_next, state.epsilon_next_split) with
            | (None, None) -> Stack.push state next_states
            | (Some next_state, None) | (None, Some next_state) ->
                add_if_new next_state next_states prev_states
            | (Some next_a, Some next_b) ->
                (
                    add_if_new next_a next_states prev_states;
                    add_if_new next_b next_states prev_states
                )

    and add_if_new
            (state : t)
            (next_states : t Stack.t)
            (prev_states : t Stack.t) : unit =
        if not (exists prev_states ((=) state)) then
            (
                Stack.push state prev_states;
                add_next_state state next_states prev_states
            )

    let match_transition
            (token : char)
            (next_states : t Stack.t)
            (state : t) : unit =
        match state.token_transition with
            | None -> ()
            | Some transition ->
                if transition.token = token then
                    Stack.create ()
                    |> add_next_state transition.state next_states

    let search (nfa : link) (candidate : string) : bool =
        let states : t Stack.t ref = Stack.create () |> ref in
        Stack.create () |> add_next_state nfa.first !states;
        for i = 0 to (String.length candidate) - 1 do
            let token : char = candidate.[i] in
            let next_states : t Stack.t = Stack.create () in
            Stack.iter (match_transition token next_states) !states;
            states := next_states
        done;
        exists !states (fun state -> state.is_end)
end

let insert_infix (input : string) : string =
    let n : int = String.length input in
    let m : int = n - 1 in
    let output : Buffer.t = Buffer.create ((n * 2) - 1) in
    for i = 0 to n - 1 do
        let token : char = input.[i] in
        Buffer.add_char output token;
        if not ((token = '(') || (token = '|') || (m <= i)) then
            let peek : char = input.[i + 1] in
            if not (Array.mem peek [|'|'; '*'; '+'; '?'; ')'|]) then
                (* NOTE: In this implementation, `.` is a concatenation
                   operator; it is not a wildcard. *)
                Buffer.add_char output '.'
    done;
    Buffer.contents output

let ops : (char, int) Hashtbl.t =
    let ops : (char, int) Hashtbl.t = Hashtbl.create 5 in
    Hashtbl.add ops '|' 0;
    Hashtbl.add ops '.' 1;
    Hashtbl.add ops '*' 2;
    Hashtbl.add ops '+' 3;
    Hashtbl.add ops '?' 4;
    ops

let ops_power : char -> int = Hashtbl.find ops

let to_postfix (input : string) : string =
    let n : int = String.length input in
    let output : Buffer.t = Buffer.create n in
    let stack : char Stack.t = Stack.create () in
    let valid (token : char) : bool =
        if Stack.is_empty stack then
            false
        else
            let peek : char = Stack.top stack in
            (peek <> '(') && ((ops_power token) <= (ops_power peek)) in
    for i = 0 to n - 1 do
        let token : char = input.[i] in
        if Array.mem token [|'.'; '|'; '*'; '+'; '?'|] then
            (
                while valid token do
                    Buffer.add_char output (Stack.pop stack)
                done;
                Stack.push token stack
            )
        else if (token = '(') then
            Stack.push token stack
        else if (token = ')') then
            (
                while (Stack.top stack) <> '(' do
                    Buffer.add_char output (Stack.pop stack)
                done;
                ignore (Stack.pop stack)
            )
        else
            Buffer.add_char output token
    done;
    while Stack.is_empty stack |> not do
        Buffer.add_char output (Stack.pop stack)
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
