(* NOTE: See `https://stackoverflow.com/questions/18024672/what-registers-are-preserved-through-a-linux-x86-64-function-call`. *)

type stack_op =
  | StackOpAdd
  | StackOpNeq

type stack_value =
  | ValueInt of int
  | ValueStr of string

type stack_inst =
  | StackLabel of string

  | StackPush of stack_value

  | StackCall of string * int * int
  | StackRet of int

  | StackJmp of string
  | StackJz of string

  | StackSwap of int
  | StackDup of int
  | StackDrop of int

  | StackBinOp of stack_op

  | StackHalt

let show_str =
  Printf.sprintf "%S"

let show_stack_value =
  function
  | ValueInt int -> string_of_int int
  | ValueStr str -> show_str str

let show_add =
  "add"

let show_neq =
  "neq"

let show_stack_op =
  function
  | StackOpAdd -> show_add
  | StackOpNeq -> show_neq

let print_stack_inst =
  function
  | StackLabel label -> Printf.printf "    %s:\n" label
  | StackPush value -> Printf.printf "        push    %s\n" (show_stack_value value)
  | StackCall (label, args, rets) ->
    Printf.printf "        call    %s, %d, %d\n" label args rets
  | StackRet rets -> Printf.printf "        ret     %d\n" rets
  | StackJmp label -> Printf.printf "        jump    %s\n" label
  | StackJz label -> Printf.printf "        jz      %s\n" label
  | StackSwap offset -> Printf.printf "        swap    %d\n" offset
  | StackDup offset -> Printf.printf "        dup     %d\n" offset
  | StackDrop count -> Printf.printf "        drop    %d\n" count
  | StackBinOp op -> Printf.printf  "        %s\n" (show_stack_op op)
  | StackHalt -> print_string  "        halt\n"

(* --- *)

type reg_op =
  | RegOpMov
  | RegOpAdd
  | RegOpNeq

let show_reg_op =
  function
  | RegOpMov -> "mov"
  | RegOpAdd -> show_add
  | RegOpNeq -> show_neq

type reg_dst =
  | DstReg of int
  | DstArg of int
  | DstRet of int

let show_reg =
  Printf.sprintf "$%d"

let show_arg =
  Printf.sprintf "arg$%d"

let show_ret =
  Printf.sprintf "ret$%d"

let show_reg_dst =
  function
  | DstReg reg -> show_reg reg
  | DstArg arg -> show_arg arg
  | DstRet ret -> show_ret ret

type reg_src =
  | SrcInt of int
  | SrcReg of int
  | SrcStr of string
  | SrcArg of int
  | SrcRet of int

let show_reg_src =
  function
  | SrcInt int -> string_of_int int
  | SrcReg reg -> show_reg reg
  | SrcStr str -> show_str str
  | SrcArg arg -> show_arg arg
  | SrcRet ret -> show_ret ret

type reg_inst =
  | RegLabel of string

  | RegCall of string
  | RegRet

  | RegJmp of string
  | RegJz of string

  | RegBinOp of reg_op * reg_dst * reg_src

  | RegHalt

let print_reg_inst =
  function
  | RegLabel label -> Printf.printf "    %s:\n" label
  | RegCall label -> Printf.printf "        call    %s\n" label
  | RegRet -> print_string "        ret\n"
  | RegJmp label -> Printf.printf "        jmp     %s\n" label
  | RegJz label -> Printf.printf "        jz      %s\n" label
  | RegHalt -> print_string "        halt\n"

  | RegBinOp (op, dst, src) ->
    Printf.printf "        %-8s%s, %s\n" (show_reg_op op) (show_reg_dst dst) (show_reg_src src)

(* --- *)

let allocate_reg stack =
  let rec loop i =
    if Dynarray.exists ((=) i) stack then
      loop (i + 1)
    else
      (
        Dynarray.add_last stack i;
        i
      )
  in
  loop 0

let stack_op_to_reg_op =
  function
  | StackOpAdd -> RegOpAdd
  | StackOpNeq -> RegOpNeq

let stack_value_to_reg_src =
  function
  | ValueInt int -> SrcInt int
  | ValueStr str -> SrcStr str

let reg_mov dst src =
  RegBinOp (RegOpMov, dst, src)

let reg_to_arg stack args offset =
  reg_mov (DstArg ((args - 1) - offset)) (SrcReg (Dynarray.pop_last stack))

let arg_to_reg stack args offset =
  reg_mov (DstReg (allocate_reg stack)) (SrcArg ((args - 1) - offset))

let reg_to_ret stack rets offset =
  reg_mov (DstRet ((rets - 1) - offset)) (SrcReg (Dynarray.pop_last stack))

let ret_to_reg stack rets offset =
  reg_mov (DstReg (allocate_reg stack)) (SrcRet ((rets - 1) - offset))

let stack_func_to_reg_func (label, args, stack_insts) =
  let labels : (string, (int, int list) Either.t) Hashtbl.t = Hashtbl.create 4 in
  for i = 0 to Array.length stack_insts - 1 do
    match stack_insts.(i) with
    | StackLabel label -> Hashtbl.add labels label (Either.left i)
    | _ -> ()
  done;

  let reg_insts = Array.make (Array.length stack_insts) [] in

  let rec loop stack i =
    match stack_insts.(i) with
    | StackLabel label ->
      (
        reg_insts.(i) <- [RegLabel label];
        loop stack (i + 1)
      )
    | StackPush value ->
      (
        reg_insts.(i) <- [
          RegBinOp (RegOpMov, DstReg (allocate_reg stack), stack_value_to_reg_src value);
        ];
        loop stack (i + 1)
      )
    | StackCall (label, args, rets) ->
      (
        let before = List.init args (reg_to_arg stack args) in
        let after = List.init rets (ret_to_reg stack rets) in

        reg_insts.(i) <- before @ (RegCall label :: after);
        loop stack (i + 1)
      )
    | StackSwap offset ->
      (
        assert (offset <> 0);
        let index = (Dynarray.length stack - 1) - offset in
        let a = Dynarray.get stack index in
        let b = Dynarray.pop_last stack in
        Dynarray.add_last stack a;
        Dynarray.set stack index b;
        loop stack (i + 1)
      )
    | StackDup offset ->
      (
        let src = Dynarray.get stack ((Dynarray.length stack - 1) - offset) in
        reg_insts.(i) <- [RegBinOp (RegOpMov, DstReg (allocate_reg stack), SrcReg src)];
        loop stack (i + 1)
      )
    | StackDrop count ->
      (
        assert (count <> 0);
        Dynarray.truncate stack (Dynarray.length stack - count);
        loop stack (i + 1)
      )

    | StackBinOp op ->
      (
        let src = Dynarray.pop_last stack in
        reg_insts.(i) <- [
          RegBinOp (stack_op_to_reg_op op, DstReg (Dynarray.get_last stack), SrcReg src);
        ];
        loop stack (i + 1)
      )

    | StackJmp label ->
      (
        match Hashtbl.find labels label with
        | Either.Left j ->
          (
            Hashtbl.replace labels label (Either.Right (Dynarray.to_list stack));
            reg_insts.(i) <- [RegJmp label];
            loop stack j
          )
        | Either.Right previous_stack -> assert (previous_stack = Dynarray.to_list stack)
      )
    | StackJz label ->
      (
        match Hashtbl.find labels label with
        | Either.Left j ->
          (
            ignore (Dynarray.pop_last stack);
            Hashtbl.replace labels label (Either.Right (Dynarray.to_list stack));
            reg_insts.(i) <- [RegJz label];
            loop (Dynarray.copy stack) j;
            loop stack (i + 1)
          )
        | Either.Right previous_stack ->
          (
            ignore (Dynarray.pop_last stack);
            assert (previous_stack = Dynarray.to_list stack);
            loop stack (i + 1)
          )
      )

    | StackRet rets ->
      (
        let before = List.init rets (reg_to_ret stack rets) in
        assert (Dynarray.length stack = 0);
        reg_insts.(i) <- before @ [RegRet]
      )
    | StackHalt ->
      (
        assert (Dynarray.length stack = 0);
        reg_insts.(i) <- [RegHalt]
      ) in

  let stack : int Dynarray.t = Dynarray.create () in

  let before = List.init args (arg_to_reg stack args) in

  loop stack 0;

  (label, args, RegLabel label :: List.concat (before :: Array.to_list reg_insts))

(* --- *)

let print_func print_insts (label, args, insts) =
  Printf.printf "; %s %d {\n" label args;
  print_insts insts;
  print_string "; }\n"

let () =
  let stack_funcs = [|
    (
      "_start",
      0,
      [|
        StackPush (ValueStr "%lu\n");
        StackPush (ValueInt 1000);
        StackCall ("sum_1_to_n", 1, 1);
        StackCall ("printf", 2, 0);
        StackHalt;
      |]
    );
    (
      "sum_1_to_n",
      1,
      [|
        StackPush (ValueInt 0);
        StackLabel ".while_start";
        StackDup 1;
        StackPush (ValueInt 0);
        StackBinOp StackOpNeq;
        StackJz ".while_end";
        StackDup 1;
        StackBinOp StackOpAdd;
        StackSwap 1;
        StackPush (ValueInt (-1));
        StackBinOp StackOpAdd;
        StackSwap 1;
        StackJmp ".while_start";
        StackLabel ".while_end";
        StackSwap 1;
        StackDrop 1;
        StackRet 1;
      |]
    );
  |] in

  Array.iter (print_func (Array.iter print_stack_inst)) stack_funcs;
  Printf.printf "\n";

  Array.iter
    (fun stack_func -> print_func (List.iter print_reg_inst) (stack_func_to_reg_func stack_func))
    stack_funcs;

  Printf.printf "\n%!";
