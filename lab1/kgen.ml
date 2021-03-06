(* lab1/kgen.ml *)

open Tree
open Keiko

let optflag = ref false

(* |gen_expr| -- generate code for an expression *)
let rec gen_expr =
  function
      Variable x ->
        SEQ [LINE x.x_line; LDGW x.x_lab]
    | Number x ->
        CONST x
    | Monop (w, e1) ->
        SEQ [gen_expr e1; MONOP w]
    | Binop (w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2; BINOP w]

(* |gen_cond| -- generate code for short-circuit condition *)
let rec gen_cond tlab flab e =
  (* Jump to |tlab| if |e| is true and |flab| if it is false *)
  match e with
      Number x ->
        if x <> 0 then JUMP tlab else JUMP flab
    | Binop ((Eq|Neq|Lt|Gt|Leq|Geq) as w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2;
          JUMPC (w, tlab); JUMP flab]
    | Monop (Not, e1) ->
        gen_cond flab tlab e1
    | Binop (And, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond lab1 flab e1; LABEL lab1; gen_cond tlab flab e2]
    | Binop (Or, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond tlab lab1 e1; LABEL lab1; gen_cond tlab flab e2]
    | _ ->
        SEQ [gen_expr e; JUMPB (true, tlab); JUMP flab]

(* |gen_stmt| -- generate code for a statement *)
let rec gen_stmt exit_stack =
  function
      Skip -> NOP
    | Exit -> SEQ [JUMP (List.hd exit_stack)]
    | Seq stmts -> SEQ (List.map (gen_stmt exit_stack) stmts)
    | Assign (v, e) ->
        SEQ [LINE v.x_line; gen_expr e; STGW v.x_lab]
    | Print e ->
        SEQ [gen_expr e; CONST 0; GLOBAL "Lib.Print"; PCALL 1]
    | Newline ->
        SEQ [CONST 0; GLOBAL "Lib.Newline"; PCALL 0]
    | IfStmt (test, thenpt, elsept) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [gen_cond lab1 lab2 test; 
          LABEL lab1; gen_stmt exit_stack thenpt; JUMP lab3;
          LABEL lab2; gen_stmt exit_stack elsept; LABEL lab3]
    | WhileStmt (test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [LABEL lab1; gen_cond lab2 lab3 test;
          LABEL lab2; gen_stmt exit_stack body; JUMP lab1; LABEL lab3]
    | RepeatStmt (body, test) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [LABEL lab1; gen_stmt exit_stack body; JUMP lab2; LABEL lab2;
             gen_cond lab3 lab1 test; LABEL lab3]
    | LoopStmt (body) ->
        let lab1 = label () and lab2 = label () in
        SEQ [LABEL lab1; gen_stmt (lab2 :: exit_stack) body; JUMP lab1;
             LABEL lab2]
    | CaseStmt (switch, cases, default) ->
        (* the list of labels *)
        let labs = List.map (fun (case) -> label ()) cases and
           def_lab = label () and exit_lab = label() in
        (* combined : [([int], label())] *)
        let combined = List.combine (List.map (fun (x, y) -> x) cases) labs in
        (* c_with_labels : [[(int_1, label_1) ... (int_s label_1)]] ... *)
        let c_with_labels = List.map (fun (xs, lab) -> (List.map (fun i -> (i, lab)) xs)) combined in
        (* c_with_labels : [(int_1, label_1) ... (int_s label_n)] ... *)
        let table = List.concat c_with_labels in
        let l_with_s = List.combine (List.map (fun (x, y) -> y) cases) labs in
        SEQ [gen_expr switch; CASEJUMP (List.length table);
             SEQ (List.map (fun (i, label) -> CASEARM(i, label)) table);
             JUMP def_lab;
             SEQ (List.map (fun (s, l) -> SEQ [LABEL l; gen_stmt exit_stack s; JUMP exit_lab]) l_with_s);
             LABEL def_lab; gen_stmt exit_stack default; LABEL exit_lab]

(* |translate| -- generate code for the whole program *)
let translate (Program ss) =
  let lab = label () in
    let code = SEQ [gen_stmt [lab] ss; LABEL lab] in
      Keiko.output (if !optflag then Peepopt.optimise code else code)
