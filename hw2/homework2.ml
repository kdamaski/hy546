  open Ast
 

(*
 * A1. Write a function that computes the free variables of an AST
 * (Hint: look in the slides)
 *)
let rec freevars ast =
  match ast with
  EVar(x) ->  [x]
	| EVal(VFun(x, e)) ->  List.filter (fun y ->  y <> x) (freevars e) (* x is bound var so remove it *)
	| EApp(e1, e2) -> List.append ( freevars e1) ( freevars e2)


(*
 * A2. Write a function that takes an AST (the first argument),
 * replaces all free occurrences of variable x (the second argument)
 * with variable y (the third argument), and returns the renamed AST.
 *
 * Note: you can alpha-rename any bound variables in sub-trees as
 * needed, as long as the meaning of the program is the same
 * (alpha-equivalent).
 *)
(* val rename: Ast.exp -> Ast.var -> Ast.var -> Ast.exp *)

let create_name =
  let i = ref 0 in
  fun str ->
    let tmp = !i in
    i := !i + 1;
    str^(string_of_int tmp)

let rec rename e x y =
	match e with 
    | EVar(var) -> (* if var is the same with x then replace it with y *)
      if var = x then
        EVar(y)
      else  (* else leave it unchanged *)
        EVar(var)
		| EVal VFun (param , body) ->
      if param = x then
        (* param is captured and is the same as x, so do not rename *)
        e
      else if param = y then
        (* param conflicts with the new variable y, so rename param *)
        let new_name = create_name param in
          let renamed_body = rename body param new_name in
          let final_body = rename renamed_body x y in
          EVal (VFun(new_name, final_body))
      else (* descend *)
			  EVal (VFun (param , (rename body x y)))
		| EApp(e1 , e2) -> EApp(rename e1 x y ,rename e2 x y)

(*
 * A3. Write a function that checks if a program is a value or not.
 *)
(* val isvalue: Ast.exp -> bool *)
let isvalue = fun (exp) -> 
  match exp with
    | EVal(_) -> true
    | _ -> false

(*
 * A4. Write a function that replaces every free occurrence of a
 * variable x (the second argument) in an AST e (the first argument)
 * with another AST e' (the third argument).  Use alpha renaming to
 * avoid capturing of free variables.
 *)
(* val subst: Ast.exp -> Ast.var -> Ast.exp -> Ast.exp *)
  let rec subst (expr :Ast.exp) (freex : Ast.var) (exp2 : Ast.exp) =
    match expr with
    | EVar var ->
        if var = freex then (* free variable *)
          exp2
        else
          EVar var
    | EVal (VFun (var, body)) ->
        if var = freex then (* x is captured do nothing *)
          exp2
        else if List.mem var (freevars exp2) then (* if var is free in e2 rename it *)
          let new_var = create_name var in
          let renamed_body = subst (rename body freex new_var) freex exp2 in
          EVal (VFun (new_var, renamed_body))
        else (* no conflicts perform subst in the body *)
          EVal (VFun (var, subst body freex exp2))
    | EApp (e1, e2) ->
      EApp (subst e1 freex exp2, subst e2 freex exp2)
(*
 * B. The lazy small-step interpreter
 *
 * Write the step function for lazy (call by name) small-step
 * operational semantics, as shown in the slides.
 *
 * The step function takes an AST, takes a step, and returns the
 * resulting AST, up to alpha-renaming.  I.e. you can rename bound
 * variables when you need to:
 * (fun x . x) is equivalent to (fun y . y)
 * The function raises an exception Cannot_step(e) for any program e
 * that cannot take a step
 *)
(* val lazy_step : Ast.exp -> Ast.exp *)

exception Cannot_step of Ast.exp
let rec lazy_step exp =
  match exp with
  | EVar(_)-> raise (Cannot_step exp) (* var cannot step *) 
  | EVal(_) -> raise (Cannot_step exp) (* func cannot step *)
  | EApp(e1, e2) ->
    if isvalue e1 then (* if e1 is value *)
      match e1 with
      | EVal (VFun (x, e1bod)) ->
          subst e1bod x e2 (* apply e2 on e1bod (replace x with e2) *)
      | _ -> raise (Cannot_step exp)
    else
      let e1' = lazy_step e1 in
      EApp(e1', e2)

(*
 * C. The big-step interpreter
 *
 * Write the evaluation function for eager (call by value) big-step
 * operational semantics, as shown in the slides.
 *
 * The eval function takes an AST, evaluates it, and returns the
 * resulting AST (which cannot run further, is in normal form), if it
 * terminates.  You don't need to worry about non-terminating programs
 * and stack-overflow in your interpreter.
 *)
(* val eager_eval : Ast.exp -> Ast.exp *)
  let rec eager_eval exp = 
    match exp with
    | EVar(_) -> raise (Cannot_step exp)
    | EVal(_) -> exp                    (* return function as is *)
    | EApp (e1, e2) ->
      let val1 = eager_eval e1 in       (* eagerly evaluate e1 into val1 *)
      (
      match val1 with
      | EVal (VFun (x, e1bod)) ->
        let val2 = eager_eval e2 in     (* eagerly evaluate e2 into val2 *)
        eager_eval (subst e1bod x val2) (* Apply val1 to val2 and evaluate *)
      | _ -> raise (Cannot_step exp)
      )
