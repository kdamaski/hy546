open Ast
(*
 * A. Write a function that takes an environment and a variable name,
 * looks up the name in the environment and returns the corresponding
 * type.  Raise exception Type_error if there is no corresponding
 * binding in the given environment.
 *)
exception Type_error
(* val lookup: Ast.env -> Ast.var -> Ast.typ *)
let lookup (envir : env) (v : var) = 
  let mysearchpattern env = match env with
  | (v2,_) ->
    if v = v2 then
      true
    else
      false
  in try match List.find mysearchpattern envir with (v2 ,t)-> t  (* if v2 == v return its type t *)
		with Not_found -> raise (Type_error)
  

(*
 * B. Write a function that takes an environment and an AST and
 * returns a type of the given term.  Raise exception Type_error if
 * there can be no type.
 *)
(* val typeof: Ast.env -> Ast.exp -> Ast.typ *)
let rec typeof (envir : env) (e : exp) = 
  match e with
  | EVar x -> lookup envir x
  | EVal value ->
    (match value with
    | VTrue | VFalse -> TBool
    | VUnit -> TUnit
    | VFun (x, t1, e2) ->
        let env2 = (x, t1)::envir in
          let t2 = typeof env2 e2 in
            TFun (t1, t2)
    )
  | EApp (e1, e2) -> (* apply e1 (func) with arg e2 *)
    (let e1_type = typeof envir e1 in
      match e1_type with
      | TFun (t, t') ->
          if (typeof envir e2) = t then t'
          else raise (Type_error)
      | _ -> raise (Type_error)
    )
  | EIf (e_cond, e_then, e_else) ->
    (let t_if = (typeof envir e_cond) in
      if t_if != TBool
      then raise (Type_error)
      else if (typeof envir e_then) != (typeof envir e_else)
        then raise (Type_error)
      else
        t_if
    )
  | ESeq (e1, e2) ->
      let t1 = typeof envir e1 in
        let t2 = typeof envir e2 in
          if t1 != TUnit then
            raise (Type_error)
          else
            t2
