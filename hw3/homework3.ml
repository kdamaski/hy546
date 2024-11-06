open Ast
(*
 * A. Write a function that takes an environment and a variable name,
 * looks up the name in the environment and returns the corresponding
 * type.  Raise exception Type_error if there is no corresponding
 * binding in the given environment.
 *)
exception Type_error
(* val lookup: Ast.env -> Ast.var -> Ast.typ *)
let lookup env var = 
	let mylookup = function
  | (v , _) ->
      if var=v then
        true
      else
        false    
	in try match List.find mylookup env with (v ,t)-> t  (* if v == var return its type t *)
		with Not_found -> print_string "no such binding in the environment\n";
				print_string var; raise (Type_error)
  

(*
 * B. Write a function that takes an environment and an AST and
 * returns a type of the given term.  Raise exception Type_error if
 * there can be no type.
 *)
(* val typeof: Ast.env -> Ast.exp -> Ast.typ *)
