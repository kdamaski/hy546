(********************************************************************
 * CS546 - http://www.csd.uoc.gr/~hy546/
 *
 * This file is part of homework 2
 *)

(* a variable is a string identifier *)
type var = string

(*
 * Values are lambda terms.
 *
 *    VFun("x", e)
 *
 * is the term "lambda x . e", a function with formal argument "x", a
 * variable, and body "e", an expression.
 *)
type value =
  | VFun of var * exp

(*
 * Expressions are either values (functions) or variables, or the
 * application of an expression to another expression.
 *
 * The types for value and expression are mutually recursive (note the
 * "and" keyword in the type definition of exp), because they refer to
 * each other: a value (which is a function) contains the body of the
 * function (which is an expression), and value expressions EVal(v)
 * contain a value v.
 *)
and exp =               (* Expression type.  Expressions can be: *)
  | EVar of var         (* Variables, e.g. EVar("x") *)
  | EVal of value       (* Values, which are functions (see 
                         * the definition of value), e.g.
                         * (lambda x . x) is
                         * EVal(VFun("x", EVar("x")))
                         *)

  | EApp of exp * exp   (* The application of an expression to another
                         * expression.  Here the first expression is
                         * the function applied, and the second is the
                         * argument.  For example (e1 e2) is
                         * EApp(e1, e2).
                         *)
