open Ast;;
open Main;;
open Homework2;;
#load "ast.cmo";;
#load "homework2.cmo";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "main.cmo";;
let exp1 = ast_of_string   "(fun n. fun s. fun z. n s (s z)) (fun s.fun z. s z)";;
let exp2 = ast_of_string "fun x . x";;
