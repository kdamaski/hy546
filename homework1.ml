(*
 * A.1: Write an OCaml function that takes a list of integers and a
 * threshold n, and splits the input list into two, one with all numbers
  * less than or equal to n, and the other with all numbers greater than
   * n.  It should return a pair (tuple of two) with the (less, more) lists.
    *)
(* val split_n : int list -> int -> (int list * int list) *)
let split_n list n =
  let rec help_split l1 l2 l n =
    match l with [] -> (l1,l2) |
    (hd::tl) ->
      if hd <= n then
        help_split (hd::l1) l2 tl n
      else
        help_split l1 (hd::l2) tl n
  in help_split [] [] list n;;

(*
 * A.2: Write an Ocaml function that takes two lists of integers and
  * returns a list of the numbers that occur in both lists.
   *)
(* val find_common : int list -> int list -> int list *)

  let find_common l1 l2 =
    let rec recurse_both l1 l2 full1 common =
      match l1,l2 with
      _, [] -> common |
      [], (hd::tl) -> recurse_both full1 tl full1 common |
      (hd1::tl1), (hd2::tl2) ->
        if hd1 = hd2 then recurse_both tl1 l2 full1 (hd1::common)
        else recurse_both tl1 l2 full1 common
    in recurse_both l1 l2 l1 [];;

(*
 * A.3: Write an Ocaml function that takes two lists of integers and
 * returns a list of the numbers that occur in the first, but not the
 * second.
 *)
(* val list_minus: int list -> int list -> int list *)
  let list_minus l1 l2 =
    let rec recurse_both l1 l2 full2 uncommon =
      match l1,l2 with
      [], _ -> uncommon |
      (hd::tl), [] -> recurse_both tl full2 full2 uncommon |
      (hd::tl), [last] -> if last != hd then recurse_both tl full2 full2 (hd::uncommon)
        else recurse_both tl full2 full2 uncommon |
      (hd1::tl1), (hd2::tl2) ->
        if hd1 = hd2 then recurse_both tl1 tl2 full2 uncommon
        else recurse_both l1 tl2 full2 uncommon
    in recurse_both l1 l2 l2 [];;

(*
 * A.4: Write an OCaml function that takes three arguments: another
 * function of two arguments, and two lists of equal size.  You should
 * apply the input function on each i-th element of both lists and
 * return a list of the results.
 *)
(* val find_filter : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list *)
  let rec find_filter func l1 l2 =
    match l1,l2 with
    [],[] -> [] |
    (hd1::tl1), (hd2::tl2) -> (func hd1 hd2)::find_filter func tl1 tl2 |
     _ -> failwith "Lists must be of equal size"

(*
 * A.5: Write an OCaml function that takes three arguments: another
 * function of one argument, a list, and an integer n.  You should
 * apply the input function on every n-th element of the input list
 * and return a list of the results.
 *)
(* val map_nth : ('a -> 'b) -> 'a list -> int -> 'b list *)
  let map_nth func list n =
    let rec apply_nth i = function
    [] -> [] |
    (hd::tl) ->
      if i mod n = 0 then (func hd) :: apply_nth (i+1) tl
      else apply_nth (i+1) tl
    in apply_nth 1 list
    
(************************************************************************
 * B: N-base arithmetic (where N >= 2)
 * Assume that a list of ints contains the digits of a number in N-based
 * representation.  For example, for binary arithmetic it would contain
 * 1s and 0s, with the head of the list being the least significant bit,
 * and the last element being the most significant bit, with 4 being
 * the list [0;0;1].
 *)

(*
 * Write OCaml functions to do the following:
 * 
 * B.1: Given an integer n for the base, and an int list, check whether
 * the list is an n-base representation of a number.
 *)
(* val check_nbase : int -> int list -> bool *)
  let check_nbase n dig_list = 
    if n < 2 then
      false
    else
      let is_valid_digit c =
        let digit = int_of_char c - int_of_char '0' in
        digit >= 0 && digit < n
      in
      let is_valid_number num =
        let num_in_str = string_of_int num in
        String.for_all is_valid_digit num_in_str
      in
      List.for_all is_valid_number dig_list

(*
 * B.2: Given an integer n for the base, and an integer a, return a list
 * of the n-base representation of the integer n.
 *)
(* val nbase_of_int : int -> int -> int list *)
  let nbase_of_int n a  =
  if n < 2 then []
  else if a = 0 then [0]
  else
    let rec parse_list a lout =
      if a = 0 then lout
      else
        let remainder = a mod n in
        let quotient = a / n in
        parse_list quotient (remainder :: lout)
    in
    parse_list a [] 

(*
 * B.3: Given an integer n for the base, and a list with an n-base
 * represenation, return an integer with the represented number.
 *)
(* val int_of_nbase : int -> int list -> int *)
  let int_of_nbase n nlist = 
    if n < 2 then
      failwith "Base is less than 2"
    else if check_nbase n nlist = false then
      failwith "Base and list digits do not match"
    else
      let length = List.length nlist in
        let rec parse_list nlist power acc = 
          match nlist with 
          [] -> acc |
          (hd::tl) -> let current_value = hd * (int_of_float (float_of_int n ** float_of_int power)) in
          parse_list tl (power-1) (acc + current_value)
        in parse_list nlist (length-1) 0 (* since it is tail recursion we start from power 0 *)

(*
 * B.4: Write a function of three arguments: an integer n for the
 * base, and two lists with an n-base represenation.  Subtract the
 * second from the first, and return a list with the n-base
 * representation of the difference.
 *)
(* val subtract_nbase: int -> int list -> int list -> int list *)
  let subtract_nbase n nl1 nl2 = 
    if n < 2 || check_nbase n nl1 = false || check_nbase n nl2 = false then
      []
    else
      let int1 = int_of_nbase n nl1 in
        let int2 = int_of_nbase n nl2 in
          nbase_of_int n (int1-int2)
      

(*
 * B.5: Write a multiply function that takes three arguments:
 * an integer n for the base and two lists of integers representing
 * n-base numbers.  It should multiply the numbers and return an
 * integer list of the n-base representation of the product.
 *)
(* val multiply_nbase: int -> int list -> int list -> int list *)
  let multiply_nbase n nl1 nl2 = 
    if n < 2 || check_nbase n nl1 = false || check_nbase n nl2 = false then
      []
    else
      let int1 = int_of_nbase n nl1 in
        let int2 = int_of_nbase n nl2 in
          nbase_of_int n (int1*int2)


(************************************************************************
 * C: Boolean syntax trees
 * Assume an abstract syntax tree of simple boolean formulas given by
 * the following type.  Remember you have to redefine the type in the
 * implementation file.
 *)

type bool_ast =
    True
  | False
  | And of bool_ast * bool_ast
  | Or of bool_ast * bool_ast
  | Not of bool_ast

(*
 * C.1: Write an OCaml function that takes a syntax tree and evaluates
 * it to true or false.
 *)
(* val eval_ast : bool_ast -> bool *)

(*
 * C.2: Write an OCaml function that takes a syntax tree and checks if
 * it contains any implications (A implies B is equivalent to not A or
 * B).
 *)
(* val has_implication : bool_ast -> bool *)

(*
 * C.3: Write an OCaml function that takes a syntax tree and
 * simplifies all double negations, returning a simpler syntax tree.
 *)
(* val remove_notnot : bool_ast -> bool_ast *)

(*
 * C.4: Write an OCaml function that takes another function and a
 * sytax tree, and returns a list of all the subtrees for which the
 * first argument returns true.
 *)
(* val select_subtrees : (bool_ast -> bool) -> bool_ast -> bool_ast list *)

(*
 * C.2: Write an OCaml function that takes a syntax tree and checks if
 * it contains any identities (an identity on A is the term "A and A").
 *)
(* val has_identity: bool_ast -> bool *)

(*
 * C.3: Write an OCaml function that takes a syntax tree and
 * rewrites all negations of products (Not (A and B)) to sums of
 * negations ((Not A) or (Not B)), returning the resulting tree.
 *)
(* val rewrite_demorgan: bool_ast -> bool_ast *)


