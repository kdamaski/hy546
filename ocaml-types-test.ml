type number =
Zero
| Integer of int
| Real of float
| Complex of float * float
| Mylist of number list;;

let find_type n = match (n: number) with
Zero -> print_string "nada\n"
| Integer i -> print_int i; print_string "\n"
| Real real -> print_string "not too complex\n"
| Complex (real, unreal) -> print_string "not too complex\n"
| _ -> print_string "uninteresting\n";;

let pi : number = (Real 3.14159);;
let mylist = (Mylist [Real 1.0; Complex (2.0,3.3)]);;
find_type Zero;;
find_type (Real 1.0);;
find_type pi;;
find_type mylist;;

type 'a option =
  None
| Some of 'a;;

let set_or_add n = function
None -> Some n
| Some n' -> Some (n + n');;

type 'a list =
Nil
| Cons of 'a * 'a list;;

let rec length = function (* takes a 'a list which is extracted from the matched type inside function *)
  Nil -> 0
| Cons (_, tl) -> 1 + (length tl);;
