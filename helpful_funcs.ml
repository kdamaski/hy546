let rec seq_intlist start finish = if start > finish then [] else start::(seq_intlist (start+1) finish);;

let mylist = seq_intlist (-2) 10;;

let print_list lst =
  List.iter (fun x -> print_int x; print_string " ") lst;
  print_newline ();;


