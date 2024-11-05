  let rec seq_list start finish = if start <= finish then start::seq_list (start+1) finish else [];;

  let mylist = seq_list 0 15;;

  let rec print_list list = match list with (hd::tl) -> print_int hd; print_char ' '; print_list(tl) | [] -> print_newline();;

  let split_n l n =
    let rec helper_split (mt, l1) n l =
      match l1 with
      [] -> (mt, l) |
      (hd::tl) -> if hd <= n then helper_split (hd::mt, tl) n tl else helper_split (mt, tl) n l
    in helper_split ([], l) n l;;

  print_list mylist;;
