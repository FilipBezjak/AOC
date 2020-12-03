let rev_list l =
  let rec rev_acc acc list =
    match list with
    | hd::tl -> rev_acc (hd::acc) tl
    | [] -> acc
  in
  rev_acc [] l

(* vrne vsaki drugi element list*)
let vsaki_drugi list=
  let rec vsaki_aux acc list =
    match list with
    | prvi::drugi::rest -> vsaki_aux (prvi::acc) rest
    | head::[] -> head::acc
    |_-> acc
  in
list |> vsaki_aux [] |> rev_list