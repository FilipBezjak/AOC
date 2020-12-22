module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'

  let rec len list=
    let rec aux st list=
      match list with
      | a::rest -> aux (st+1) rest
      | []-> st
    in
    aux 0 list
  (* funckija obrne seznam, kopirana iz stack overflow*)
  let rev_list l =
    let rec rev_acc acc list =
      match list with
      | hd::tl -> rev_acc (hd::acc) tl
      | [] -> acc
    in
    rev_acc [] l

  let min l=
    List.fold_left min 9999999999 l

    let find x lst : int =
      let rec aux x list acc=
        match list with
        | [] -> failwith "ne najde"
        | h :: t -> if x = h then acc else aux x t (acc+1)
        in
        aux x lst 0

  let max l=
    List.fold_left max 0 l

  (* vrne vsaki drugi element list*)
  let vsaki_drugi list=
    let rec vsaki_aux acc list = 
      match list with
      | prvi::drugi::rest -> vsaki_aux (prvi::rest) rest
      | head::[] -> head::acc
      |_-> acc
    in
  list |> vsaki_aux [] |> rev_list


end

let rec loop (list1: int list) (list2: int list): int list=
  match list1 with
  |[] -> list2
  |p::ps ->
    print_int p;
    print_endline "  prva";
    match list2 with
    |[]-> list1
    |d::ds ->
      print_int d;
      print_endline "  druga";
    if d < p then 
      loop (ps@[p]@[d]) ds
      else
      loop ps (ds@[d]@[p])
    |_-> list1
  |_-> list2


let rec v_dva (list: string list) =
  let rec aux acc list =
    match list with 
    |x::xs when x <> ""-> aux (x::acc) xs
    |""::xs-> 
      match (List.rev_list acc), xs with
      |ply::xs, ply2::ys -> [xs;ys]
    |_-> failwith "v_dva"
  in
  aux [] list


let naloga1 (data: string)=
  let podatki = String.split_on_char '\n' data in
  let [list1;list2] =
  podatki 
  |> v_dva
  |> List.map (List.map int_of_string)
  in
  loop list1 list2
  |> List.rev_list
  |> List.mapi (fun i x -> (i+1)*x)
  |> List.fold_left (+) 0 
  |> string_of_int