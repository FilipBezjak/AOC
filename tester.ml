module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

    let min l=
      List.fold_left min 9999999999 l
  
    let max l=
      List.fold_left max 0 l


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



let sesteva2 part1 podatki=
  let rec aux (part1: int) (podatki': int list) (acc: int list)=
    match podatki' with 
    |y::ys-> 
      if ((List.sum acc) + y) < part1 then
        aux part1 ys (y::acc) (*y doda k acc*)
      else if ((List.sum acc) + y) = part1 then
      Some ((List.min (y::acc)) + (List.max (y::acc)))
        (*Some (y + (List.nth (List.rev_list acc) 0))*)
      else (*ce je vsota zaporednih vecja od part 1*)
        None
      in
    aux part1 podatki []


let rec sesteva2_prva part1 podatki=
match podatki with 
|x::xs -> 
match sesteva2 part1 xs with
  |Some z -> z
  |None  -> sesteva2_prva part1 xs
|_-> failwith "napaka sesteva2_prva"




let naloga2 data part1 =
let lines = String.split_on_char '\n' data in
let x = lines 
|> List.map int_of_string
|> sesteva2_prva (int_of_string part1) in
 x