let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

let  izracuna list=
    let rec izracuna_aux min max list l d=
      let index1 = (1 + max - min)/2 + min in
      let index2 = (1 + d - l)/2 + l in
      match list with
      | 'F'::rest -> izracuna_aux min (index1-1) rest l d
      | 'B'::rest -> izracuna_aux index1 max rest l d
      | 'L'::rest -> izracuna_aux index1 max rest l (index2-1)
      | 'R'::rest -> izracuna_aux index1 max rest index2 d
      | [] -> (min,l)
      |_ -> failwith "napacen vnos"
      in
    izracuna_aux 0 127 list 0 7 
(* funckija obrne seznam, kopirana iz stack overflow*)
let reverse l =
  let rec rev_acc acc list =
    match list with
    | hd::tl -> rev_acc (hd::acc) tl
    | [] -> acc
  in
  rev_acc [] l


let id (vrsta, sedez)=
  vrsta * 8 + sedez

  (*kopirano z stack overflow. Sortira seznam po velikosti*)
  let rec sort lst =
    match lst with
      [] -> []
    | head :: tail -> insert head (sort tail)
  and insert elt lst =
    match lst with
    |  [] -> [elt]
    | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail;;

let sez prvi zadnji =
  let rec aux acc prvi zadnji  =
    if prvi < zadnji then aux (prvi::acc) (prvi + 1) zadnji
    else (zadnji::acc) in
    aux [] prvi zadnji


    let od_min_do_max list=
      let list_rev =  reverse list in
      match list with
      |prvi::xs->
      let prvi= prvi in
      match list_rev with
      |zadnji::x->
      let zadnji = zadnji in
      sez prvi zadnji

    let rec poisce_manjkajoce prvizadnji list=
        match prvizadnji with
        |x::xs->
        if List.mem x list then poisce_manjkajoce xs list else x

    let naloga1 (data: string)=
      let lines = String.split_on_char '\n' data in
      let sortiran = lines |>List.map explode|> List.map izracuna |> List.map id
      |>sort in
      let sezz = sortiran |> od_min_do_max in
      poisce_manjkajoce sezz sortiran
      




