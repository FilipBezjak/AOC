let f str1 str2 =
  str1 ^ " " ^ str2


  let g str1 str2 =
    str1 ^ str2


    let explode str =
      let rec exp a b =
        if a < 0 then b
        else exp (a - 1) (str.[a] :: b)
      in
      exp (String.length str - 1) []

let loci_presledke list=
  let str = List.fold_left f "" list in
  let nov_sez = String.split_on_char ' ' str in
  match nov_sez with
  |_::x-> x
  |[] -> failwith "napaka pri loci_presledke"


 (*funkcija iz stackoverflow, odstrani ponovljene elemente v listu*)
let uniq_cons x xs = if List.mem x xs then xs else x :: xs

let remove_from_right xs = List.fold_right uniq_cons xs []


let passport_v_seznam (list: string list): string list list =
  let rec uredi_aux (koncni: string list list) (delovni: string list) list2 =
    match list2 with
    |""::rest -> 
    let novi = loci_presledke delovni in
    uredi_aux (novi::koncni) [] rest
    |podatek::rest -> uredi_aux koncni (podatek::delovni) rest
    |[]-> (delovni::koncni)
  in
  uredi_aux [] [] list


  let rec vsebuje x rep=
    match rep with
    |y::ys -> if List.mem x y 
    then vsebuje x ys else false
    |[]->true

(*za koliko elementov iz glave seznama je vsebovanih v vsakem seznamu v repu*)

let prestej_vsebovanost_rec glava rep=
    let rec aux glava rep acc=
      match glava with
      |x::xs-> if vsebuje x rep
      then aux xs rep (acc+1)
      else aux xs rep acc
      |[]->acc
      |_->failwith "napaka pri prestej" in
    aux glava rep 0


let prestej_vsebovanost list=
  match list with
  |glava::rep ->
  prestej_vsebovanost_rec glava rep
  |_->0


  
let naloga2 (data: string) prva=
  let lines = String.split_on_char '\n' data in
  lines|> passport_v_seznam 
  |> List.map (List.map explode)
  |> List.map prestej_vsebovanost
  |>List.fold_left (+) 0