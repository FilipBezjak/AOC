(* Converts a string to a list of chars *)
let explode str =
  let rec explode_inner cur_index chars = 
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [new_char])
      else chars in
      explode_inner 0 []
      
let ocisti_c (str: string) : string=
  let [crka;_] = String.split_on_char ':' str in
  crka

  let rec len list=
    let rec aux st list=
      match list with
      | a::rest -> aux (st+1) rest
      | []-> st
    in
    aux 0 list

let rec razpakira (niz: string) =
  let rec aux (nizi: string list)=
    match nizi with
    | stri::[] -> 
      let a = String.split_on_char '-' stri in
      aux a
    |int1::rest::[] ->
      let [int2; char; geslo] = String.split_on_char ' ' rest in
      aux [int1; int2; ocisti_c char; geslo]
    |_ -> nizi in
  aux [niz]

let rec presteje_crke geslo crka=
  geslo |> String.split_on_char crka |> len |> (fun x -> x-1)


let preveri_geslo prva druga crka geslo=
  if (presteje_crke geslo crka <= druga) && (prva <=(presteje_crke geslo crka) )then true else false

let rec preveri_pravilnost [int1;int2;char;geslo]=
  let prva = int_of_string int1 in
  let druga = int_of_string int2 in
  let crka = String.get char 0 in
  preveri_geslo prva druga crka geslo


let rec steje_pravilne list =
  let rec aux acc list =
    match list with
    |geslo::rest -> if preveri_pravilnost geslo
      then aux (acc+1) rest else (aux acc rest)
    | [] -> acc
  in
  aux 0 list


let rec list_to_ListOfValues list=
  let rec aux acc list =
    match list with
    | prvi::rest -> aux ((razpakira prvi)::acc) rest
    | [] -> acc in
  aux [] list

let naloga1 (data: string)=
  let lines = String.split_on_char '\n' data in
  lines |> list_to_ListOfValues 
  |> steje_pravilne



let vrednost = "15-16 k: vxwxxhhkkhklqksd"

let ocisti_c (str: string) : string=
  let [crka;_] = String.split_on_char ':' str in
  crka

  let rec len list=
    let rec aux st list=
      match list with
      | a::rest -> aux (st+1) rest
      | []-> st
    in
    aux 0 list

let rec razpakira (niz: string) =
  let rec aux (nizi: string list)=
    match nizi with
    | stri::[] -> 
      let a = String.split_on_char '-' stri in
      aux a
    |int1::rest::[] ->
      let [int2; char; geslo] = String.split_on_char ' ' rest in
      aux [int1; int2; ocisti_c char; geslo]
    |_ -> nizi in
  aux [niz]

let rec presteje_crke geslo crka=
  geslo |> String.split_on_char crka |> len |> (fun x -> x-1)


let preveri_geslo1 prva druga crka geslo=
  if (presteje_crke geslo crka <= druga) && (prva <=(presteje_crke geslo crka) )then true else false

let preveri_geslo2 prva druga crka geslo=
  if (String.get geslo prva = crka && String.get geslo druga = crka) then false else
    if (String.get geslo prva = crka || String.get geslo druga = crka) then true else false

    let rec print_list = function 
    |[] -> ()
    | e::l -> print_string e ; print_string " " ; print_list l

let rec preveri_pravilnost  [int1;int2;char;geslo::rest]=
 (* print_list int1; *)
  let prva = int_of_string int1 in
  let druga = int_of_string int2 in
  let crka = String.get char 0 in
  if  true then preveri_geslo1 prva druga crka geslo
  else preveri_geslo2 prva druga crka geslo

let rec steje_pravilne  list =
  let rec aux acc list =
    match list with
    |geslo::rest -> if preveri_pravilnost geslo
      then aux (acc+1) rest else (aux acc rest)
    | [] -> acc
  in
  aux 0 list


let rec list_to_ListOfValues list=
  let rec aux acc list =
    match list with
    | prvi::rest -> aux ((razpakira prvi)::acc) rest
    | [] -> acc in
  aux [] list

let naloga1 (data: string)=
  let lines = String.split_on_char '\n' data in
  lines |> list_to_ListOfValues
  |> steje_pravilne |> string_of_int

  wwwwrswthgwhkwwrw