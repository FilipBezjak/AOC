#use "topfind"
#require "str"
open Str


let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan


module AVL = struct
    type height = int
    type int = height
    module Elt = Int32
  
    type t = Leaf | Node of t * Elt.t * t * height
  
    exception Impossible
  
    let height = function
      | Leaf -> 0
      | Node (_, _, _, h) -> h
  
    let empty = Leaf
  
    let mknode tl y tr = Node (tl, y, tr, 1 + max (height tl) (height tr))
  
    let rec contains x = function
      | Leaf -> false
      | Node (tl, y, tr, _) ->
          match Elt.compare x y with
          | 0 -> true
          | res when res < 0 (* x < y *) -> contains x tl
          | _ (* x > y *) -> contains x tr
  
    let rotr = function
      | Node (Node (t1, y2, t3, _), y4, t5, _) ->
          mknode t1 y2 (mknode t3 y4 t5)
      | _ -> raise Impossible
  
  
    let rotl = function
      | Node (t1, y2, Node (t3, y4, t5, _), _) ->
          mknode (mknode t1 y2 t3) y4 t5
      | _ -> raise Impossible
  
  
    let rec insert x = function
      | Leaf -> Node (Leaf, x, Leaf, 1)
      | (Node (tl, y, tr, _)) as node ->
          match Elt.compare x y with
          | 0 -> node
          | res when res < 0 (* x < y *) ->
              begin match insert x tl with
              | Leaf -> raise Impossible
              | (Node (tll, yl, tlr, hl)) as tl ->
                  if hl - height tr <= 1 then
                    mknode tl y tr
                  else
                    let tl = if height tll < height tlr
                             then rotl tl
                             else tl
                    in
                    rotr (mknode tl y tr)
              end
          | _ (* x > y *) ->
              begin match insert x tr with
              | Leaf -> raise Impossible
              | (Node (trl, yr, trr, hr)) as tr ->
                  if hr - height tl <= 1 then
                    mknode tl y tr
                  else
                    let tr = if height trl > height trr
                             then rotr tr
                             else tr
                    in
                    rotl (mknode tl y tr)
            end
  
    let rec inorder = function
      | Leaf -> []
      | Node (tl, y, tr, _) -> inorder tl @ y :: inorder tr
  
    let rec check_balanced = function
      | Leaf -> ()
      | Node (tl, _, tr, h) ->
          assert (h = 1 + max (height tl) (height tr));
          check_balanced tl;
          check_balanced tr
  
    let check_search_tree =
      let assert_lt = function
        | Some x, Some y -> assert (Elt.compare x y < 0);
        | _, _ -> ()
      in
      let rec f bl br = function
        | Leaf -> assert_lt (bl, br) (* probably redundant *)
        | Node (tl, y, tr, _) -> assert_lt (bl, Some y);
                                 assert_lt (Some y, br);
                                 f bl (Some y) tl;
                                 f (Some y) br tr
      in
      f None None

    end

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

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end

module Solver0 : Solver = struct
  let cost_fun x = (x / 3) - 2

  let rec full_cost x =
    let c_cost = cost_fun x in
    if c_cost <= 0 then 0 else c_cost + full_cost c_cost

  let naloga1 data =
    let lines = List.lines data in
    lines |> List.int_list
    |> List.fold_left (fun s x -> s + cost_fun x) 0
    |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
    |> string_of_int

end

(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
module Solver1 : Solver = struct
  let rec preveri_prvega (a: int) list : int option =
    match list with
    |b::rest -> if a + b = 2020 then Some b else (preveri_prvega a (rest))
    |[] -> None
  (*dobi neki int in seznam in pogleda, če je vsota int z katerim številom iz seznama enaka 2020. Vrne tip option*)
  
  let rec preveri_vsoto prvo (list: int list)  =
    match list with
    |a::rest -> if preveri_prvega (a + prvo) rest != None then Some ((preveri_prvega (a + prvo) rest),a) else preveri_vsoto prvo rest
    |[]-> None
  (*dobi seznam in uporabi funkcijo preveri prvega, pri čemer (a:int) preteče vsa števila v listu, ...  vzame prvega in preveri, če je vsota 2020 s katerim številom iz preostanka seznama. če ne vzame naslednjega in naredi isto*)

  let zmnozi list =
    let Some (a,b) = preveri_vsoto 0 list in
    match a with
    |Some x -> x*b
    |_-> failwith "napaka"
  
    let naloga1 data =
      let lines = List.lines data in
      lines |> List.int_list
      |> zmnozi |> string_of_int

    
    let rec funkcija list =
      match list with
      | a::rest -> if preveri_vsoto a rest != None then (a,(preveri_vsoto a rest)) else funkcija rest
      | [] -> failwith "Narobe maš"
    
      
      let zmnozi2 (trojica: int*((int option)*int) option) =
        match trojica with
        | (a,(Some (Some b,c))) -> a*b*c
        | _ -> failwith "krneke"

  
    let naloga2 data _part1= 
      let lines = List.lines data in
      lines |> List.int_list
      |> funkcija |> zmnozi2
      |> string_of_int
end

(* Poženemo zadevo *)

module Solver2 : Solver = struct

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
  

  let preveri_geslo2 prva druga crka geslo=
    if (String.get geslo (prva -1)= crka && String.get geslo (druga-1) = crka) then false else
      if (String.get geslo (prva -1)= crka || String.get geslo (druga-1) = crka) then true else false

  let rec preveri_pravilnost [int1;int2;char;geslo]=
    let prva = int_of_string int1 in
    let druga = int_of_string int2 in
    let crka = String.get char 0 in
    preveri_geslo prva druga crka geslo
  
  let rec preveri_pravilnost2 [int1;int2;char;geslo]=
    let prva = int_of_string int1 in
    let druga = int_of_string int2 in
    let crka = String.get char 0 in
    preveri_geslo2 prva druga crka geslo
    
  let rec steje_pravilne list =
    let rec aux acc list =
      match list with
      |geslo::rest -> if preveri_pravilnost geslo
        then aux (acc+1) rest else (aux acc rest)
      | [] -> acc
    in
    aux 0 list
  
    let rec steje_pravilne2 list =
      let rec aux acc list =
        match list with
        |geslo::rest -> if preveri_pravilnost2 geslo
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
  
  let naloga2 input_data part1 =
    let lines = String.split_on_char '\n' input_data in
    lines |> list_to_ListOfValues |>steje_pravilne2|> string_of_int

end

module Solver3 : Solver = struct

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
      | prvi::drugi::rest -> vsaki_aux (prvi::acc) rest
      | head::[] -> head::acc
      |_-> acc
    in
  list |> vsaki_aux [] |> rev_list


  let je_drevo n str=
    if str.[n] = '#'
    then true else false
  
   (* stringe stevilci po clovesko. Zacne z 1 *)
  
  let steje_drevesa desno dol (list: string list) =
    let rec aux desno dol st_dreves korak ostanek list=
      match list with
      |prvi::rep->
      let index = korak*desno + ostanek in
        if index <= 30 then (* če je korak*desno + ostanek manjši ali enak 31 smo še v vrstici. vse vrstice so dolge 31 znakov *)
          if je_drevo (index) prvi then aux desno dol (st_dreves + 1) (korak + 1) ostanek rep (* ce je, rekurzivno pokličemo
           f na preostanku in korak povecamo.*)
          else aux desno dol st_dreves (korak + 1) ostanek  rep
        else 
          let ostanek' = index - 31 in
             if je_drevo ostanek' prvi then aux desno dol (st_dreves + 1) 1 ostanek' rep
          else aux desno dol st_dreves 1 ostanek' rep
        |[]-> st_dreves
       in
    if dol = 1 then aux desno dol 0 0 0 list
    else aux desno dol 0 0 0 (vsaki_drugi list)

        (*ce je korak*desno + ostanek vecje od 31, korak nastavimo na nic ostanek primerno spremenimo  *)
  
  
  
  (*let rec v_stringList_List list=
    let rec aux acc list =
      match list with
      | prvi::rest -> aux ((explode prvi)::acc) rest
      | [] -> acc in
    aux [] list*)
  
  let naloga1 (data: string)=
    let lines = String.split_on_char '\n' data in
    lines |> steje_drevesa 3 1 |> string_of_int

  let naloga2 data part1 =
    let lines = String.split_on_char '\n' data in
    let prva = lines |> steje_drevesa 1 1 in
    let druga = lines |> steje_drevesa 3 1 in
    let tretja = lines |> steje_drevesa 5 1 in
    let cetrta = lines |> steje_drevesa 7 1 in
    let peta = lines |> steje_drevesa 1 2 in
    prva*druga*tretja*cetrta*peta |> string_of_int

end

module Solver4 : Solver = struct

  
  
  let rev_list l =
    let rec rev_acc acc list =
      match list with
      | hd::tl -> rev_acc (hd::acc) tl
      | [] -> acc
    in
    rev_acc [] l
  
  
  (*ce je elementvo 8 je ok, ce pa 7 brez string, tudi ok*)
  let presteje_pomozna string list=
    if List.mem string list then List.length list = 8
    else List.length list = 7
  
  let f str1 str2 =
    str1 ^ " " ^ str2
  
  let loci_presledke list=
    let str = List.fold_left f "" list in
    let nov_sez = String.split_on_char ' ' str in
    match nov_sez with
    |_::x-> x
    |[] -> failwith "napaka pri loci_presledke"
  
    (*funkcija dobi seznam stringov npr "SSAD", "Asd", "!!WDAS", "", "SDCA", "" in vse 
    med dvema "" "" da v en seznam, loci tudi po presledkih*)
    let passport_v_seznam (list: string list): string list list =
        let rec uredi_aux (koncni: string list list) (delovni: string list) list2 =
          match list2 with
          |""::rest -> 
          let novi = loci_presledke delovni in
          uredi_aux (novi::koncni) [] rest
          |podatek::rest -> uredi_aux koncni (podatek::delovni) rest
          |[]-> koncni
        in
        uredi_aux [] [] list
  
  let vrne_brez_stevil string=
    match String.split_on_char ':' string with
    |x::y -> x
    |_-> failwith "napaka"
  
  (*vsako geslo iz "ASdad:123232" spremeni samo v "ASdad"*)
  let uredi_field sez=
    let rec field_aux sez acc=
      match sez with
      |prvi::rest -> field_aux rest ((vrne_brez_stevil prvi)::acc)
      |[]-> acc
      in
    field_aux sez []
  
    (*koda je iz stack overflowa*)
    let explode str =
      let rec exp a b =
        if a < 0 then b
        else exp (a - 1) (str.[a] :: b)
      in
      exp (String.length str - 1) []
  
  
  let ocisti_passport funkcija list=
    let rec ocisti_aux list acc=
      match list with
      |prvi::rest -> ocisti_aux rest ((funkcija prvi)::acc)
      |[]-> acc
      in
    ocisti_aux list []
  
    let presteje_passporte funkcija list1=
      let rec presteje_aux list acc=
        match list with
        |prvi::rest -> if (funkcija prvi) then presteje_aux rest (acc +1) else presteje_aux rest acc
        |[]-> acc
        in
      presteje_aux list1 0
  
  
    let preveri_byr (geslo:string): bool=
      int_of_string geslo <=2002 && int_of_string geslo >= 1920
  
      let preveri_hgt (geslo:string)=
        let cm y = y<=193 && y>=150 in
        let inch y = y<=76 && y>=59 in
        let sez = explode geslo in
        match sez with
        |s::d::e::c::m::[]-> if (c = 'c' && m='m') then ([s;d;e] |> List.map (fun x -> String.make 1 x)
        |> List.fold_left (^) ""
        |> int_of_string|> cm )else false
        |d::e::i::n::[]-> if i = 'i' && n='n' then 
        [d;e] |> List.map (fun x -> String.make 1 x)
        |> List.fold_left (^) ""
        |> int_of_string|> inch else false
        |_->false
  
    let preveri_ecl (geslo:string): bool=
      List.mem geslo ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
  
    let preveri_hcl (geslo:string): bool=
      geslo.[0]='#'
  
    let preveri_eyr (geslo:string): bool=
      int_of_string geslo <=2030 && int_of_string geslo >= 2020
  
    let preveri_pid (geslo:string): bool=
      (geslo |> explode |> List.length) = 9
  
    let preveri_iyr (geslo:string): bool=
      int_of_string geslo <=2020 && int_of_string geslo >= 2010
  
  
    let preveri (str: string): bool=
      (*print_string str;
      print_string "\n";*)
      let sez = String.split_on_char ':' str in
      match sez with
      | "hgt"::geslo::_ -> preveri_hgt geslo
      |"byr"::geslo::_ -> preveri_byr geslo
      |"ecl"::geslo::_ -> preveri_ecl geslo
      |"hcl"::geslo::_ -> preveri_hcl geslo
      |"eyr"::geslo::_ -> preveri_eyr geslo
      |"pid"::geslo::_ -> preveri_pid geslo
      |"iyr"::geslo::_ -> preveri_iyr geslo
      |"cid"::geslo::_ -> true
      |_-> true (*zato se moramo dati filter po prvi 
      funkciji na koncu*)

      let rec zadnja_funkcija (str)=
        let rec zadnja_aux acc stringg =
          match stringg with
          |x::xs ->
          let (a::_) = String.split_on_char ':' x in
          if (a <> "cid") then zadnja_aux (acc+1) xs
          else zadnja_aux acc xs
          |_-> acc
        in
        (zadnja_aux 0 str ) = 7

    let rec je_pravilno (sez: string list): bool=
      match sez with
      |prvi::rest -> if preveri prvi 
        then je_pravilno rest else false
      |[]-> true
  
    let naloga2 input part1 =
    let lines = String.split_on_char '\n' input in
    lines |> passport_v_seznam |>List.filter zadnja_funkcija|> List.filter je_pravilno
    |>List.length|> string_of_int
  
  
  
  let naloga1 (data: string)=
    let lines = String.split_on_char '\n' data in
    lines |> passport_v_seznam |> ocisti_passport uredi_field
    |> presteje_passporte (presteje_pomozna "cid") |> string_of_int
  
  
end
module Solver5 : Solver = struct

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

  let id (vrsta, sedez)=
  vrsta * 8 + sedez

  (* funckija obrne seznam, kopirana iz stack overflow*)
  let reverse l =
  let rec rev_acc acc list =
    match list with
    | hd::tl -> rev_acc (hd::acc) tl
    | [] -> acc
  in
  rev_acc [] l


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

    let naloga2 (data: string) prva =
      let lines = String.split_on_char '\n' data in
      let sortiran = lines |>List.map explode|> List.map izracuna |> List.map id
      |>sort in
      let sezz = sortiran |> od_min_do_max in
      poisce_manjkajoce sezz sortiran |> string_of_int

    let naloga1 (data: string)=
      let lines = String.split_on_char '\n' data in
      lines |>List.map explode|> List.map izracuna |> List.map id
      |>List.fold_left max 0 |> string_of_int


end

module Solver6 : Solver = struct

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
  
    
  let naloga1 (data: string)=
    let lines = String.split_on_char '\n' data in
    lines |> passport_v_seznam |>List.map (List.fold_left g "")
    |> List.map explode |> List.map remove_from_right
    |> List.map List.length |> List.fold_left (+) 0
    |> string_of_int

    let naloga2 a b = "kezze"


    
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
      in
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
    |> string_of_int

end

module Solver7 : Solver = struct

  
  
  let fun1 list=
    String.split_on_char ' '
  
      let contain = Str.split (Str.regexp "bags?")
  
      let f = global_replace (Str.regexp "contain?") ""
      let g = global_replace (Str.regexp "[0-9]") ""
      let h = global_replace (Str.regexp " ") ""
      let i = global_replace (Str.regexp ",") ""
  
      let rec torbe_z_gold (stringi: string list list): string list =
        match stringi with
        |y::ys -> if List.mem "shinygold" y then ((List.nth y 0)::(torbe_z_gold ys))
        else (torbe_z_gold ys)
        |[]->[]


  (*preveri ali seznam sez1 vsebuje kak element sez2 oz, ce imata kak skupen element*)
  let rec vsebuje sez1 sez2=
    match sez1 with
    |y::ys -> if List.mem y sez2 then
      true
      else vsebuje ys sez2
    |[]->false

      let rec presteje torbe podatki : string list=
        match podatki with
        |y::ys -> 
          if vsebuje torbe y then
          [List.nth y 0]@(presteje torbe ys)
          else presteje torbe ys
        |[] -> []
  
      let rec koncna_presteje torbe podatki=
        let sez = presteje torbe podatki in
        if (List.length sez) !=
        (List.length (presteje sez podatki))
        then koncna_presteje sez podatki else
        sez

  let naloga1 (data: string)=
    let lines = String.split_on_char '\n' data in
    let podatki = lines
    |> List.map f
    |> List.map g
    |> List.map h
    |> List.map contain
    |> List.map (List.map i)
    |> List.map (List.filter (fun x-> (x <> ".")))
    |> List.filter (fun (y::_)-> (y <> "shinygold"))
    in
    let z_zlato = podatki |> torbe_z_gold in
    koncna_presteje z_zlato podatki
    |> List.length
    |>string_of_int

    let naloga2 data part1 =
    let lines = String.split_on_char '\n' data in
    let podatki = lines
    |> List.map f
    |> List.map g
    |> List.map h
    |> List.map contain
    |> List.map (List.map i)
    |> List.map (List.filter (fun x-> (x <> ".")))in
    "nic"


end

module Solver8 : Solver = struct

  let indeksiraj index list=
    (string_of_int index)::list


    let skace podatki=
      let rec aux (index: int) acc podatki stevila=
        let (i::ukaz::n::_) = List.nth podatki index in
        if List.mem i stevila then 
          acc
        else if ukaz = "jmp" then 
          aux (index + (int_of_string n)) acc podatki (i::stevila)
        else if ukaz = "acc" then
          aux (index + 1) (acc + (int_of_string n)) podatki (i::stevila)
        else if ukaz = "nop" then
          aux (index + 1) (acc) podatki (i::stevila)
        else failwith "skace napaka"
        in
        aux 0 0 podatki []
      
        (* dobi zacetne podatke. Zacne pri prvem, ce je loop, spremeni prvi nop v jmp oz prvi jmp v nop. 
        Nato spet zacne pri prvem in spremeni drugi nop/jmp *)
        let pobere_indekse podatki=
          let size = List.length podatki in
          let rec aux (index: int) acc podatki stevila jmp_nop=
            if size > index then
            let (i::ukaz::n::_) = List.nth podatki index in
            if List.mem i stevila then 
              jmp_nop
            else if ukaz = "jmp" then 
              aux (index + 1) acc podatki (i::stevila) (i::jmp_nop)
            else if ukaz = "acc" then
              aux (index + 1) (acc + (int_of_string n)) podatki (i::stevila) jmp_nop
            else if ukaz = "nop" then
              aux (index + 1) (acc) podatki (i::stevila) (i::jmp_nop)
            else failwith "skace napaka"
            else jmp_nop
            in
            aux 0 0 podatki [] []
        

        let skace2 podatki=
          let size = List.length podatki in
          let rec aux (index: int) (acc: int) podatki stevila=
            if size = index then string_of_int acc
            else
            (*print_endline "skace2";*)
            let (i::ukaz::n::_) = List.nth podatki index in
            if List.mem i stevila then 
              "loop"
            else if index >= size then
              string_of_int acc
            else if ukaz = "jmp" then 
              aux (index + (int_of_string n)) acc podatki (i::stevila)
            else if ukaz = "acc" then
              aux (index + 1) (acc + (int_of_string n)) podatki (i::stevila)
            else if ukaz = "nop" then
              aux (index + 1) (acc) podatki (i::stevila)
            else failwith "skace napaka"
            in
            aux 0 0 podatki []

        let f' index (pos': int) ((i::act::num::_): string list): string list=
          let pos = string_of_int pos' in
          if pos = index then 
            if act = "jmp" then 
              [i;"nop";num]
            else if act = "nop" then
              [i;"jmp";num] 
            else [i;act;num] 
          else [i;act;num]

        let spremeni_element (index: string) (podatki: string list list): string list list=
          List.mapi (f' index) podatki


              (* dobi zacetne podatke. Zacne pri prvem, ce je loop, spremeni prvi nop v jmp oz prvi jmp v nop. 
              Nato spet zacne pri prvem in spremeni drugi nop/jmp, ce ni loop, vrne acc iz prvega *)
              let menja podatki indeksi=
                let rec aux podatki' indexi=
                  match indexi with
                  |prvi::indexi_rep ->
                  if skace2 podatki' = "loop" then
                    let novi_podatki = spremeni_element prvi podatki in
                    aux novi_podatki indexi_rep
                  else skace2 podatki'
                  |_ -> failwith "menja ansnansa" in
                aux podatki indeksi

    let naloga2 data part1=
      let lines = String.split_on_char '\n' data in
      let podatki = lines 
      |> List.map (String.split_on_char ' ')
      |> List.mapi indeksiraj in
      let indeksi = podatki |> pobere_indekse |> List.rev in
      menja podatki indeksi



  let naloga1 (data: string)=
    let lines = String.split_on_char '\n' data in
    let podatki = lines 
    |> List.map (String.split_on_char ' ')
    |> List.mapi indeksiraj in
    skace podatki |> string_of_int



  
end

module Solver9 : Solver = struct

  let rec preveri_vsoto x xs n=
    match xs with
    |y::ys -> if y+x=n then true
    else preveri_vsoto x ys n
    |_-> false
  
    let rec je_vsota (n: int) (podatki: int list): bool=
      match podatki with
        |x::xs -> if (preveri_vsoto x xs n) = true
          then true else
          je_vsota n xs
        |_ -> false

  (*spremeni acc vzame x, acc. X doda na zacetek, zadnji
  element pa odstrani*)
      let spremeni_acc x acc=
        match List.rev_list acc with
        |y::ys -> [x]@ (List.rev_list ys)
        |_-> failwith "prekratk seznam"



  (**)
    let sesteva (preamble: int) (podatki: int list)=
      let rec aux peamble acc podatki=
        let size = List.length acc in
        match podatki with
        |x::xs ->
        if size = preamble then
          if je_vsota x acc then
            aux preamble (spremeni_acc x acc) xs
          else
            x
        else aux preamble (x::acc) xs
        |_-> failwith "napaka sesteva"
        in
        aux preamble [] podatki



    let naloga1 (data: string)=
      let lines = String.split_on_char '\n' data in
      lines 
      |> List.map int_of_string
      |> sesteva 25 
      |>string_of_int
      
        let sesteva2 part1 podatki=
          let rec aux (part1: int) (podatki': int list) (acc: int list) : int option=
            match podatki' with 
            |y::ys-> 
              if ((List.sum acc) + y) < part1 then
                aux part1 ys (y::acc) (*y doda k acc*)
              else if ((List.sum acc) + y) = part1 then
                Some ((List.min (y::acc)) + (List.max (y::acc)))
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



    (*sestej min in max, ne prvega in zadnjegaaaa!!!!!!*)
    let naloga2 data part1 =
      let lines = String.split_on_char '\n' data in
      let x = lines 
      |> List.map int_of_string
      |> sesteva2_prva (int_of_string part1) in
      string_of_int x



end

module Solver15 : Solver = struct

  
    let rec poteza (zadnji:int) (rest:int list): int list=
      let index = (List.find zadnji rest) + 1 in
      (*let prvi = (List.length rest + 1) - index in*)
      index::zadnji::rest


  (* dobi int sez, ga obrne, matcha na zadnjega in rest. Če je zadnji v rest,
  pokliče funckijo poteza, ki pogleda na katerem mestu je element "zadnji",
  in vrne razliko list.length - mesto *)
  let funkcija  i (list: int list): int= 
    let rev= List.rev_list list in
    let rec aux list=
      match list with
      |zadnji::rest->
      if List.length list = i then 
        zadnji else
        if List.mem zadnji rest then
        aux (poteza zadnji rest) else
        aux (0::zadnji::rest)
      | [] -> failwith "ni seznama"
      in
      aux rev

      let rec poteza' (zadnji:int) (rest:int list): int list=
        let index = (List.find zadnji rest) + 1 in
        (*let prvi = (List.length rest + 1) - index in*)
        index::zadnji::rest
  
  
    (* dobi int sez, ga obrne, matcha na zadnjega in rest. Če je zadnji v rest,
    pokliče funckijo poteza, ki pogleda na katerem mestu je element "zadnji",
    in vrne razliko list.length - mesto *)
    let funkcija'  i (listt: int list): int= 
      let rev= List.rev_list listt in
      let rec aux len list razlicni=
        match list with
        |zadnji::rest->
        if len = i then 
          zadnji else
          if List.mem zadnji razlicni then
          aux (len +1) (poteza' zadnji rest) razlicni else
          aux (len +1) (0::zadnji::rest) ([zadnji] @ razlicni)
        | [] -> failwith "ni seznama"
        in
        aux 6 rev [0;14;1;3;7]
  


  let naloga1 (data: string)=
    let podatki = String.split_on_char ',' data in
    podatki 
    |> List.map int_of_string
    |> funkcija 2020
    |> string_of_int


    let naloga2 data part1 =
      let podatki = String.split_on_char ',' data in
      podatki 
      |> List.map int_of_string
      |> funkcija' 320000
      |> string_of_int
end

let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver4)
  | "1" -> (module Solver4)
  | "2" -> (module Solver4)
  | "3" -> (module Solver4)
  | "4" -> (module Solver4)
  | "5" -> (module Solver5)
  | "6" -> (module Solver6)
  | "7" -> (module Solver7)
  | "8" -> (module Solver8)
  | "9" -> (module Solver9)
  | "15" -> (module Solver15)
  | _ -> failwith "ni se reseno"

let main () =
  let day = "15" in (*Sys.argv.(1) in*)
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()