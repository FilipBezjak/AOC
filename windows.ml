let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

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
    | Some x -> x*b
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

let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  |_ -> failwith "ni še rešeno"

let main () =
  let day = "2" in (*Sys.argv.(1) in*)
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