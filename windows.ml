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
end

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string
end

module Solver0 : Solver = struct
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

      
    let naloga2 data= 
      let lines = List.lines data in
      lines |> List.int_list
      |> funkcija |> zmnozi2
      |> string_of_int

end

(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
(*module Solver1 : Solver = struct
  let naloga1 data = ""

  let naloga2 data _part1 = ""
end*)

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver0)
  | _ -> failwith "Ni še rešeno"

let main () =
  let day = "0" in (*Sys.argv.(1) in*)
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
  let part2 = Solver.naloga2 input_data in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()