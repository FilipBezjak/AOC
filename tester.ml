

let fun1 list=
  String.split_on_char ' '

    let indeksiraj index list=
      (string_of_int index)::list


    let skace podatki=
      let rec aux (index: int) acc podatki stevila=
        print_endline "skace";
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


        let pobere_indekse podatki=
          let size = List.length podatki in
          let rec aux (index: int) acc podatki stevila jmp_nop=
            print_int index;
            print_endline "indeksi";
            if size > index then
            let (i::ukaz::n::_) = List.nth podatki index in
            print_string i;
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
            print_int index;
            print_endline "index nasledn  ";
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
                  print_endline prvi;
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
