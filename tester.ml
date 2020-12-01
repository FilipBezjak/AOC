let rec preveri_prvega (a: int) list =
  match list with
  |b::rest -> if a + b = 2020 then Some b else (preveri_prvega a (rest))
  |[] -> None

let rec preveri_vsoto list =
  match list with
  |a::rest -> if preveri_prvega a rest != None then ((preveri_prvega a rest),a) else preveri_vsoto rest
  |[]-> failwith "nedokončano"

let zmnozi list =
  let (a,b) = preveri_vsoto list in
  match a with
  | Some x -> x*b
  |_-> failwith "napaka"