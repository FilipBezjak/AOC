let rec zadnja_funkcija (str)=
  let rec zadnja_aux acc stringg =
    match stringg with
    |x::xs ->
    let (a::_) = String.split_on_char ':' x in
    if not (a <> "cid") then zadnja_aux (acc+1) xs
    else zadnja_aux acc xs
    |_-> acc
  in
  (zadnja_aux 0 str )= 7