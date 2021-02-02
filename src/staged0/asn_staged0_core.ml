type writer = int * (int -> bytes -> unit)

(* A simple operator to combine 2 Writer.t values*)
let (<+>) : writer -> writer -> writer =
  fun (len_1, writer_1) (len_2, writer_2) ->
  let w off bs = 
    (writer_1 off bs; writer_2 (off + len_1) bs) in
  (len_1 + len_2, w)

(* Again legacy from asn1-combinators --> could be changed to Rules *)
(* Previous code only was designed for Ber and Der, so a boolean was sufficient,
but I am aiming for more coverage, so a variant type should be appropriate*)
type config = Ber | Der (* | Cer | ... *)
