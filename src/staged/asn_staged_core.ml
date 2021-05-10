(* Again legacy from asn1-combinators --> could be changed to Rules *)
(* Previous code only was designed for Ber and Der, so a boolean was sufficient,
but I am aiming for more coverage, so a variant type should be appropriate*)
type config = Ber | Der (* | Cer | ... *)
