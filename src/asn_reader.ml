open Asn_core

module Prim = Asn_prim

(* name 'inherited' from asn1-combinators --> should change to something more descriptive e.g. Generic_tag *)
module G = Generic

(* Again legacy from asn1-combinators --> could be changed to Rules *)
(* Previous code only was designed for Ber and Der, so a boolean was sufficient,
but I am aiming for more coverage, so a variant type should be appropriate*)
type config = Ber | Der (* | Cer | ... *)


(* Coding and length [where appropriate] *)
type coding = 
  | Primitive   of int
  | Constructed of int
  | Constructed_indefinite

(* The `Header' covers the identifier and length octets *)
(* Still need to work out a good procedure for indefinite length encoding -- currently unimplemented*)
module Header = struct

  let parse cfg bs = 
    let b0 = Bytes.get_uint8 bs 0 in
    let (tag_num, id_len) = match b0 land 0x1F with
      | 0x1F -> (* TODO: High tag number extends into further octets *)
        assert false (* currently unimplemented *)
      | x    -> (x, 1) in
    let tag = match b0 land 0xC0 with
      | 0x00 -> Tag.Universal tag_num
      | 0x40 -> Tag.Application tag_num
      | 0x80 -> Tag.Context_specific tag_num
      | 0xC0 -> Tag.Private tag_num
      | _    -> assert false in
    let l0 = Bytes.get_uint8 bs id_len in 
    let (len, hdr_len) = match l0 with 
      | 0x80 -> (0, id_len + 1)
      | x    -> match x land 0x80 with
        | 0x00 -> (l0, id_len + 1)
        | _    -> assert false (* TODO: longer form if the length of the contents > 127 *) in
    let cd = match b0 land 0x20 with
      (* Primitive *)
      | 0x00 -> ( match l0 with 
        | 0x80 -> failwith "Cannot have Primitive type with indefinite length"
        | _    -> Primitive len )
      (* Constructed *)
      | 0x20 -> ( match l0 with
        | 0x80 -> Constructed_indefinite
        | _    -> Constructed len )
      (* Never occurs, but makes pattern matching exhaustive *)
      | _  -> assert false in
      (tag, cd, hdr_len)
    

end