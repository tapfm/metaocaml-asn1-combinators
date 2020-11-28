open Asn_core

(* May want this later, currently commented to suppress warnings *)
(* module Prim = Asn_prim *)

(* name 'inherited' from asn1-combinators --> should change to something more descriptive e.g. Generic_tag *)
module G = Generic

module Prim = Asn_prim

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

module Gen = struct

  let eof1 bs = Bytes.length bs = 0
  and eof2 bs = Bytes.get_uint16_le bs 0 = 0

  let split_off bs off n =
    let k = off + n in
    Bytes.(sub bs off n, sub bs k (length bs - k))

  let node cfg bs = 
    let (tag, coding, off) = Header.parse cfg bs in
    match coding with
    | Primitive n            -> 
        let (hd, tl) = split_off bs off n in
        (G.Prim (tag, hd), tl)
      (*TODO generate the tags for constructed type*)
    | Constructed n          -> assert false
    | Constructed_indefinite -> assert false
  
  let parse cfg bs = try node cfg bs with Invalid_argument _ -> failwith "Unexpected EOF"
end

let primitive t f = function 
  | G.Prim (t1, bs) when Tag.equal t t1 -> f bs
  | g                                  -> failwith "Type mismatch parsing primitive"

let c_prim : type a. config -> tag -> a prim -> G.t -> a = fun cfg tag -> function
  | Bool       -> primitive tag Prim.Boolean.of_bytes
  | Int        -> primitive tag Prim.Integer.of_bytes
  | Bits
  | Octets
  | Null
  | OID
  | CharString -> assert false

let rec c_asn : type a. a asn -> config -> G.t -> a = fun asn cfg ->
  let rec go : type a. ?t:tag -> a asn -> G.t -> a = fun ?t -> function
  | Prim p -> c_prim cfg (match t with | Some x -> x | None -> tag_of_prim p) p 
  | _      -> assert false in

  go asn

let compile cfg asn = 
  let p = c_asn asn cfg in
  fun bs -> let (g, bs') = Gen.parse cfg bs in (p g, bs')