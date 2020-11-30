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

  let long_len cfg bs off = function
    | 0x7F -> failwith "Not allowed - See ITU X.690 8.1.3.5 c"
    | n    -> 
      let rec find_start bs i = function 
      | 0 -> 0L
      | n -> match Bytes.get_uint8 bs i with
        | 0 when cfg = Der -> failwith "Redundant length"
        | 0 -> find_start bs (i + 1) (n - 1)
        | _ when n > 8 -> failwith "Length overflow - size of length > 64 bits"
        | x -> find_len (Int64.of_int x) bs (i + 1) (n - 1)
      and find_len acc bs i = function
      | 0 -> acc
      | n -> let acc = Int64.add (Int64.shift_left acc 8) (Int64.of_int (Bytes.get_uint8 bs i)) in 
             find_len acc bs (i + 1) (n - 1) in 
      Int64.to_int(find_start bs off n)

  let parse cfg bs = 
    let b0 = Bytes.get_uint8 bs 0 in
    let (tag_num, id_len) = match b0 land 0x1F with
      | 0x1F -> (* TODO: High tag number extends into further octets *)
        failwith "Unimplemented"
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
        | 0x00 -> (x, id_len + 1)
        | _    -> let lbody = x land 0x7f in
                  let n     = long_len cfg bs (id_len + 1) lbody in
                  (n, id_len + 1 + lbody) in
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

  let rec children cfg eof acc bs = 
    if eof bs then
      (List.rev acc, bs)
    else
      let (g, bs') = node cfg bs in
      children cfg eof (g::acc) bs'

  and node cfg bs = 
    let (tag, coding, off) = Header.parse cfg bs in
    match coding with
    | Primitive n            -> 
        let (hd, tl) = split_off bs off n in
        (G.Prim (tag, hd), tl)
    | Constructed n          -> 
      let (hd, tl) = split_off bs off n in
      let (gs, _ ) = children cfg eof1 [] hd in 
      (G.Cons(tag, gs), tl)
    | Constructed_indefinite -> match cfg with 
      | Ber -> 
        let (gs, tl) = children cfg eof2 [] (Bytes.sub bs off ((Bytes.length bs) - off)) in 
        (G.Cons (tag, gs), (Bytes.sub bs 2 ((Bytes.length bs) - 2)))
      | _   -> failwith "Constructed indefinite form not in BER decoding"
  
  let parse cfg bs = try node cfg bs with Invalid_argument _ -> failwith "Unexpected EOF"
end

let (@?) x_opt y = match x_opt with Some x -> x | None -> y

let primitive t f = function 
  | G.Prim (t1, bs) when Tag.equal t t1 -> f bs
  | g                                   -> failwith "Type mismatch parsing primitive"

(* 
String_like types are either encoded as:
| Primitive   -> simple to decode
| Constructed -> recursively made of smaller String_like value
So more care is needed to decode it
*)
let string_like (type a) c t (module P : Prim.Prim_s with type t = a) =
  let rec p = function
    | G.Prim (t1, bs) when Tag.equal t t1 -> P.of_bytes bs
    | G.Cons (t1, gs) when Tag.equal t t1 && c = Ber ->
        P.concat (List.map p gs)
    | g -> failwith "Type mismatch parsing string_like" 
  in
    p

let c_prim : type a. config -> tag -> a prim -> G.t -> a = fun cfg tag -> function
  | Bool       -> primitive tag Prim.Boolean.of_bytes
  | Int        -> primitive tag Prim.Integer.of_bytes
  | Bits       -> string_like cfg tag (module Prim.Bits)
  | Octets     -> string_like cfg tag (module Prim.Octets)
  | Null       -> primitive tag Prim.Null.of_bytes
  | OID        -> failwith "Unimplemented"
  | CharString -> string_like cfg tag (module Prim.Gen_string)

let rec c_asn : type a. a asn -> config -> G.t -> a = fun asn cfg ->
  let rec go : type a. ?t:tag -> a asn -> G.t -> a = fun ?t -> function
  | Sequence s       
  | Set s            -> failwith "Unimplemented - Set or Sequence"
  | Choice (a1, a2)  -> failwith "Unimplemented - Choice"
  | Implicit (t0, a) -> go ~t:(t @? t0) a
  | Explicit (t0, a) -> failwith "Unimplemented - Explicit relies on constructed types"
  | Prim p           -> c_prim cfg (match t with | Some x -> x | None -> tag_of_prim p) p in

  go asn

let compile cfg asn = 
  let p = c_asn asn cfg in
  fun bs -> let (g, bs') = Gen.parse cfg bs in (p g, bs')
