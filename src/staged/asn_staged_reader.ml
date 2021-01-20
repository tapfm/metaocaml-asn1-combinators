open Asn_staged_core
open Asn_core

(* name 'inherited' from asn1-combinators --> should change to something more descriptive e.g. Generic_tag *)
module G = Generic

module Prim = Asn_staged_prim


(* Coding and length [where appropriate] *)
type coding = 
  | Primitive   of int
  | Constructed of int
  | Constructed_indefinite

(* The `Header' covers the identifier and length octets *)
(* Still need to work out a good procedure for indefinite length encoding -- currently unimplemented*)
module Header = struct


  let long_tag bs = 
    let rec go acc = function 
      | 8 -> failwith "Parse error: length of tag in excess of 8 bytes"
      | i ->
              let b = Bytes.get_uint8 bs i in 
              let x = (b land 0x7F) in 
              match (Int64.(add (shift_left acc 7) (of_int x)), b land 0x80) with 
              | (0L,  _) -> failwith "Parse Error: Leading 0 in long tag"
              | (acc, 0) -> (Int64.to_int acc, i + 1)
              | (acc, _) -> go acc (i + 1) in
    go 0L 0

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
      | n -> let acc = Int64.(add (shift_left acc 8) (of_int (Bytes.get_uint8 bs i)) ) in 
             find_len acc bs (i + 1) (n - 1) in 
      Int64.to_int(find_start bs off n)

  let parse cfg bs = 
    let b0 = Bytes.get_uint8 bs 0 in
    let (tag_num, id_len) = match b0 land 0x1F with
      | 0x1F -> 
        let (n, i) = long_tag (Bytes.sub bs 1 ((Bytes.length bs) - 1)) in 
        (n, i + 1)
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
      let (g, bs) = node cfg bs in
      children cfg eof (g::acc) bs

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
        (G.Cons (tag, gs), (Bytes.sub tl 2 ((Bytes.length tl) - 2)))
      | _   -> failwith "Constructed indefinite form not in BER decoding"
  
  let parse cfg bs = try node cfg bs with Invalid_argument _ -> failwith "Unexpected EOF"
end

let (@?) x_opt y = match x_opt with Some x -> x | None -> y

let primitive : type a. tag -> (bytes -> a) code -> (G.t -> a) code = 
  fun t f -> .< function 
    | Generic.Prim (t1, bs) when Tag.equal t t1 -> .~f bs
    | g -> failwith "Type mismatch parsing primitive" 
  >.

let constructed : type a. tag -> (G.t list -> a) code -> (G.t -> a) code = 
  fun t f -> .< function 
    | Generic.Cons (t1, gs) when Tag.equal t t1 -> .~f gs
    | g -> failwith "Type mismatch parsing constructed"
  >.

(* 
String_like types are either encoded as:
| Primitive   -> simple to decode
| Constructed -> recursively made of smaller String_like value
So more care is needed to decode it
*)
let string_like (type a) c t (module P : Prim.Prim_s with type t = a) =
  .<let rec p = function
    | Generic.Prim (t1, bs) when Tag.equal t t1 -> .~(P.of_bytes) bs
    | Generic.Cons (t1, gs) when Tag.equal t t1 && c = Ber ->
        P.concat (List.map p gs)
    | g -> failwith "Type mismatch parsing string_like"
  in
    p>.

let c_prim : type a. config -> tag -> a prim -> (G.t -> a) code = fun cfg tag -> function
  | Bool       -> primitive tag Prim.Boolean.of_bytes
  | Int        -> primitive tag Prim.Integer.of_bytes
  | Bits       -> string_like cfg tag (module Prim.Bits)
  | Octets     -> string_like cfg tag (module Prim.Octets)
  | Null       -> primitive tag Prim.Null.of_bytes
  | OID        -> primitive tag Prim.OID.of_bytes
  | Real       -> primitive tag Prim.Real.of_bytes
  | CharString -> string_like cfg tag (module Prim.Gen_string)
  | Time       -> primitive tag Prim.Time.of_bytes

let peek : 'a asn -> (G.t -> bool) code = fun asn ->
  
    match tag_set asn with 
      | [tag] -> .<fun g -> Tag.equal (G.tag g) tag>.
      | tags  -> .<fun g -> let tag = G.tag g in 
                          List.exists (fun t -> Tag.equal t tag) tags>.
  

let rec c_asn : type a. a asn -> config -> (G.t -> a) code = fun asn cfg ->
  let rec go : type a. ?t:tag -> a asn -> (G.t -> a) code = fun ?t -> function
  | Sequence s       -> constructed (t @? seq_tag) (c_seq s cfg)
  | Sequence_of a    -> constructed (t @? seq_tag) .<(List.map .~(c_asn a cfg))>.
  | Set s            -> constructed (t @? set_tag) (c_set s cfg)
  | Set_of a         -> constructed (t @? set_tag) .<(List.map .~(c_asn a cfg))>.
  | Choice (a1, a2)  -> 
    let accepts = peek a1 in 
    .<fun g -> if .~accepts g then L (.~(c_asn a1 cfg) g) else R (.~(c_asn a2 cfg) g)>.
  | Implicit (t0, a) -> go ~t:(t @? t0) a
  | Explicit (t0, a) -> constructed (t @? t0) (c_explicit a cfg)
  | Prim p           -> c_prim cfg (t @? tag_of_prim p) p in

  go asn

and c_explicit : type a. a asn -> config -> (G.t list -> a) code = fun a cfg ->
  .<function 
  | [g] -> .~(c_asn a cfg) g
  | gs  -> failwith "Parse Error: Explicit tag with a sequence">.

and c_seq : type a. a sequence -> config -> (G.t list -> a) code = fun s cfg->
  let rec seq : type a. a sequence -> (G.t list -> a) code = function
    | Pair (e, s) -> let (p1, p2) = (element e, c_seq s cfg) in 
                     .<fun gs -> let (r, gs') = .~p1 gs in (r, .~p2 gs')>.
    | Last e -> let p = element e in 
                .<fun gs -> match .~p gs with 
                            | (a, []) -> a 
                            | (_, gs) -> failwith "Parse error: Trailing asn in sequence">.
  and element : type a. a element -> (G.t list -> a * G.t list) code = function 
    | Required (lbl, a) ->
      let p = c_asn a cfg in 
      .<function 
        | g::gs -> (.~p g, gs)
        | []    -> failwith ("Parse error: Sequence missing trailing " ^ (label lbl))>.
    | Optional (_, a) ->
      let (p, accepts) = (c_asn a cfg, peek a) in 
      .<function | g::gs when .~accepts g -> (Some (.~p g), gs)
                 | gs                     -> (None,         gs)>.
    in seq s
  

  and c_set : type a. a sequence -> config -> (G.t list -> a) code = fun a b -> failwith "Unimplemented"

let compile cfg asn = 
  let p_code = c_asn asn cfg in
  (*Codelib.print_code Format.std_formatter p_code;*)
  let p      = Runnative.run p_code in
  fun bs -> let (g, bs') = Gen.parse cfg bs in (p g, bs')
