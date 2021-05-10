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



let teq : tag -> tag code -> bool code = fun t1 t2 -> match t1 with
| Asn_core.Tag.Universal x        -> .< match .~t2 with | Universal y -> x = y | _ -> false >.
| Asn_core.Tag.Application x      -> .< match .~t2 with | Application y -> x = y | _ -> false >.
| Asn_core.Tag.Context_specific x -> .< match .~t2 with | Context_specific y -> x = y | _ -> false >.
| Asn_core.Tag.Private x          -> .< match .~t2 with | Private y -> x = y | _ -> false >.

(*let teq : tag -> tag code -> bool code = fun t1 t2 -> match t1 with
| Asn_core.Tag.Universal x        -> .< .~t2 = Universal x >.
| Asn_core.Tag.Application x      -> .< .~t2 = Application x >.
| Asn_core.Tag.Context_specific x -> .< .~t2 = Context_specific x >.
| Asn_core.Tag.Private x          -> .< .~t2 = Private x >.*)

let tqs : tag list -> tag code -> bool code = fun t1 t2-> 
  let rec expand = function
    | []    -> .< false >.
    | t::ts -> .< .~(teq t t2) || .~(expand ts) >. in
  expand t1

let primitive : type a. tag -> (bytes code -> a code) -> (G.t code -> a code) = 
  fun t f p -> 
    .<
      match .~p with
      | Generic.Prim (t1, bs) when .~(teq t .<t1>.) -> .~(f .<bs>.)
      | g -> failwith "Type mismatch parsing primitive"
    >.

let constructed : type a. tag -> (G.t list code -> a code) -> (G.t code -> a code) = 
  fun t f p->
    .< 
      match .~p with  
      | Generic.Cons (t1, gs) when .~(teq t .<t1>.) -> .~(f .<gs>.)
      | g -> failwith "Type mismatch parsing constructed"
    >.

(* 
String_like types are either encoded as:
| Primitive   -> simple to decode
| Constructed -> recursively made of smaller String_like value
So more care is needed to decode it.

Additionally I was having issues here with the definitions being recursively
spliced, rather than what I wanted which was to splice a recursive defintion.
This is a smaller fix adapted from the version in asn_staged_reader_old.ml, 
I plan to find a more elegant solution for splicing in a recursive function
but for the time being I just want it to work.
*)
let string_like (type a) c t (module P : Prim.Prim_s with type t = a) = 
  let s =
    .<let rec p = function
      | Generic.Prim (t1, bs) when .~(teq t .<t1>.) -> .~(P.of_bytes .<bs>.)
      | Generic.Cons (t1, gs) when .~(teq t .<t1>.) ->
          .~(if c = Ber then
            .<.~(P.concat .<List.map p gs>.)>.
          else
            .<failwith "Can't have constructed string_like type in DER">.)
      | g -> failwith "Type mismatch parsing string_like"
    in
      p>. in 
  fun g -> .< .~s .~g >.


let c_prim : type a. config -> tag -> a prim -> G.t code -> a code = fun cfg tag -> function
  | Bool       -> primitive tag Prim.Boolean.of_bytes
  | Int        -> primitive tag Prim.Integer.of_bytes
  | Bits       -> string_like cfg tag (module Prim.Bits)
  | Octets     -> string_like cfg tag (module Prim.Octets)
  | Null       -> primitive tag Prim.Null.of_bytes
  | OID        -> primitive tag Prim.OID.of_bytes
  | Real       -> primitive tag Prim.Real.of_bytes
  | CharString -> string_like cfg tag (module Prim.Gen_string)
  | Time       -> primitive tag Prim.Time.of_bytes

let peek : 'a asn -> G.t code -> bool code = fun asn ->
    match tag_set asn with 
      | [tag] -> fun g -> teq tag  .<Generic.tag .~g>.
      | tags  -> fun g -> tqs tags .<Generic.tag .~g>.
  

let rec c_asn : type a. a asn -> config -> G.t code -> a code = fun asn cfg ->
  let rec go : type a. ?t:tag -> a asn -> G.t code -> a code = fun ?t -> function
  | Sequence s       -> constructed (t @? seq_tag) (c_seq s cfg)
  | Sequence_of a    -> constructed (t @? seq_tag) (fun gs -> .<List.map (fun g -> .~(c_asn a cfg .<g>.)) .~gs>.)
  | Set s            -> constructed (t @? set_tag) (c_set s cfg)
  | Set_of a         -> constructed (t @? set_tag) (fun gs -> .<List.map (fun g -> .~(c_asn a cfg .<g>.)) .~gs>.)
  | Choice (a1, a2)  -> 
    let accepts = peek a1 in 
    fun g -> .<if .~(accepts g) then L (.~(c_asn a1 cfg g)) else R (.~(c_asn a2 cfg g))>.
  | Implicit (t0, a) -> go ~t:(t @? t0) a
  | Explicit (t0, a) -> constructed (t @? t0) (c_explicit a cfg)
  | Prim p           -> c_prim cfg (t @? tag_of_prim p) p in
  go asn

and c_explicit : type a. a asn -> config -> G.t list code -> a code = fun a cfg gsc ->
  .<
    match .~gsc with 
    | [g] -> .~(c_asn a cfg .<g>.)
    | gs  -> failwith "Parse Error: Explicit tag with a sequence"
  >.

and c_seq : type a. a sequence -> config -> G.t list code -> a code = fun s cfg->
  let rec seq : type a. a sequence -> G.t list code -> a code = function
    | Pair (e, s) -> let (p1, p2) = (element e, c_seq s cfg) in 
                     fun gs -> .<let (r, gs') = .~(p1 gs) in (r, .~(p2 .<gs'>.))>.
    | Last e -> let p = element e in 
                fun gs -> .<match .~(p gs) with 
                            | (a, []) -> a 
                            | (_, gs) -> failwith "Parse error: Trailing asn in sequence">.
  and element : type a. a element -> G.t list code -> (a * G.t list) code = function 
    | Required (lbl, a) -> let l = match lbl with | Some s -> s | None -> "" in 
      let p = c_asn a cfg in 
      fun gsc -> .<match .~gsc with 
        | g::gs -> (.~(p .<g>.), gs)
        | []    -> .~(let msg = "Parse error: Sequence missing trailing " ^ l in .<failwith msg>.)>.
    | Optional (_, a) ->
      let (p, accepts) = (c_asn a cfg, peek a) in 
      fun gsc ->  .<
                    match .~gsc with
                    | g::gs when .~(accepts .<g>.) -> (Some .~(p .<g>.), gs)
                    | gs                           -> (None,             gs)
                  >.
    in seq s
  

  and c_set : type a. a sequence -> config -> G.t list code -> a code = fun a b -> failwith "Unimplemented"

let stage name cfg asn =
  (*Due to metaocaml issues I am having to write the decoder as a special file, which is then decoded later*)
  let decoder_code = .<fun gs -> .~(c_asn asn cfg .<gs>.)>. in
  let out_channel  = open_out name in 
  let formatter    = Format.formatter_of_out_channel out_channel in
  Printf.fprintf out_channel "let f = \n";
  Codelib.(format_code formatter (close_code decoder_code));
  flush out_channel;
  (*For some reason when printing to a file, format_code is leaving off the last case, so I am manually adding it*)
  Printf.fprintf out_channel " g_581 -> Stdlib.failwith \"Type mismatch parsing constructed\"";
  Printf.fprintf out_channel "\nlet decode bs =\n  let (g, b) = Asn_staged_reader.Gen.parse Asn_staged_core.Ber bs in\n  Ok(f g, b)\n";
  flush out_channel;
  close_out out_channel
  (*Codelib.(format_code Format.std_formatter (close_code decoder_code));
  Format.printf "\n"*)

(*This doesn't work, due to issues with where MetaOCaml looks for definitions*)
let compile cfg asn = 
  let decoder_code = .<fun gs -> .~(c_asn asn cfg .<gs>.)>. in 
  fun bs -> 
    let (g, bs') = Gen.parse cfg bs in
    let p = Runnative.run decoder_code in 
    (p g, bs')