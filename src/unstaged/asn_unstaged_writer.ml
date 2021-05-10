open Asn_unstaged_core
open Asn_core

module Prim = Asn_unstaged_prim

(* Type for whether the encoded value is primitive or constructed *)
type mode = Constructed | Primitive

type config = Ber | Der (* | Cer | ... *)

(* A simple operator to combine 2 Writer.t values*)
let (<+>) : writer -> writer -> writer =
  fun (len_1, writer_1) (len_2, writer_2) ->
  let w off bs = 
    (writer_1 off bs; writer_2 (off + len_1) bs) in
  (len_1 + len_2, w)

let empty_writer = (0, fun off bs -> ())

module Seq = struct
  type 'r f = { f : 'a. 'a -> 'a asn -> 'r -> 'r }

  let rec fold_with_value : type a. 'r f -> 'r -> a -> a sequence -> 'r
  = fun f r a -> function
    | Last (Required (_, asn)) -> f.f a asn r
    | Last (Optional (_, asn)) -> 
      (match a with None -> r | Some a' -> f.f a' asn r)
    | Pair (Required (_, asn), asns) -> 
      let (a1, a2) = a in f.f a1 asn (fold_with_value f r a2 asns)
    | Pair (Optional (_, asn), asns) -> 
      match a with 
      | (None,    a2) -> fold_with_value f r a2 asns
      | (Some a1, a2) -> f.f a1 asn (fold_with_value f r a2 asns)
end

(*Header module encodes the identifier and length octets for an encoded value *)
module Header = struct 

  let encode tag mode len = 

    let (class_code, tag_num) = 
      let open Tag in
      match tag with
        | Universal        n -> (0x00, n)
        | Application      n -> (0x40, n)
        | Context_specific n -> (0x80, n)
        | Private          n -> (0xC0, n) 
    in
    let constructed = match mode with 
      | Primitive   -> 0x00
      | Constructed -> 0x20
    in
    let identifier = 
      if tag_num < 0x1f then
        (1, fun off bs -> Bytes.set_uint8 bs off (class_code lor constructed lor tag_num))
      else
        let cons x = function | [] -> [x] | xs -> (x lor 0x80)::xs in 
        let rec loop acc = function | 0 -> acc | n -> loop (cons (n land 0x7F) acc) (n lsr 7) in 
        let tag_bytes = loop [] tag_num in 
        let w off bs = List.iteri (fun i -> Bytes.set_uint8 bs (off + i)) tag_bytes in 
        (1, fun off bs -> Bytes.set_uint8 bs off (class_code lor constructed lor 0x1f)) <+>
        (List.length tag_bytes, w)
    in
    let length_bytes = 
      (* Currently doesn't actually support encoding for indefinite length values --> could be solve with a boolean? *)
      if len < 0x80 then
        (1, fun off bs -> Bytes.set_uint8 bs off len)
      else
        (* Here the first octet will contain the number of subsequent length octets *)
        (* Then the subsequent octets will _actually_ encode the length *)
        let rec f acc k = function
          | 0 -> (acc, k) 
          | n -> f (n land 0xFF :: acc) (k + 1) (n lsr 8)  in
        let (lst, num_bytes) = f [] 0 len in
        (1, fun off bs -> Bytes.set_uint8 bs off (0x80 lor num_bytes)) <+>
        (num_bytes, fun off bs -> (List.iteri (fun i -> Bytes.set_uint8 bs (off + i)) lst) )
    in
      identifier <+> length_bytes

end

let (@?) x_opt y = match x_opt with Some x -> x | None -> y

let e_constructed tag body = 
  let (len, _) = body in 
  Header.encode tag Constructed len <+> body

let e_primitive tag body = 
  let (len, _) = body in
  Header.encode tag Primitive len <+> body


(* The actual encoding function *)
let rec encode : type a. config -> tag option -> a -> a asn -> writer
  = fun conf tag a -> function 
  | Sequence asns       -> e_constructed (tag @? seq_tag) (e_seq conf a asns)
  | Sequence_of asn     -> e_constructed (tag @? seq_tag) @@
                            List.(fold_left (fun w1 w2 -> w1 <+> w2) 
                                            empty_writer
                                            (map (fun e -> encode conf None e asn) a))
  | Set _               -> failwith "Set not implemented"
  | Set_of asn            -> e_constructed (tag @? set_tag) @@
                            List.(fold_left (fun w1 w2 -> w1 <+> w2) 
                                            empty_writer
                                            (map (fun e -> encode conf None e asn) a))
  | Choice (asn1, asn2) -> (match a with
                            | L a' -> encode conf tag a' asn1 
                            | R b' -> encode conf tag b' asn2)

  | Implicit (t, asn) ->
      encode conf (Some(tag @? t)) a asn
  | Explicit (t, asn) ->
      e_constructed (tag @? t) (encode conf None a asn)

  | Prim p -> encode_prim tag a p

and e_seq : type a. config -> a -> a sequence -> writer = fun conf ->
  let f = {Seq.f = fun e asn w -> encode conf None e asn <+> w} in 
  Seq.fold_with_value f empty_writer

and encode_prim : type a. tag option -> a -> a prim -> writer = fun tag a prim -> 
  let e = e_primitive (tag @? tag_of_prim prim) in 
  match prim with 
  | Bool       -> e @@ Prim.Boolean.to_writer a
  | Int        -> e @@ Prim.Integer.to_writer a
  | Bits       -> e @@ Prim.Bits.to_writer a
  | Octets     -> e @@ Prim.Octets.to_writer a
  | Null       -> e @@ Prim.Null.to_writer a
  | OID        -> e @@ Prim.OID.to_writer a
  | Real       -> e @@ Prim.Real.to_writer a
  | CharString -> e @@ Prim.Gen_string.to_writer a
  | Time       -> e @@ Prim.Time.to_writer a

let to_writer cfg asn a = encode cfg None a asn
