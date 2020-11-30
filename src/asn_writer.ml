open Asn_core

module Prim = Asn_prim

(* Type for whether the encoded value is primitive or constructed *)
type mode = Constructed | Primitive

type config = Ber | Der (* | Cer | ... *)

(* A simple operator to combine 2 Writer.t values*)
let (<+>) : writer -> writer -> writer =
  fun (len_1, writer_1) (len_2, writer_2) ->
  let w off bs = 
    (writer_1 off bs; writer_2 (off + len_1) bs) in
  (len_1 + len_2, w)

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
        (* TODO: tag_num must extend into further octects *)
        assert false
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

let e_primitive tag body = 
  let (len, _) = body in
  Header.encode tag Primitive len <+> body


(* The actual encoding function *)
let rec encode : type a. config -> tag option -> a -> a asn -> writer
  = fun conf tag a -> function 
  | Sequence _
  | Set _
  | Choice _ -> (* TODO: constructed types *)
    assert false

  | Implicit (t, asn) ->
      encode conf (Some(tag @? t)) a asn
  | Explicit (t, asn) ->
      failwith "Unimplemented" (*I believe that this relies on constructed types*)

  | Prim p -> encode_prim tag a p

and encode_prim : type a. tag option -> a -> a prim -> writer = fun tag a prim -> 
  let e = e_primitive (tag @? tag_of_prim prim) in 
  match prim with 
  | Bool       -> e @@ Prim.Boolean.to_writer a
  | Int        -> e @@ Prim.Integer.to_writer a
  | Bits       -> e @@ Prim.Bits.to_writer a
  | Octets     -> e @@ Prim.Octets.to_writer a
  | Null       -> e @@ Prim.Null.to_writer a
  (* TODO: Implement remaing primitive types *)
  | OID        -> failwith "Unimplemented"
  | CharString -> e @@ Prim.Gen_string.to_writer a

let to_writer cfg asn a = encode cfg None a asn
