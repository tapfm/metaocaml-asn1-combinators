open Asn_core

(* Type for whether the encoded value is primitive or constructed *)
type mode = Constructed | Primitive

(* A simple operator to concatenated two bytes values*)
let (<+>) = Bytes.cat

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
        Bytes.make(1)(Char.chr(class_code lor constructed lor tag_num))
      else
        (* TODO: tag_num must extend into further octects *)
        assert false
    in
    let length_bytes = 
      (* Currently doesn't actually support encoding for indefinite length values --> could be solve with a boolean? *)
      if len < 0xF0 then
        Bytes.make(1)(Char.chr(len))
      else
        (* Here the first octet will contain the number of subsequent length octets *)
        (* Then the subsequent octets will _actually_ encode the length *)
        (* TODO: calculate the required size for the length and populate it *)
        assert false
    in
      identifier <+> length_bytes

end