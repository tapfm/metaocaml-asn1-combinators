let () = 
  let (<|)  = Asn.OID.(<|) in
  let (<||) = Asn.OID.(<||) in
  let rsa = Asn.OID.base 1 2 <| 841 <| 113549 <|| [1;2;3] in
  Format.print_newline(); Asn.OID.pp Format.std_formatter rsa; Format.print_newline()
(*
let pp_header (tag, coding, hdr_length) = 
  let open Asn_core in
  let open Asn_reader in
  let (tag_name, tag_val) = match tag with
    | Tag.Universal        x -> ("Universal", x)
    | Tag.Application      x -> ("Application", x)
    | Tag.Context_specific x -> ("Context specific", x)
    | Tag.Private          x -> ("Private", x) in 
  let (coding_name, content_length) = match coding with
    | Primitive x            -> ("Primitive", Some x)
    | Constructed x          -> ("Constructed", Some x)
    | Constructed_indefinite -> ("Constructed Indefinite", None) in
  Format.print_string("Tag name: " ^ tag_name ^ "; Tag value: ");
  Format.print_int tag_val;
  Format.print_string("; Coding: " ^ coding_name ^ "; Content length: ");
  (match content_length with 
    | Some l -> Format.print_int(l); Format.print_string " octets"
    | None   -> Format.print_string "Indefinite length");
  Format.print_string("; Header length: ");
  Format.print_int hdr_length;
  Format.print_newline()

let () = 
  let open Asn_core in
  let open Asn_reader in
  let hdr_bytes = Bytes.make(2)(Char.chr(0)) in 
  Bytes.set(hdr_bytes)(0)(Char.chr(0x05));
  let (tag, coding, hdr_length) = Header.parse () hdr_bytes in
  pp_header(tag, coding, hdr_length)

  *)