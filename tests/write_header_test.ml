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
  let open Asn_writer in
  let hdr_bytes = Header.encode(Tag.Context_specific 30)(Constructed)(0) in
  let (tag, coding, hdr_length) = Asn_reader.Header.parse () hdr_bytes in
  pp_header(tag, coding, hdr_length)
  