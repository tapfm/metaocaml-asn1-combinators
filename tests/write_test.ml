let bytes_to_hex bs = 
  let chr_seq = Bytes.to_seq bs in
  let int_seq = Seq.map (fun c -> Char.code c) chr_seq in
  let str_seq = Seq.map (fun i -> Printf.sprintf "%02x " i) int_seq in
  Seq.fold_left (^) "" str_seq 

let () =
  let toy_asn = Asn.S.bool in 
  let toy_codec = Asn.codec Asn.ber toy_asn in
  let encoded = Asn.encode toy_codec true in
  (* 01 01 FF expected *)
  Format.print_string(bytes_to_hex encoded);
  Format.print_newline()