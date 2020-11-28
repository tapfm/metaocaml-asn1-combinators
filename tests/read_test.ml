let bytes_to_hex bs = 
  let chr_seq = Bytes.to_seq bs in
  let int_seq = Seq.map (fun c -> Char.code c) chr_seq in
  let str_seq = Seq.map (fun i -> Printf.sprintf "%02x " i) int_seq in
  Seq.fold_left (^) "" str_seq 

let to_bytes xs =
  Bytes.of_seq (Seq.map (fun x -> Char.chr x) (List.to_seq xs))

let () =
  let toy_asn = Asn.S.bool in 
  let toy_codec = Asn.codec Asn.ber toy_asn in
  let to_decode = to_bytes [0x01; 0x01; 0xFF] in 
  Format.print_string("Decoding : " ^ bytes_to_hex(to_decode) ^ "\n");
  let decoded   = Asn.decode toy_codec to_decode in
  match decoded with 
  | Ok (bl, bs) -> Format.print_string("Decoded bl = " ^ Bool.to_string(bl) ^ "; Remaining bs = \"" ^ bytes_to_hex(bs) ^ "\"\n")
  | Error _     -> Format.print_string "Failure\n"

let () = 
 Format.print_string "\nTesting whether decode and encode are inverse:\n";
 let toy_asn = Asn.S.bool in
 let toy_codec = Asn.codec Asn.ber toy_asn in 
 let (encoder,decoder) = (Asn.encode toy_codec, Asn.decode toy_codec) in
 let v = true in
 Format.print_string("Encoding value v = " ^ Bool.to_string(v) ^ "\n");
 let encoded = encoder v in 
 Format.print_string("Encoded bytes = \"" ^ bytes_to_hex(encoded) ^ "\"\n");
 let decoded = decoder encoded in 
 match decoded with 
  | Ok (bl, bs) -> 
    assert (bl = v);
    Format.print_string("Decoded bl = " ^ Bool.to_string(bl) ^ "; Remaining bs = \"" ^ bytes_to_hex(bs) ^ "\"\n")
  | Error _     -> Format.print_string "Failure\n"