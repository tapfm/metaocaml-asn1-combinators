
let () =
  let codec = Asn.Staged.codec Asn.Staged.ber Certificate.certificate in 
  Asn.Staged.stage_decoder codec "tmp/decoder.ml"



(*
let () = 
  let int_codec = Asn_combinators.integer in 
  let dec_code  = Asn_staged_reader.c_asn int_codec Asn_staged_core.Ber in 
  let out_chan  = open_out "tmp/decoder.ml" in
  let decode_ft = Format.formatter_of_out_channel out_chan in 
  let code_clsd = Codelib.close_code dec_code in 
  Codelib.format_code decode_ft code_clsd;
  close_out out_chan
*)

(*
let () = 
  let b = Bytes.create 1 in 
  Bytes.set_int8 b 0 19;
  let x = Asn_core.Generic.Prim(Asn_core.Tag.Universal 2, b) in 
  let z = Staged.decode x in
  Z.print z;
  print_newline()
*)