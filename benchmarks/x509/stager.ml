let () =
  
  Printf.printf "Starting staging using Staged_old stager...\n";
  let codec = Asn.Staged_old.codec Asn.Staged_old.ber Certificate.certificate in 
  Asn.Staged_old.stage_decoder codec "tmp/decoder_staged_old.ml";
  Printf.printf "Finished staging using Staged_old stager\n\n";
  
  
  Printf.printf "Starting staging using Staged0 stager...\n";
  let codec = Asn.Staged0.codec Asn.Staged0.ber Certificate.certificate in 
  Asn.Staged0.stage_decoder codec "tmp/decoder_staged0.ml";
  Printf.printf "Finished staging using Staged0 stager\n\n"
  
  (*
  let open Asn.S in
  let protocol = printable_string  in
  let codec = Asn.Staged0.codec Asn.Staged0.ber protocol in 
  Asn.Staged0.stage_decoder codec "tmp/bool_staged0.ml"
  *)
