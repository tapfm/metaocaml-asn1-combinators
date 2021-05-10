let () =
  Printf.printf "Starting staging using Staged_old stager...\n";
  let codec = Asn.Staged_old.codec Asn.Staged_old.ber Certificate.certificate in 
  Asn.Staged_old.stage_decoder codec "tmp/decoder_staged_old.ml";
  Printf.printf "Finished staging using Staged_old stager\n\n";
  
  
  Printf.printf "Starting staging using Staged stager...\n";
  let codec = Asn.Staged.codec Asn.Staged.ber Certificate.certificate in 
  Asn.Staged.stage_decoder codec "tmp/decoder_staged.ml";
  Printf.printf "Finished staging using Staged stager\n\n";
