let () =
  let open Asn.S in
  let protocol = sequence (single(required bool)) in
  let codec = Asn.Staged.codec Asn.Staged.ber protocol in 
  Asn.Staged.stage_decoder codec "tmp/bool_staged.ml";
  let codec_old = Asn.Staged_old.codec Asn.Staged_old.ber protocol in
  Asn.Staged_old.stage_decoder codec_old "tmp/bool_staged_old.ml"