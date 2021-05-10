let () =
  let open Asn.S in
  let protocol = sequence (
    (required integer) @
    (optional @@ explicit 69 integer) @
    (required integer) @
    (required integer) @
    (required integer) @
    (optional @@ explicit 420 integer) @
    (optional @@ explicit 237 integer) -@
    (required integer)
  ) in
  let codec = Asn.Staged.codec Asn.Staged.ber protocol in 
  Asn.Staged.stage_decoder codec "tmp/intseq_staged.ml";
  let codec_old = Asn.Staged_old.codec Asn.Staged_old.ber protocol in
  Asn.Staged_old.stage_decoder codec_old "tmp/intseq_staged_old.ml"