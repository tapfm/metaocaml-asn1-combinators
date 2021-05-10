let () =
  let codec_req = Asn.Staged.codec Asn.Staged.ber Kerberos_certs.as_req in 
  Asn.Staged.stage_decoder codec_req "tmp/kerberos_as_req_staged.ml";
  let codec_req_old = Asn.Staged_old.codec Asn.Staged_old.ber Kerberos_certs.as_req in
  Asn.Staged_old.stage_decoder codec_req_old "tmp/kerberos_as_req_staged_old.ml";

  let codec_rep = Asn.Staged.codec Asn.Staged.ber Kerberos_certs.as_rep in 
  Asn.Staged.stage_decoder codec_rep "tmp/kerberos_as_rep_staged.ml";
  let codec_rep_old = Asn.Staged_old.codec Asn.Staged_old.ber Kerberos_certs.as_rep in
  Asn.Staged_old.stage_decoder codec_rep_old "tmp/kerberos_as_rep_staged_old.ml"