let () =
  let codec_req = Asn.Staged.codec Asn.Staged.ber Ldap_certs.ldapMessage in 
  Asn.Staged.stage_decoder codec_req "tmp/ldap_staged.ml";
  let codec_req_old = Asn.Staged_old.codec Asn.Staged_old.ber Ldap_certs.ldapMessage in
  Asn.Staged_old.stage_decoder codec_req_old "tmp/ldap_staged_old.ml";