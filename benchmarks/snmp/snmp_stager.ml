let () =
  let codec_req = Asn.Staged.codec Asn.Staged.ber Snmp_certs.snmpV3Message in 
  Asn.Staged.stage_decoder codec_req "tmp/snmp_staged.ml";
  let codec_req_old = Asn.Staged_old.codec Asn.Staged_old.ber Snmp_certs.snmpV3Message in
  Asn.Staged_old.stage_decoder codec_req_old "tmp/snmp_staged_old.ml";