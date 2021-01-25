let () =
  let codec  = Asn.Staged.codec Asn.Staged.ber Certificate.certificate in 
  let ic     = open_in "benchmarks/x509/data/test0.dat" in 
  let data   = Bytes.of_string(really_input_string ic (in_channel_length ic)) in 
  let result = Asn.Staged.decode codec data in 
  ()