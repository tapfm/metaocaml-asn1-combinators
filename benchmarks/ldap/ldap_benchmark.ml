let bench bs = 
  let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber Ldap_certs.ldapMessage in
  let decoder_unstaged = Asn.Unstaged.decode codec_unstaged in 
  let decoder_staged   = Ldap_staged.decode in 
  let decoder_old      = Ldap_staged_old.decode in 
  let res = Benchmark.throughputN ~repeat:3 1 [
    ("Unstaged",   decoder_unstaged, bs);
    ("Staged old", decoder_old,      bs);
    ("Staged",     decoder_staged,   bs)
  ] in 
  print_newline();
  Benchmark.tabulate res;
  Printf.printf "\nTesting Staged section only\n";
  let (g, b) = Asn_staged_reader.Gen.parse Asn_staged_core.Ber bs in 
  let res_staged = Benchmark.throughputN ~repeat:3 1 [
    ("Unstaged",   Asn_unstaged_reader.c_asn Ldap_certs_core.ldapMessage Asn_unstaged_reader.Ber, g);
    ("Staged",     Ldap_staged.f,     g);
    ("Staged old", Ldap_staged_old.f, g)
  ] in 
  print_newline();
  Benchmark.tabulate res_staged



let () =
  let data = Asn.random Ldap_certs.ldapMessage in 
  let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber Ldap_certs.ldapMessage in 
  let encoder = Asn.Unstaged.encode codec_unstaged in
  let encoded = encoder data in 
  bench encoded
  