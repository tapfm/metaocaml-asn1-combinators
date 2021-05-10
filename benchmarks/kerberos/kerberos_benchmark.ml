let bench_req bs name = 
  let codec_unstaged   = Asn.Unstaged.codec Asn.Unstaged.ber Kerberos_certs.as_req in
  let decoder_unstaged = Asn.Unstaged.decode codec_unstaged in 
  let decoder_staged   = Kerberos_as_req_staged.decode in 
  let decoder_old      = Kerberos_as_req_staged_old.decode in 
  let res = Benchmark.throughputN ~repeat:3 1 [
    (name ^ " Unstaged",   decoder_unstaged, bs);
    (*(name ^ " Staged old", decoder_old,      bs);*)
    (name ^ " Staged",     decoder_staged,   bs)
  ] in 
  print_newline();
  Benchmark.tabulate res;
  Printf.printf "\nTesting Staged section only\n";
  let (g, b) = Asn_staged_reader.Gen.parse Asn_staged_core.Ber bs in 
  let res_staged = Benchmark.throughputN ~repeat:3 1 [
    ("Unstaged",   Asn_unstaged_reader.c_asn Kerberos_certs_core.as_req Asn_unstaged_reader.Ber, g);
    ("Staged",     Kerberos_as_req_staged.f,     g);
    (*("Staged old", Kerberos_as_req_staged_old.f, g)*)
  ] in 
  print_newline();
  Benchmark.tabulate res_staged

  let bench_rep bs name = 
    let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber Kerberos_certs.as_rep in
    let decoder_unstaged = Asn.Unstaged.decode codec_unstaged in 
    let decoder_staged   = Kerberos_as_rep_staged.decode in 
    let decoder_old      = Kerberos_as_rep_staged_old.decode in 
    let res = Benchmark.throughputN ~repeat:3 1 [
      (name ^ " Unstaged",   decoder_unstaged, bs);
      (*(name ^ " Staged old", decoder_old,      bs);*)
      (name ^ " Staged",     decoder_staged,   bs)
    ] in 
    print_newline();
    Benchmark.tabulate res;
    Printf.printf "\nTesting Staged section only\n";
    let (g, b) = Asn_staged_reader.Gen.parse Asn_staged_core.Ber bs in 
    let res_staged = Benchmark.throughputN ~repeat:3 1 [
      ("Unstaged",   Asn_unstaged_reader.c_asn Kerberos_certs_core.as_rep Asn_unstaged_reader.Ber, g);
      ("Staged",     Kerberos_as_rep_staged.f,     g);
      (*("Staged old", Kerberos_as_rep_staged_old.f, g)*)
    ] in 
    print_newline();
    Benchmark.tabulate res_staged


let () = 
  Printf.printf "Benchmarking AS-REQ protocol for KRB_AS_REQ message\n";
  let data = Asn.random Kerberos_certs.as_req in 
  let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber Kerberos_certs.as_req in 
  let encoder = Asn.Unstaged.encode codec_unstaged in
  let encoded = encoder data in 
  bench_req encoded "Random data";
  Printf.printf "\nFinished AS-REQ\n\n";
  Printf.printf "Benchmarking AS-REP protocol for KRB_AS_REP message\n";
  let data = Asn.random Kerberos_certs.as_rep in 
  let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber Kerberos_certs.as_rep in 
  let encoder = Asn.Unstaged.encode codec_unstaged in
  let encoded = encoder data in 
  bench_rep encoded "Random data";
  Printf.printf "\nFinished AS-REP\n";
  