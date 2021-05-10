let bench_cert bs name = 
  let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber Certificate.certificate in
  let decoder_unstaged = Asn.Unstaged.decode codec_unstaged in 
  let decoder_staged   = Decoder_staged.decode in 
  let decoder_old      = Decoder_staged_old.decode in 
  let res = Benchmark.throughputN ~repeat:3 5 [
    (name ^ " Unstaged",   decoder_unstaged, bs);
    (name ^ " Staged old", decoder_old,      bs);
    (name ^ " Staged",     decoder_staged,   bs)
  ] in 
  print_newline();
  Benchmark.tabulate res;
  Printf.printf "\nTesting Staged section only\n";
  let (g, b) = Asn_staged_reader.Gen.parse Asn_staged_core.Ber bs in 
  let res_staged = Benchmark.throughputN ~repeat:3 5 [
    ("Unstaged",   Asn_unstaged_reader.c_asn Cert2.certificate Asn_unstaged_reader.Ber, g);
    ("Staged",     Decoder_staged.f,     g);
    ("Staged old", Decoder_staged_old.f, g)
  ] in 
  print_newline();
  Benchmark.tabulate res_staged



let () = 
  (*let data0 = (
    let filename = "benchmarks/x509/data/test0.dat" in
    let ic   = open_in filename in
    let data = really_input_string ic (in_channel_length ic) in 
    close_in ic;
    Bytes.of_string data
  ) in 
  let data1 = (
    let filename = "benchmarks/x509/data/test0.dat" in
    let ic   = open_in filename in
    let data = really_input_string ic (in_channel_length ic) in 
    close_in ic;
    Bytes.of_string data
  ) in 
  let data2 = (
    let filename = "benchmarks/x509/data/test0.dat" in
    let ic   = open_in filename in
    let data = really_input_string ic (in_channel_length ic) in 
    close_in ic;
    Bytes.of_string data
  ) in 
  bench_cert data0 "test0.dat";
  bench_cert data1 "test1.dat";
  bench_cert data2 "test2.dat";*)
  let data = Asn.random Certificate.certificate in 
  let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber Certificate.certificate in 
  let encoder = Asn.Unstaged.encode codec_unstaged in
  let encoded = encoder data in 
  bench_cert encoded "Random data"
  