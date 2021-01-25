let () = 
  let codec = Asn.Unstaged.codec Asn.Unstaged.ber Certificate.certificate in
  let decoder = Asn.Unstaged.decode codec in 
  let dir   = "benchmarks/x509/data/" in
  let files = Array.to_seq(Sys.readdir dir) in 
  let open Seq in
  let benchmarks = 
  map (fun (s,n) -> ("Unstaged " ^ n, decoder, (Bytes.of_string s)) ) @@
  map (fun (filename, n) -> 
    let ic   = open_in filename in
    let data = really_input_string ic (in_channel_length ic) in 
    close_in ic;
    (data,n)
  ) @@
  map (fun filename -> (dir ^ filename, filename)) @@
  filter (fun name -> Filename.check_suffix name ".dat") @@ 
  files in 
  let res = Benchmark.latencyN ~repeat:3 1000000L (List.of_seq benchmarks) in 
  print_newline();
  Benchmark.tabulate res

(*Would like to directly compare them file to file, but it complains since the type signatures are slighty different*)

let () =
  let dir   = "benchmarks/x509/data/" in
  let files = Array.to_seq(Sys.readdir dir) in 
  let open Seq in
  let benchmarks = 
  map (fun (b,n) -> ("Staged " ^ n, Decoder.decode, b)) @@
  map (fun (s,n) -> (Bytes.of_string s, n)) @@
  map (fun (filename,n) -> 
    let ic   = open_in filename in
    let data = really_input_string ic (in_channel_length ic) in 
    close_in ic;
    (data,n)
  ) @@
  map (fun filename -> (dir ^ filename, filename)) @@
  filter (fun name -> Filename.check_suffix name ".dat") @@ 
  files in 
  let res = Benchmark.latencyN ~repeat:3 1000000L (List.of_seq benchmarks) in 
  print_newline();
  Benchmark.tabulate res