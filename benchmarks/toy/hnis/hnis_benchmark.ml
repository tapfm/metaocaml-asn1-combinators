let () =
  let protocol = Hnis_stager.protocol in
  let p2 = Hnis_stager.protocol_core in
  let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber protocol in 
  let encoder = Asn.Unstaged.encode codec_unstaged in


  Printf.printf "\nCreating Decoders:\n";
  let decoder_unstaged = Asn.Unstaged.decode codec_unstaged in 
  let decoder_staged   = Hnis_staged.decode in
  let decoder_old      = Hnis_staged_old.decode in
  Printf.printf "Done\n";

  Printf.printf "\nGenerating Data\n";
  let p (((i1, (i2, i3)), i4), (i5, (i6, (i7, i8)))) = 
    let g = Z.to_string in 
    Printf.sprintf "(%s, %s, %s, %s, %s, %s, %s, %s)" (g i1) (g i2) (g i3) (g i4) (g i5) (g i6) (g i7) (g i8) in 



  Random.self_init();
  let data = [
    Asn.random protocol;
    Asn.random ~full:true protocol;
    Asn.random protocol;
  ] in 

  let test data = 
    (match decoder_unstaged data with 
    | Ok (r, bs) -> Printf.printf "  Unstaged returned: %s\n" (p r)
    | _          -> Printf.printf "  Unstaged failed to complete");
    (match decoder_old data with 
    | Ok (r, bs) -> Printf.printf "  Old returned:      %s\n" (p r)
    | _          -> Printf.printf "  Old failed to complete");
    (match decoder_staged data with 
    | Ok (r, bs) -> Printf.printf "  Staged returned:   %s\n" (p r)
    | _          -> Printf.printf "  Staged failed to complete"); in 

  Printf.printf "Testing:\n";
  (List.iter (fun d -> 
    Printf.printf "%s\n" (p d);
    let encoded = encoder d in
    test encoded
    ) data);
  Printf.printf "\nDone\n";

  let bench name data = 
    let res = Benchmark.throughputN ~repeat:3 1 [
      (name ^ " Unstaged",   decoder_unstaged, data);
      (name ^ " Staged old", decoder_old,      data);
      (name ^ " Staged",     decoder_staged,   data)
    ] in 
    print_newline();
    Benchmark.tabulate res;

    Printf.printf "Just testing the staged part now\n";
    let (g, b) = Asn_staged_reader.Gen.parse Asn_staged_core.Ber data in 
    let res_staged = Benchmark.throughputN ~repeat:3 1 [
      ("Unstaged",   Asn_unstaged_reader.c_asn p2 Asn_unstaged_reader.Ber, g);
      ("Staged",     Hnis_staged.f,     g);
      ("Staged old", Hnis_staged_old.f, g)
    ] in 
    print_newline();
    Benchmark.tabulate res_staged in 

  Printf.printf "\nBenchmarking\n";
  (List.iteri (fun i d -> 
    Printf.printf "%s\n" (p d);
    let encoded = encoder d in
    bench (Int.to_string i) encoded
    ) data);
  Printf.printf "\nDone\n"
