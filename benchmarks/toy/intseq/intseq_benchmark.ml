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
  let p2 = Asn_combinators.(sequence (
    (required integer) @
    (optional @@ explicit 69 integer) @
    (required integer) @
    (required integer) @
    (required integer) @
    (optional @@ explicit 420 integer) @
    (optional @@ explicit 237 integer) -@
    (required integer)
  ) ) in
  let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber protocol in 
  let encoder = Asn.Unstaged.encode codec_unstaged in


  Printf.printf "\nCreating Decoders:\n";
  let decoder_unstaged = Asn.Unstaged.decode codec_unstaged in 
  let decoder_staged   = Intseq_staged.decode in
  let decoder_old      = Intseq_staged_old.decode in
  Printf.printf "Done\n";

  Printf.printf "\nGenerating Data\n";
  let p (i1, (o2, (i3, (i4, (i5, (o6, (o7, i8))))))) = 
    let f = function | Some x -> "Some " ^ (Z.to_string x) | None -> "None" in
    let g = Z.to_string in 
    Printf.sprintf "(%s, %s, %s, %s, %s, %s, %s, %s)" (g i1) (f o2) (g i3) (g i4) (g i5) (f o6) (f o7) (g i8) in 
  
  let c (i1, o2, i3, i4, i5, o6, o7, i8) = (i1, (o2, (i3, (i4, (i5, (o6, (o7, i8))))))) in
  let b (i1, (o2, (i3, (i4, (i5, (o6, (o7, i8))))))) = (i1, o2, i3, i4, i5, o6, o7, i8) in


  Random.self_init();
  let data = [
    c(Z.of_int 1, Some (Z.of_int 2), Z.of_int 3, Z.of_int 4, Z.of_int 5, Some (Z.of_int 6), Some (Z.of_int 7), Z.of_int 8);
    c(Z.of_int 1, None             , Z.of_int 3, Z.of_int 4, Z.of_int 5, None             , None             , Z.of_int 8);
    Asn.random protocol;
    Asn.random ~full:true protocol
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
      ("Staged",     Intseq_staged.f,     g);
      ("Staged old", Intseq_staged_old.f, g)
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
