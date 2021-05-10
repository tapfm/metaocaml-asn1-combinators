let () =
  let open Asn.S in
  let protocol = sequence (single(required bool)) in
  let p_core   = Asn_combinators.(sequence (single(required bool))) in
  let codec_unstaged = Asn.Unstaged.codec Asn.Unstaged.ber protocol in 
  let encoder = Asn.Unstaged.encode codec_unstaged in 
  Printf.printf "Creating Decoders:\n";
  let decoder_unstaged = Asn.Unstaged.decode codec_unstaged in 
  (* let codec_staged     = Asn.Staged.codec Asn.Staged.ber protocol in  *)
  let decoder_staged   = Bool_staged.decode in (* Asn.Staged.decode codec_staged in *)
  (* let codec_old        = Asn.Staged_old.codec Asn.Staged_old.ber protocol in *)
  let decoder_old      = Bool_staged_old.decode in (* Asn.Staged_old.decode codec_old in *)
  Printf.printf "Done\n";
  Printf.printf "Testing Bool:\n";
  let encoded_true = encoder true in 
  Printf.printf "Checking correct\n";
  (match decoder_unstaged encoded_true with 
  | Ok (b, bs) -> Printf.printf "  Unstaged returned: %B\n" b
  | _          -> Printf.printf "  Unstaged failed to complete");
  (match decoder_old encoded_true with 
  | Ok (b, bs) -> Printf.printf "  Old returned:      %B\n" b
  | _          -> Printf.printf "  Old failed to complete");
  (match decoder_staged encoded_true with 
  | Ok (b, bs) -> Printf.printf "  Staged returned:   %B\n" b
  | _          -> Printf.printf "  Staged failed to complete");
  Printf.printf "Done\n";
  let res_true = Benchmark.throughputN ~repeat:3 1 [
    ("Unstaged",   decoder_unstaged, encoded_true);
    ("Staged old", decoder_old,      encoded_true);
    ("Staged",     decoder_staged,   encoded_true)
  ] in 
  print_newline();
  Benchmark.tabulate res_true;
  Printf.printf "Just testing the staged part now\n";
  let (g, b) = Asn_staged_reader.Gen.parse Asn_staged_core.Ber encoded_true in 
  let res_true_staged = Benchmark.throughputN ~repeat:3 1 [
    ("Unstaged",   Asn_unstaged_reader.c_asn p_core Asn_unstaged_reader.Ber, g);
    ("Staged",     Bool_staged.f,     g);
    ("Staged old", Bool_staged_old.f, g)
  ] in 
  print_newline();
  Benchmark.tabulate res_true_staged
