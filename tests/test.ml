(*

Main test file to handle using the alcotest library

*)

(* Some helper and printing functions *)

let bytes_to_hex bs = 
  let chr_seq = Bytes.to_seq bs in
  let int_seq = Seq.map (fun c -> Char.code c) chr_seq in
  let str_seq = Seq.map (fun i -> Printf.sprintf "%02x " i) int_seq in
  Seq.fold_left (^) "" str_seq 

let to_bytes xs =
  Bytes.of_seq (Seq.map (fun x -> Char.chr x) (List.to_seq xs))

let int64_to_bytes i = 
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 i;
  b

let int_to_bytes i = 
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int i);
  b

let pp_error ppf (`Parse err) = Format.fprintf ppf "Parse error: %s" err

let parse_err = Alcotest.testable pp_error (fun (`Parse _) (`Parse _) -> true)

(* The actual tests *)
let encode_booleans () = 
  let bool_asn = Asn.S.bool in 
  let bool_codec = Asn.codec Asn.ber bool_asn in
  let encoder = Asn.encode bool_codec in
  Alcotest.(check bytes) "true  =encodes=to=> 0x0101FF" (to_bytes [0x01; 0x01; 0xFF]) (encoder true);
  Alcotest.(check bytes) "false =encodes=to=> 0x010100" (to_bytes [0x01; 0x01; 0x00]) (encoder false)

let decode_booleans () = 
  let bool_asn = Asn.S.bool in 
  let bool_codec = Asn.codec Asn.ber bool_asn in
  let decoder = Asn.decode bool_codec in
  Alcotest.(check (result (pair bool bytes) parse_err))
    "0x0101FF =decodes=to=> true"
    (Ok(true, Bytes.empty)) 
    (decoder (to_bytes [0x01; 0x01; 0xFF]));
  Alcotest.(check (result (pair bool bytes) parse_err))
    "0x010100 =decodes=to=> false"
    (Ok(false, Bytes.empty))
    (decoder (to_bytes [0x01; 0x01; 0x00]));
  Alcotest.(check (result (pair bool bytes) parse_err))
    "0x010105 =decodes=to=> true"
    (Ok(true, Bytes.empty))
    (decoder (to_bytes [0x01; 0x01; 0x05]))

let circular_booleans () = 
  let bool_asn = Asn.S.bool in 
  let bool_codec = Asn.codec Asn.ber bool_asn in
  let encoder = Asn.encode bool_codec in
  let decoder = Asn.decode bool_codec in
  Alcotest.(check (result (pair bool bytes) parse_err)) 
    "true  ==> true" 
    (Ok(true, Bytes.empty))
    (decoder (encoder true));
  Alcotest.(check (result (pair bool bytes) parse_err))
    "false ==> false"
    (Ok(false, Bytes.empty))
    (decoder (encoder false))

(* Currently all integers are encoded as int64 values, so all will have a length of 8*)
let encode_integers () = 
  let int_asn = Asn.S.integer in
  let int_codec = Asn.codec Asn.ber int_asn in
  let encoder = Asn.encode int_codec in
  Alcotest.(check bytes)
    "0 =encodes=to=> 0x02_08_00000000"
    (Bytes.cat (to_bytes [0x02; 0x08]) (int64_to_bytes Int64.zero))
    (encoder Int64.zero);
  Alcotest.(check bytes)
    "0 =encodes=to=> 0x02_08_11111111"
    (Bytes.cat (to_bytes [0x02; 0x08]) (int64_to_bytes Int64.minus_one))
    (encoder Int64.minus_one);
  Alcotest.(check bytes)
    "0 =encodes=to=> 0x02_08_00000001"
    (Bytes.cat (to_bytes [0x02; 0x08]) (int64_to_bytes Int64.one))
    (encoder Int64.one)

let decode_integers () = 
  let int_asn = Asn.S.integer in 
  let int_codec = Asn.codec Asn.ber int_asn in
  let decoder = Asn.decode int_codec in
  Alcotest.(check (result (pair int64 bytes) parse_err))
    "0x0101FF =decodes=to=> true"
    (Ok( (Int64.zero), Bytes.empty)) 
    (decoder (Bytes.cat (to_bytes [0x02; 0x08]) (int_to_bytes 0)) );
  Alcotest.(check (result (pair int64 bytes) parse_err))
    "0x010100 =decodes=to=> false"
    (Ok( (Int64.one), Bytes.empty))
    (decoder (Bytes.cat (to_bytes [0x02; 0x08]) (int_to_bytes 1)) );
  Alcotest.(check (result (pair int64 bytes) parse_err))
    "0x010105 =decodes=to=> true"
    (Ok( (Int64.minus_one ), Bytes.empty))
    (decoder (Bytes.cat (to_bytes [0x02; 0x08]) (int_to_bytes(-1)) ) )

let random_ints =
  let int_asn = Asn.S.integer in
  let int_codec = Asn.codec Asn.ber int_asn in
  let encoder = Asn.encode int_codec in
  let decoder = Asn.decode int_codec in
  QCheck.Test.make ~count: 1000
    ~name:"Circular Random Integers"
    QCheck.(int64) 
    ( fun i -> decoder (encoder i) = Ok(i, Bytes.empty) )


let () = 
  Alcotest.run "Testing Primitives" [
    ("Encoding Primitives",
      [
        Alcotest.test_case "Encoding Booleans" `Quick encode_booleans;
        Alcotest.test_case "Encoding Integers" `Quick encode_integers
      ]
    );
    ("Decoding Primitives",
      [
        Alcotest.test_case "Decoding Booleans" `Quick decode_booleans;
        Alcotest.test_case "Decoding Integers" `Quick decode_integers;
      ]
    );
    ("Circular Primitives",
      [
        Alcotest.test_case "Circular Booleans" `Quick circular_booleans;
        QCheck_alcotest.to_alcotest random_ints
      ]
    )
  ]