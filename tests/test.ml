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

let parse_err = Alcotest.testable Asn_core.pp_error (fun (`Parse _) (`Parse _) -> true)

(* The actual tests *)
let encode_booleans () = 
  let bool_asn   = Asn.S.bool in 
  let bool_codec = Asn.codec Asn.ber bool_asn in
  let encoder    = Asn.encode bool_codec in
  Alcotest.(check bytes) "true  =encodes=to=> 0x0101FF" (to_bytes [0x01; 0x01; 0xFF]) (encoder true);
  Alcotest.(check bytes) "false =encodes=to=> 0x010100" (to_bytes [0x01; 0x01; 0x00]) (encoder false)

let decode_booleans () = 
  let bool_asn   = Asn.S.bool in 
  let bool_codec = Asn.codec Asn.ber bool_asn in
  let decoder    = Asn.decode bool_codec in
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
  let bool_asn   = Asn.S.bool in 
  let bool_codec = Asn.codec Asn.ber bool_asn in
  let encoder    = Asn.encode bool_codec in
  let decoder    = Asn.decode bool_codec in
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
  let int_asn   = Asn.S.integer in
  let int_codec = Asn.codec Asn.ber int_asn in
  let encoder   = Asn.encode int_codec in
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
  let int_asn   = Asn.S.integer in 
  let int_codec = Asn.codec Asn.ber int_asn in
  let decoder   = Asn.decode int_codec in
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
  let int_asn   = Asn.S.integer in
  let int_codec = Asn.codec Asn.ber int_asn in
  let encoder   = Asn.encode int_codec in
  let decoder   = Asn.decode int_codec in
  QCheck.Test.make ~count: 1000
    ~name:"Circular Random Integers"
    QCheck.(int64) 
    ( fun i -> decoder (encoder i) = Ok(i, Bytes.empty) )

let encode_bits () = 
  let bit_asn   = Asn.S.bit_string in 
  let bit_codec = Asn.codec Asn.ber bit_asn in 
  let encoder   = Asn.encode bit_codec in 
  Alcotest.(check bytes) "\"\" =encodes=to=> 0x0301_00"
    (to_bytes [0x03; 0x01; 0x00])
    (encoder [||]);
  Alcotest.(check bytes) "0b1 =encodes=to=> 0x0302_07_80"
    (to_bytes [0x03; 0x02; 0x07; 0x80])
    (encoder [|true|]);
  Alcotest.(check bytes) "0b11 =encodes=to=> 0x0302_06_C0"
    (to_bytes [0x03; 0x02; 0x06; 0xC0])
    (encoder [|true; true|]);
  Alcotest.(check bytes) "0b1111_1111 =encodes=to=> 0x0302_00_FF"
    (to_bytes [0x03; 0x02; 0x00; 0xFF])
    (encoder [|true; true; true; true; true; true; true; true|]);
  Alcotest.(check bytes) "0b0000_0000 =encodes=to=> 0x0302_00_00"
    (to_bytes [0x03; 0x02; 0x00; 0x00])
    (encoder [|false; false; false; false; false; false; false; false|]);
  Alcotest.(check bytes) "0b1010_1010 =encodes=to=> 0x0302_00_AA"
    (to_bytes [0x03; 0x02; 0x00; 0xAA])
    (encoder [|true; false; true; false; true; false; true; false|])

let decode_bits () = 
  let bit_asn   = Asn.S.bit_string in 
  let bit_codec = Asn.codec Asn.ber bit_asn in 
  let decoder   = Asn.decode bit_codec in 
  Alcotest.(check (result (pair (array bool) bytes) parse_err))
    "0x0301_08 =decodes=to=> \"\""
    (Ok([||], Bytes.empty))
    (decoder (to_bytes [0x03; 0x01; 0x00]));
  Alcotest.(check (result (pair (array bool) bytes) parse_err))
    "0x0302_07_80 =decodes=to=> 0b1"
    (Ok([|true|], Bytes.empty))
    (decoder (to_bytes [0x03; 0x02; 0x07; 0x80]));
  Alcotest.(check (result (pair (array bool) bytes) parse_err))
    "0x0302_06_C0 =decodes=to=> 0b11"
    (Ok([|true; true|], Bytes.empty))
    (decoder (to_bytes [0x03; 0x02; 0x06; 0xC0]));
  Alcotest.(check (result (pair (array bool) bytes) parse_err))
    "0x0302_00_AA =decodes=to=> 0b1010_1010"
    (Ok([|true; false; true; false; true; false; true; false|], Bytes.empty))
    (decoder (to_bytes [0x03; 0x02; 0x00; 0xAA]))

let random_bits =
  let bit_asn   = Asn.S.bit_string in 
  let bit_codec = Asn.codec Asn.ber bit_asn in 
  let encoder   = Asn.encode bit_codec in
  let decoder   = Asn.decode bit_codec in
  QCheck.Test.make ~count: 1000
    ~name:"Circular Random Bit Strings"
    QCheck.(array bool) 
    ( fun arr -> decoder (encoder arr) = Ok(arr, Bytes.empty) )


(* encode_octets and decode_octets are for simpler / shorter test -> more complicated one will be covered in the circular tests*)
let encode_octets () = 
  let octet_asn   = Asn.S.octet_string in
  let octet_codec = Asn.codec Asn.ber octet_asn in 
  let encoder     = Asn.encode octet_codec in 
  Alcotest.(check bytes) "\"\" =encodes=to=> 0x0401_01" (to_bytes [0x04; 0x00]) (encoder Bytes.empty);
  Alcotest.(check bytes) "0x01 =encodes=to=> 0x0401_01" (to_bytes [0x04; 0x01; 0x01]) (encoder (to_bytes [0x01]));
  let bytes_127 = Bytes.make 127 'a' in 
  Alcotest.(check bytes) "bytes_127 =encodes=to=> 0x047F ^ bytes127"
    (Bytes.cat (to_bytes [0x04; 0x7F]) bytes_127)
    (encoder bytes_127);
  let bytes_128 = Bytes.make 128 'b' in 
  Alcotest.(check bytes) "bytes_128 =encodes=to=> 0x048180 ^ bytes128"
    (Bytes.cat (to_bytes [0x04; 0x81; 0x80]) bytes_128)
    (encoder bytes_128)

let decode_octets () = 
  let octet_asn   = Asn.S.octet_string in
  let octet_codec = Asn.codec Asn.ber octet_asn in 
  let decoder     = Asn.decode octet_codec in 
  Alcotest.(check (result (pair bytes bytes) parse_err)) 
    "0x0401_01 =decodes=to=> \"\"" 
    (Ok(Bytes.empty, Bytes.empty))
    (decoder (to_bytes [0x04; 0x00]));
  Alcotest.(check (result (pair bytes bytes) parse_err))
    "0x0401_01 =decodes=to=> 0x01"
    (Ok(to_bytes [0x01], Bytes.empty))
    (decoder (to_bytes [0x04; 0x01; 0x01]));
  let bytes_127 = Bytes.make 127 'c' in
  let bytes_128 = Bytes.make 128 'd' in
  Alcotest.(check (result (pair bytes bytes) parse_err))
    "0x047F ^ bytes127 =decodes=to=> bytes_127"
    (Ok(bytes_127, Bytes.empty))
    (decoder (Bytes.cat (to_bytes [0x04; 0x7F]) bytes_127));
  Alcotest.(check (result (pair bytes bytes) parse_err))
    "0x048180 ^ bytes128 =decodes=to=> bytes_128"
    (Ok(bytes_128, Bytes.empty))
    (decoder (Bytes.cat (to_bytes [0x04; 0x81; 0x80]) bytes_128) )
  
let random_octets =
  let octet_asn   = Asn.S.octet_string in
  let octet_codec = Asn.codec Asn.ber octet_asn in 
  let encoder     = Asn.encode octet_codec in 
  let decoder     = Asn.decode octet_codec in 
  QCheck.Test.make ~count: 1000
    ~name:"Circular Random Octets"
    QCheck.(string) 
    ( fun s -> let b = Bytes.of_string s in decoder (encoder b) = Ok(b, Bytes.empty) )

let encode_null () = 
  let null_asn = Asn.S.null in
  let null_codec = Asn.codec Asn.ber null_asn in
  let encoder    = Asn.encode null_codec in
  Alcotest.(check bytes) "unit =encodes=to=> 0x0500" (to_bytes [0x05; 0x00]) (encoder ())

let decode_null () = 
  let null_asn = Asn.S.null in
  let null_codec = Asn.codec Asn.ber null_asn in
  let decoder    = Asn.decode null_codec in
  Alcotest.(check (result (pair unit bytes) parse_err))
    "0x0500 =decodes=to=> unit"
    (Ok((), Bytes.empty))
    (decoder (to_bytes [0x05; 0x00]))

let circular_null () = 
  let null_asn   = Asn.S.null in
  let null_codec = Asn.codec Asn.ber null_asn in
  let encoder    = Asn.encode null_codec in
  let decoder    = Asn.decode null_codec in
  Alcotest.(check (result (pair unit bytes) parse_err))
    "unit ==> unit"
    (Ok((), Bytes.empty))
    (decoder (encoder ()))

let () = 
  Alcotest.run "Testing Primitives" [
    ("Encoding Primitives",
      [
        Alcotest.test_case "Encoding Booleans"   `Quick encode_booleans;
        Alcotest.test_case "Encoding Integers"   `Quick encode_integers;
        Alcotest.test_case "Encoding Bit String" `Quick encode_bits;
        Alcotest.test_case "Encoding Octets"     `Quick encode_octets;
        Alcotest.test_case "Encoding Null"       `Quick encode_null;
      ]
    );
    ("Decoding Primitives",
      [
        Alcotest.test_case "Decoding Booleans"   `Quick decode_booleans;
        Alcotest.test_case "Decoding Integers"   `Quick decode_integers;
        Alcotest.test_case "Decoding Bit String" `Quick encode_bits;
        Alcotest.test_case "Decoding Octets"     `Quick decode_octets;
        Alcotest.test_case "Decoding Null"       `Quick decode_null;
      ]
    );
    ("Circular Primitives",
      [
        Alcotest.test_case "Circular Booleans" `Quick circular_booleans;
        QCheck_alcotest.to_alcotest random_ints;
        QCheck_alcotest.to_alcotest random_octets;
        QCheck_alcotest.to_alcotest random_bits;
        Alcotest.test_case "Circular Null"     `Quick circular_null;
      ]
    )
  ]
