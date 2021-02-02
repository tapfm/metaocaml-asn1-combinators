open Asn_staged_core_old

module type Prim = sig
  type t
  val of_bytes  : (bytes -> t) code

  val to_writer : t -> writer

end

let rec write_n_bytes (off : int) (bs : bytes) (n : int) (x : int64) = 
  assert (n >= 0 && n <= 8);
  match n with
  | 0 ->  ()
  | 1 ->  Bytes.set_uint8 bs off Int64.(to_int (logand x 0xFFL))
  | 2 ->  Bytes.set_uint16_be bs off Int64.(to_int (logand x 0xFFFFL))
  | 3 ->  Bytes.set_uint16_be bs off Int64.(to_int (logand (shift_right x 8) 0xFFFFL));
          Bytes.set_uint8 bs (off + 1) Int64.(to_int (logand x 0xFFL))
  | 4 ->  Bytes.set_int32_be bs off Int64.(to_int32 (logand x 0xFFFFFFFFL))
  | 8 ->  Bytes.set_int64_be bs off x
  | n ->  (* 5, 6, 7*)
          Bytes.set_int32_be bs off Int64.(to_int32 (logand (shift_right x ((n - 4) * 8)) 0xFFFFFFFFL));
          write_n_bytes (off + 4) bs (n - 4) x

let rec read_n_bytes (off: int) (bs : bytes) (acc : int64) = function 
  | 0 -> acc
  | n -> let acc' = Int64.(logor (shift_left acc 8) (of_int (Bytes.get_uint8 bs off)) ) in
         read_n_bytes (off + 1) (bs) acc' (n - 1)


module type Prim_s = sig
  include Prim
  
  (* Not sure if I like the name for this -- but it is consistent with Bytes.concat *)
  val concat : (t list -> t) code
  val length : t      -> int

end

module Boolean : Prim with type t = bool = struct
  type t = bool

  let of_bytes = .<fun bs ->
    if Bytes.length bs = 1 then
      let r = Bytes.get_uint8 bs 0 in
      r <> 0
    else 
      failwith "Boolean must have a length of 1 octet"
  >.

  let to_writer b = 
    let encoded = if b then 0xFF else 0x00 in
    (1, fun off bs -> Bytes.set_uint8 bs off encoded)
end

module Integer : Prim with type t = Z.t = struct
  (* This should be changed to a type that can handle "primitive" integers and big integers *)
  type t = Z.t

  let of_bytes = .< fun bs ->
    let rec go acc i = function 
      | n when n >= 8 ->
        let w = Z.of_int64 (Bytes.get_int64_be bs i) in 
        go Z.((acc lsl 64) lor (extract w 0 64)) (i + 8) (n - 8)
      | n when n >= 4 -> 
        let w = Z.of_int32 (Bytes.get_int32_be bs i) in 
        go Z.((acc lsl 32) lor (extract w 0 32)) (i + 4) (n - 4)
      | n when n >= 2 -> 
        let w = Z.of_int(Bytes.get_uint16_be bs i) in
        go Z.((acc lsl 16) lor w) (i + 2) (n - 2)
      | 1 ->
        let w = Z.of_int(Bytes.get_uint8 bs i) in
        Z.((acc lsl 8) lor w)
      | _ -> acc in
    if Bytes.length bs > 1
      then 
        let w0 = Bytes.get_int16_be bs 0 in 
        match w0 land 0xFF80 with 
        | 0x0000
        | 0xFF80 -> failwith "Integer Error: Redundant form"
        | _      -> (match Bytes.length bs with
                      | 2 -> Z.of_int   (Bytes.get_int16_be bs 0)
                      | 4 -> Z.of_int32 (Bytes.get_int32_be bs 0)
                      | 8 -> Z.of_int64 (Bytes.get_int64_be bs 0)
                      | n -> go (Z.of_int w0) 2 (n - 2) )
      else 
        match Bytes.length bs with 
        | 0 -> failwith "Integer Error: Length = 0"
        | 1 -> Z.of_int   (Bytes.get_int8     bs 0)
        | n -> assert false
  >.

  let last8 z = Z.(extract z 0 8 |> to_int)

  let to_writer n = 
    let sz  = Z.size n * 8 + 1 in 
    let sz1 = sz - 1 in 
    let bs  = Bytes.create sz in 

    let rec write i n = 
      if n = Z.minus_one || n = Z.zero 
        then i
        else (Bytes.set_uint8 bs i (last8 n);
              write (pred i) Z.(n asr 8) ) in
    let (bad_b0, padding) = 
        if n >= Z.zero
          then ((<=) 0x80, 0x00)
          else ((>)  0x80, 0xFF) in 
    let off =
      let i = write sz1 n in
      if  i = sz1 || bad_b0 (Bytes.get_uint8 bs (succ i)) 
        then (Bytes.set_uint8 bs i padding; i)
        else succ i in 
    let b2 = Bytes.sub bs off (sz - off) in 
    let x  = Bytes.length b2 in 
    (x, fun off bs -> Bytes.blit b2 0 bs off x)

end

module Bits : Prim_s with type t = bool array = struct
  
  type t = bool array

  let of_bytes = .< fun bs ->
    assert (Bytes.length bs >= 1);
    let unused = Bytes.get_uint8 bs 0 in 
    let lbody  = Bytes.length bs - 1 in 
    let num_bits = 8 * lbody - unused in 
    Array.init num_bits (fun i -> let byte = (Bytes.get_uint8 bs (1 + (i / 8))) lsl (i mod 8) in byte land 0x80 = 0x80 )
  >.

  let (|<) n = function 
    | true  -> (n lsl 1) lor 1
    | false -> (n lsl 1)

  let of_array arr = 
    let bs = Bytes.create ((Array.length arr + 7) / 8) in 
    match
      Array.fold_left
        (fun (n, acc, i) bit ->
          if n = 8 then
            (Bytes.set_uint8 bs i acc; (1, 0 |< bit, i + 1) )
          else (n + 1, acc |< bit, i))
        (0,0,0)
        arr
    with
    | (0, _acc, _) -> (0, bs)
    | (n,  acc, i) -> 
      Bytes.set_uint8 bs i (acc lsl (8 - n));
      (8 - n, bs)

  let to_writer arr =
    let (unused, bbs) = of_array arr in 
    let size = Bytes.length bbs in
    (size + 1, fun off bs -> Bytes.set_uint8 bs off unused; (Bytes.blit bbs 0 bs (off + 1) size))
  
  let concat = .<Array.concat>.

  let length arr = Array.length arr
end

module Octets : Prim_s with type t = bytes = struct
  
  type t = bytes

  let of_bytes = .<fun bs -> bs>.

  let to_writer os = 
    let len = Bytes.length os in
    (len, fun off bs -> Bytes.blit os 0 bs off len)

  let concat = .<Bytes.(concat empty)>.

  let length = Bytes.length
end

module Null : Prim with type t = unit = struct
  type t = unit

  let of_bytes = .< fun b ->
    if Bytes.length b = 0 then
      ()
    else
      failwith "Null must be encoded with a length of 0"
  >.

  let to_writer () =
    (0, fun (off : int) (bs : bytes) -> ())

end

module Real : Prim with type t = float = struct
  
  type t = float

  let decode_binary = fun bs -> 
    let b0   = Bytes.get_uint8 bs 0 in
    let sign = if (b0 land 0x40) = 0x40 then Float.minus_one else Float.one in 
    let base = (match (b0 land 0x30) with
                | 0x00 -> 2.
                | 0x10 -> 8.
                | 0x20 -> 16.
                | 0x30 -> failwith "Undefined - Reserved for future definitions"
                | _    -> assert false) in 
    let fact = (match (b0 land 0x0C) with
                | 0x00 -> 1. (*2^0*)
                | 0x40 -> 2. (*2^1*)
                | 0x80 -> 4. (*2^2*)
                | 0xC0 -> 8. (*2^3*)
                | _    -> assert false) in
    let (expn, expn_length)  = (match (b0 land 0x03) with 
                                | 0x00 -> (Float.of_int(Bytes.get_int8 bs 1),1)
                                | 0x01 -> (Float.of_int(Bytes.get_int16_be bs 1), 2)
                                | 0x02 -> (*3 byte 2's complement number*)
                                          failwith "Unimplemented - X.690 8.5.7.4 c"
                                | 0x03 -> (*n byte 2's complement number*)
                                          failwith "Unimplemented - X.690 8.5.7.4 d"
                                | _    -> assert false) in
    let mant = Int64.to_float (read_n_bytes (1 + expn_length) bs 0L ((Bytes.length bs) - (expn_length + 1))) in
    (sign *. mant *. fact) *. (Float.pow base expn)


  let of_bytes = .<fun bs ->
    if Bytes.length bs = 0 then
      Float.zero
    else 
      let b0 = Bytes.get_uint8 bs 0 in 
      match (b0 land 0x80) with 
      | 0x80 -> (*Binary encoding*) 0. (*.~(decode_binary) bs*)
      | 0x00 -> ( match b0 land 0x40 with
        | 0x40 -> (*"Special Real value"*)
                  (match b0 with 
                    | 0b01000000 -> infinity
                    | 0b01000001 -> neg_infinity
                    | 0b01000010 -> nan
                    | 0b01000011 -> Float.(neg zero)
                    | _          -> failwith "Undefined - Reserved for future definitions")
        | 0x00 -> (*Number encoding -- unimplemented*)
                  (match b0 with
                    | 0b0000_0001 -> failwith "Unimplemented - ISO 6093 NR1 form"
                    | 0b0000_0010 -> failwith "Unimplemented - ISO 6093 NR2 form"
                    | 0b0000_0011 -> failwith "Unimplemented - ISO 6093 NR3 form"
                    | _           -> failwith "Undefined - Reserved for future definitions")
        | _    -> (*Should be impossible *) assert false )
      | _    -> (*Should be impossible *) assert false
  >.

  let rec mant_to_int64 (acc, len) f = match Float.classify_float f with
    | FP_zero      -> (acc, len - 1)
    | FP_normal    -> let (frac, intl) = Float.modf f in 
                      let acc' = if Float.(compare intl zero) > 0 then Int64.(logor (shift_left acc 1) 1L) else Int64.(shift_left acc 1) in 
                      mant_to_int64 (acc', len + 1) (frac *. 2.)
    | FP_subnormal -> failwith "Unimplemented - Subnormal floats"
    | _            -> assert false

  let to_writer = function
  | f when Float.((equal zero f) && not (sign_bit f)) ->
      (0, fun off bs -> ())
  | f when Float.((equal zero f) && (sign_bit f)) -> 
      (1, fun off bs -> Bytes.set_uint8 bs off 0b01000011)
  | f when Float.is_nan f ->
      (1, fun off bs -> Bytes.set_uint8 bs off 0b01000010)
  | f when Float.( (is_infinite f) && not (sign_bit f) ) ->
      (1, fun off bs -> Bytes.set_uint8 bs off 0b01000000)
  | f when Float.( (is_infinite f) && (sign_bit f) ) ->
      (1, fun off bs -> Bytes.set_uint8 bs off 0b01000001)
  | f -> (*I would like to do this directly manipulating the bits at some point, however for the time being I am doing it this way*)
      let b0 = if Float.sign_bit f then 0b1_1_00_00_01 else 0b1_0_00_00_01 in 
      let (mant_float, expn) = Float.frexp f in
      let (mant, len) = mant_to_int64 (0L,0) Float.(abs mant_float) in
      let byte_len = ((len + 7) / 8) in 
      (3 + byte_len, fun off bs -> Bytes.set_uint8 bs off b0; Bytes.set_int16_be bs (off + 1) (expn - len);  write_n_bytes (off + 3) bs byte_len mant)



end

module Gen_string : Prim_s with type t = string = struct
  type t = string

  let of_bytes = .<fun bs -> Bytes.to_string bs>.

  let to_writer s = let n = String.length s in (n, fun off bs -> Bytes.blit_string s 0 bs off n)

  let concat = .<String.concat "">.

  let length = String.length
end

module OID : Prim with type t = Asn_oid.t = struct

  open Asn_oid

  (*let (<+>) = Asn_writer.(<+>)*)

  let (<+>) : writer -> writer -> writer =
    fun (len_1, writer_1) (len_2, writer_2) ->
    let w off bs = 
      (writer_1 off bs; writer_2 (off + len_1) bs) in
    (len_1 + len_2, w)

  type t = Asn_oid.t

  let chain = .<fun bs i n ->
    let rec go acc bs i = function 
      | 0 -> failwith "OID Error: unterminated component"
      | n -> 
        match Bytes.get_uint8 bs i with 
        | 0x80 when acc = 0L -> failwith "OID Error: Redundant form"
        | b ->
          let lo = b land 0x7F in 
          let acc = Int64.(logor (shift_left acc 7) (of_int lo)) in 
          if b < 0x80 then (Int64.to_int acc, i + 1) else go acc bs (i + 1) (n - 1) in 
    if n < 1 then (failwith "OID Error: 0 length component")
    else go 0L bs i (min n 8)
  >.

  let of_bytes= .< fun bs ->
    let rec components bs i = function 
      | 0 -> []
      | n -> let (c, i') = .~(chain) bs i n in 
             c :: components bs i' (n + i - i') in
    match Bytes.length bs with 
    | 0 -> failwith "OID Error: 0 length"
    | n -> let (b1, i) = .~(chain) bs 0 n in
           let v1 = (min (b1 / 40) 2) in
           let v2 = b1 - (40 * v1) in 
           match base_opt v1 v2 with 
             | Some b -> b <|| components bs i (n - 1)
             | None   -> failwith "OID Error: Invalid Base"
  >.


  let write_uint8_list lst = 
    let open List in 
    let w off bs = 
      iteri (fun i -> Bytes.set_uint8 bs (off + i)) lst in 
    (length lst, w)

  let to_writer (Oid(v1, v2, vs)) = 
    let cons x = function [] -> [x] | xs -> x lor 0x80 :: xs in 
    let rec component xs x = 
      if x < 0x80 then cons x xs
      else component (cons (x land 0x7f) xs) (x lsr 7)
    and values = function
      | []    -> (0, fun _off _bs -> ())
      | v::vs -> (write_uint8_list (component [] v)) <+> values vs in 
    (1, fun off bs -> Bytes.set_uint8 bs off (v1 * 40 + v2)) <+> values vs
end


(*This just treats a time value as a string*)
module Time : Prim with type t = string = struct
  type t = string

  let of_bytes = .<fun bs -> Bytes.to_string bs>.

  let to_writer str =
    let len = String.length str in 
    (len, fun off bs -> Bytes.blit_string str 0 bs off len)
end
