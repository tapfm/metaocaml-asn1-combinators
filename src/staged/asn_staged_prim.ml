open Asn_staged_core

module type Prim = sig
  type t
  val of_bytes  : bytes code -> t code

end

let rec read_n_bytes (off: int) (bs : bytes) (acc : int64) = function 
  | 0 -> acc
  | n -> let acc' = Int64.(logor (shift_left acc 8) (of_int (Bytes.get_uint8 bs off)) ) in
         read_n_bytes (off + 1) (bs) acc' (n - 1)


module type Prim_s = sig
  include Prim
  
  (* Not sure if I like the name for this -- but it is consistent with Bytes.concat *)
  val concat : t list code -> t code
  val length : t      -> int

end

module Boolean : Prim with type t = bool = struct
  type t = bool

  let of_bytes : bytes code -> t code = fun bs ->
    .<
      if Bytes.length .~bs = 1 then
        (Bytes.get_uint8 .~bs 0) <> 0
      else 
        failwith "Boolean must have a length of 1 octet"
    >.
end

module Integer : Prim with type t = Z.t = struct
  (* This should be changed to a type that can handle "primitive" integers and big integers *)
  type t = Z.t

  let of_bytes : bytes code -> t code = fun bs ->
    .<
      let rec go acc i = function 
        | n when n >= 8 ->
          let w = Z.of_int64 (Bytes.get_int64_be .~bs i) in 
          go Z.((acc lsl 64) lor (extract w 0 64)) (i + 8) (n - 8)
        | n when n >= 4 -> 
          let w = Z.of_int32 (Bytes.get_int32_be .~bs i) in 
          go Z.((acc lsl 32) lor (extract w 0 32)) (i + 4) (n - 4)
        | n when n >= 2 -> 
          let w = Z.of_int(Bytes.get_uint16_be .~bs i) in
          go Z.((acc lsl 16) lor w) (i + 2) (n - 2)
        | 1 ->
          let w = Z.of_int(Bytes.get_uint8 .~bs i) in
          Z.((acc lsl 8) lor w)
        | _ -> acc in
      if Bytes.length .~bs > 1
        then 
          let w0 = Bytes.get_int16_be .~bs 0 in 
          match w0 land 0xFF80 with 
          | 0x0000
          | 0xFF80 -> failwith "Integer Error: Redundant form"
          | _      -> (match Bytes.length .~bs with
                        | 2 -> Z.of_int   (Bytes.get_int16_be .~bs 0)
                        | 4 -> Z.of_int32 (Bytes.get_int32_be .~bs 0)
                        | 8 -> Z.of_int64 (Bytes.get_int64_be .~bs 0)
                        | n -> go (Z.of_int w0) 2 (n - 2) )
        else 
          match Bytes.length .~bs with 
          | 0 -> failwith "Integer Error: Length = 0"
          | 1 -> Z.of_int (Bytes.get_int8     .~bs 0)
          | n -> assert false
    >.

end

module Bits : Prim_s with type t = bool array = struct
  
  type t = bool array

  let of_bytes : bytes code -> t code = fun bs ->
    .<
      assert (Bytes.length .~bs >= 1);
      let unused = Bytes.get_uint8 .~bs 0 in 
      let lbody  = Bytes.length .~bs - 1 in 
      let num_bits = 8 * lbody - unused in 
      Array.init num_bits (fun i -> let byte = (Bytes.get_uint8 .~bs (1 + (i / 8))) lsl (i mod 8) in byte land 0x80 = 0x80 )
    >.
  
  (*let concat : t list code -> t code = fun ts -> .<Array.concat .~ts>.*)

  let concat b = .<Array.concat .~b>.

  let length arr = Array.length arr
end

module Octets : Prim_s with type t = bytes = struct
  
  type t = bytes

  let of_bytes : bytes code -> t code = fun bs -> bs

  let concat : t list code -> t code = fun ts -> .<Bytes.(concat empty .~ts)>.

  let length = Bytes.length
end

module Null : Prim with type t = unit = struct
  type t = unit

  let of_bytes : bytes code -> t code = fun b ->
    .<
      if Bytes.length .~b = 0 then
        ()
      else
        failwith "Null must be encoded with a length of 0"
    >.

end

module Real : Prim with type t = float = struct
  
  type t = float

  let decode_binary : bytes code -> t code = fun bs -> 
    .<
      let b0   = Bytes.get_uint8 .~bs 0 in
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
                                  | 0x00 -> (Float.of_int(Bytes.get_int8 .~bs 1),1)
                                  | 0x01 -> (Float.of_int(Bytes.get_int16_be .~bs 1), 2)
                                  | 0x02 -> (*3 byte 2's complement number*)
                                            failwith "Unimplemented - X.690 8.5.7.4 c"
                                  | 0x03 -> (*n byte 2's complement number*)
                                            failwith "Unimplemented - X.690 8.5.7.4 d"
                                  | _    -> assert false) in
      let mant = Int64.to_float (read_n_bytes (1 + expn_length) .~bs 0L ((Bytes.length .~bs) - (expn_length + 1))) in
      (sign *. mant *. fact) *. (Float.pow base expn)
    >.


  let of_bytes : bytes code -> t code = fun bs ->
    .<
      if Bytes.length .~bs = 0 then
        Float.zero
      else 
        let b0 = Bytes.get_uint8 .~bs 0 in 
        match (b0 land 0x80) with 
        | 0x80 -> .~(decode_binary bs)
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

end

module Gen_string : Prim_s with type t = string = struct
  type t = string

  let of_bytes : bytes code -> t code = fun bs -> .<Bytes.to_string .~bs>.

  let concat : t list code -> t code = fun ts -> .<String.concat "" .~ts>.

  let length = String.length
end

module OID : Prim with type t = Asn_oid.t = struct

  type t = Asn_oid.t

  open Asn_oid

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

  let of_bytes_rec = .< fun bs ->
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

  let of_bytes : bytes code -> t code = 
    fun bs -> .< .~of_bytes_rec .~bs>.

end


(*This just treats a time value as a string*)
module Time : Prim with type t = string = struct
  type t = string

  let of_bytes : bytes code -> t code = fun bs -> .<Bytes.to_string .~bs>.

end
