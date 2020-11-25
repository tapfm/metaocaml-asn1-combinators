open Asn_core

module type Prim = sig
  type t
  val of_bytes : bytes -> t

  val to_bytes : t -> bytes
end

module Boolean : Prim with type t = bool = struct
  type t = bool

  (* TODO: needs to check length of b *)
  let of_bytes b = 
    if Bytes.length b = 1 then
      let r = Bytes.get_uint8 b 0 in
      r <> 0
    else 
      failwith "Boolean must have a length of 1 octet"

  let to_bytes boolean = 
    if boolean then
      Bytes.make(1)(Char.chr(0xFF))
    else 
      Bytes.make(1)(Char.chr(0x00))
end

module Integer : Prim with type t = int64 = struct
  (* This should be changed to a type that can handle "primitive" integers and big integers *)
  type t = int64

  let of_bytes b = match Bytes.length b with 
    | 1 -> Int64.of_int(Bytes.get_int8 b 0)
    | 2 -> Int64.of_int(Bytes.get_int16_be b 0)
    | 4 -> Int64.of_int32(Bytes.get_int32_be b 0)
    | 8 -> Bytes.get_int64_be b 0
    (* TODO: need to deal with arbitrary length integers *)
    | _ -> assert false

  let to_bytes i = 
    let b = Bytes.create 8 in
    Bytes.set_int64_be b 0 i;
    b

end