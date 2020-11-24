open Asn_core

module type Prim = sig
  type t
  val of_bytes : bytes -> t
end

module Boolean : Prim with type t = bool = struct
  type t = bool

  (* TODO: needs to check length of b *)
  let of_bytes b = 
    let r = Bytes.get_uint8 b 0 in
    r <> 0
end

module Integer : Prim with type t = int64 = struct
  type t = int64

  let of_bytes b = match Bytes.length b with 
    | 1 -> Int64.of_int(Bytes.get_int8 b 0)
    | 2 -> Int64.of_int(Bytes.get_int16_be b 0)
    | 4 -> Int64.of_int32(Bytes.get_int32_be b 0)
    | 8 -> Bytes.get_int64_be b 0
    (* TODO: need to deal with arbitrary length integers *)
    | _ -> assert false

end