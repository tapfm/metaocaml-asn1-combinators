open Asn_core

module type Prim = sig
  type t
  val of_bytes  : bytes -> t

  val to_writer : t -> writer

end

module type Prim_s = sig
  include Prim
  
  (* Not sure if I like the name for this -- but it is consistent with Bytes.concat *)
  val concat : t list -> t
  val length : t      -> int

end

module Boolean : Prim with type t = bool = struct
  type t = bool

  let of_bytes bs = 
    if Bytes.length bs = 1 then
      let r = Bytes.get_uint8 bs 0 in
      r <> 0
    else 
      failwith "Boolean must have a length of 1 octet"

  let to_writer b = 
    let encoded = if b then 0xFF else 0x00 in
    (1, fun off bs -> Bytes.set_uint8 bs off encoded)
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
    | _ -> failwith "Unsupported length of integer"

  let to_writer i = 
    (8, fun off bs -> Bytes.set_int64_be bs off i)

end

module Null : Prim with type t = unit = struct
  type t = unit

  let of_bytes b =
    if Bytes.length b = 0 then
      ()
    else
      failwith "Null must be encoded with a length of 0"

  let to_writer () =
    (0, fun (off : int) (bs : bytes) -> ())

  let to_bytes () =
    Bytes.empty
end

module Octets : Prim_s with type t = bytes = struct
  
  type t = bytes

  let of_bytes bs = bs

  let to_writer os = 
    let len = Bytes.length os in
    (len, fun off bs -> Bytes.blit os 0 bs off len)

  let concat = Bytes.(concat empty)

  let length = Bytes.length
end