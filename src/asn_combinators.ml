open Asn_core
(*module Prim  = Asn_prim*)


module Int = struct
  type t = int
  let compare (a: t) b = compare a b
  let equal (a: t) b = a = b
end

type cls = [ `Universal | `Application | `Private ]

let to_tag id = function
  | Some `Application -> Tag.Application id
  | Some `Private     -> Tag.Private id
  | Some `Universal   -> Tag.Universal id
  | None              -> Tag.Context_specific id


let rec implicit : type a. ?cls:cls -> int -> a asn -> a asn =
  fun ?cls id -> function 
    | asn -> Implicit (to_tag id cls, asn)


let bool             = Prim Bool
and integer          = Prim Int
and octet_string     = Prim Octets
and bit_string       = Prim Bits
and null             = Prim Null
and real             = Prim Real
and character_string = Prim CharString

let string tag = implicit ~cls:`Universal tag character_string

let utf8_string      = string 0x0C
and numeric_string   = string 0x12
and printable_string = string 0x13
and teletex_string   = string 0x14
and videotex_string  = string 0x15
and ia5_string       = string 0x16
and graphic_string   = string 0x19
and visible_string   = string 0x1A
and general_string   = string 0x1C
and universal_string = string 0x1C
and bmp_string       = string 0x1E

let single a = Last a
and ( @ ) a b = Pair (a, b)
and (-@ ) a b = Pair (a, Last b)
and element a = Element (a)

let sequence seq = Sequence seq

let set seq = Set seq

let choice a b = Choice(a, b)
