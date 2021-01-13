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


let explicit ?cls id asn = Explicit (to_tag id cls, asn)

let rec implicit : type a. ?cls:cls -> int -> a asn -> a asn = 
  fun ?cls id -> function
    | Choice (_, _) as asn -> explicit ?cls id asn
    | asn                  -> Implicit (to_tag id cls, asn)

let rec implicit : type a. ?cls:cls -> int -> a asn -> a asn =
  fun ?cls id -> function 
    | asn -> Implicit (to_tag id cls, asn)


let bool             = Prim Bool
and integer          = Prim Int
and octet_string     = Prim Octets
and bit_string       = Prim Bits
and null             = Prim Null
and real             = Prim Real
and oid              = Prim OID
and character_string = Prim CharString
and time_val         = Prim Time

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

let time tag = implicit ~cls:`Universal tag time_val

let utc_time         = time 0x17 
and generalized_time = time 0x18

let single a = Last a
and ( @ ) a b = Pair (a, b)
and (-@ ) a b = Pair (a, Last b)
and optional ?label a = Optional(label, a)
and required ?label a = Required(label, a)

let sequence seq = Sequence seq

let sequence_of asn = Sequence_of asn

let set seq = Set seq

let set_of asn = Set_of asn

let choice a b = Choice(a, b)
