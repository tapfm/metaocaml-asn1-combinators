open Asn_core
(*module Prim  = Asn_prim*)


module Int = struct
  type t = int
  let compare (a: t) b = compare a b
  let equal (a: t) b = a = b
end


let bool         = Prim Bool
and integer      = Prim Int
and octet_string = Prim Octets
and null         = Prim Null

let single a = Last a
and ( @ ) a b = Pair (a, b)
and (-@ ) a b = Pair (a, Last b)
and element a = Element (a)

let sequence seq = Sequence seq

let set seq = Set seq

let choice a b = Choice(a, b)
