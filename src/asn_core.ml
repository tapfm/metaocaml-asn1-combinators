type ('a, 'b) sum = L of 'a | R of 'b

type bits = int * bytes

(* Adapted from the existing asn1-combinators library *)
(* length * (offset -> bytes -> unit)*)
type writer = int * (int -> bytes -> unit)

type _ asn = 
  | Sequence  : 'a sequence -> 'a asn
  | Set       : 'a sequence -> 'a asn
  | Choice    : 'a asn * 'b asn -> ('a, 'b) sum asn
  
  | Prim      : 'a prim -> 'a asn

and _ element =
  | Element   : 'a asn -> 'a element
(*
This will include 'Required' and 'Optional' tags,
but for now I will assume all are required,
so the contructor is simplified
*)

and _ sequence = 
  | Last  : 'a element -> 'a sequence
  | Pair  : 'a element * 'b sequence -> ('a * 'b) sequence

and _ prim = 
  | Bool       : bool       prim
  | Int        : int64      prim
  | Bits       : bits       prim
  | Octets     : bytes      prim
  | Null       : unit       prim
  | OID        : Asn_oid.t  prim
  | CharString : string     prim

(* Not sure how much of this I need ... *)

module Tag = struct
  
  type t = 
    | Universal        of int
    | Application      of int
    | Context_specific of int
    | Private          of int

  (*
  let compare t1 t2 = match (t1, t2) with
    | (Universal        a, Universal        b)
    | (Application      a, Application      b)
    | (Context_specific a, Context_specific b)
    | (Private          a, Private          b)
      -> compare a b
    | (Universal        _, _)
    | (Application      _, (Context_specific _ | Private _))
    | (Context_specific  _, Private _)
      -> -1
    | _
      -> 1

  let equal t1 t2 = match (t1, t2) with
    | (Universal        a, Universal        b)
    | (Application      a, Application      b)
    | (Context_specific a, Context_specific b)
    | (Private          a, Private          b)
      -> a = b
    | _
      -> false
  *)
end

type tag = Tag.t

module Generic = struct

  type t = 
    | Cons of tag * t list
    | Prim of tag * bytes

  let tag = function
    | Cons (t, _) -> t
    | Prim (t, _) -> t
end

let tag_of_prim : type a. a prim -> tag = 
  let open Tag in function 
    | Bool       -> Universal 0x01
    | Int        -> Universal 0x02
    | Bits       -> Universal 0x03
    | Octets     -> Universal 0x04
    | Null       -> Universal 0x05
    | OID        -> Universal 0x06
    | CharString -> Universal 0x1D