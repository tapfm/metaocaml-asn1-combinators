type ('a, 'b) sum = L of 'a | R of 'b

type bits = int * bytes

module Tag = struct
  
  type t = 
    | Universal        of int
    | Application      of int
    | Context_specific of int
    | Private          of int

  
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

end

type tag = Tag.t

(* Adapted from the existing asn1-combinators library *)
(* length * (offset -> bytes -> unit)*)
type writer = int * (int -> bytes -> unit)

type _ asn = 
  | Sequence    : 'a sequence -> 'a asn
  | Sequence_of : 'a asn -> 'a list asn
  | Set         : 'a sequence -> 'a asn
  | Set_of      : 'a asn -> 'a list asn
  | Choice      : 'a asn * 'b asn -> ('a, 'b) sum asn

  | Implicit    : tag * 'a asn -> 'a asn
  | Explicit    : tag * 'a asn -> 'a asn
  
  | Prim        : 'a prim -> 'a asn

and _ element =
  | Required : string option * 'a asn -> 'a element
  | Optional : string option * 'a asn -> 'a option element

and _ sequence = 
  | Last  : 'a element -> 'a sequence
  | Pair  : 'a element * 'b sequence -> ('a * 'b) sequence

and _ prim = 
  | Bool       : bool       prim
  | Int        : Z.t        prim
  | Bits       : bool array prim
  | Octets     : bytes      prim
  | Null       : unit       prim
  | OID        : Asn_oid.t  prim
  | Real       : float      prim
  | CharString : string     prim
  | Time       : string     prim

type error = [ `Parse of string ]


let pp_error ppf (`Parse err) = Format.fprintf ppf "Parse error: %s" err

exception Parse_error of error

module Generic = struct

  type t = 
    | Cons of tag * t list
    | Prim of tag * bytes

  let tag = function
    | Cons (t, _) -> t
    | Prim (t, _) -> t
end

let label = function
  | Some s -> s
  | None   -> ""
let seq_tag = Tag.Universal 0x10
and set_tag = Tag.Universal 0x11

let tag_of_prim : type a. a prim -> tag = 
  let open Tag in function 
    | Bool       -> Universal 0x01
    | Int        -> Universal 0x02
    | Bits       -> Universal 0x03
    | Octets     -> Universal 0x04
    | Null       -> Universal 0x05
    | OID        -> Universal 0x06
    | Real       -> Universal 0x09
    | CharString -> Universal 0x1D
    | Time       -> Universal 0x0E

let rec tag_set : type a. a asn -> tag list = function
  | Sequence _          -> [seq_tag]
  | Sequence_of _       -> [seq_tag]
  | Set _               -> [set_tag]
  | Set_of _            -> [set_tag]
  | Choice (asn1, asn2) -> (tag_set asn1) @ (tag_set asn2)

  | Implicit (t, _)     -> [t]
  | Explicit (t, _)     -> [t]

  | Prim p              -> [tag_of_prim p]