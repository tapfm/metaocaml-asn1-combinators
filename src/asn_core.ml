type ('a, 'b) sum = L of 'a | R of 'b

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
  | Bool  : bool  prim
  | Int   : int   prim

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