(*
    Copyright notice to go here
*)

(**

    Staged implementation of `ocaml-asn1-combinators'

    For Part II University of Cambridge project

*)

(** {1 Object Identifiers} *)

type oid
(** ASN.1 [OBJECT IDENTIFIER]. *)

module OID : sig

    type t = oid

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val seeded_hash : int -> t -> int

    val base : int -> int -> t

    val (<|) : t -> int -> t

    val (<||) : t -> int list -> t

    val to_nodes : t -> int * int * int list

    val of_nodes : int -> int -> int list -> t option

    val pp : Format.formatter -> t -> unit

    val of_string : string -> t option

end

