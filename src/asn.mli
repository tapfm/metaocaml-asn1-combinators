(*
    Copyright notice to go here
*)

(**

    Staged implementation of `ocaml-asn1-combinators'

    For Part II University of Cambridge project

*)

open Result

(** {1 Object Identifiers} *)

type oid
(** ASN.1 [OBJECT IDENTIFIER]. *)

module OID : sig

  type t = oid

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val base : int -> int -> t

  val (<|) : t -> int -> t

  val (<||) : t -> int list -> t

  val to_nodes : t -> int * int * int list

  val of_nodes : int -> int -> int list -> t option

  val pp : Format.formatter -> t -> unit

  val of_string : string -> t option

end


type 'a t

module S : sig

  type cls = [ `Universal | `Application | `Private ]

  val implicit     : ?cls:cls -> int -> 'a t -> 'a t

  val explicit     : ?cls:cls -> int -> 'a t -> 'a t

  type 'a element

  val optional     : ?label:string -> 'a t -> 'a option element

  val required     : ?label:string -> 'a t -> 'a element

  type 'a sequence

  val single       : 'a element -> 'a sequence

  val ( @ )        : 'a element -> 'b sequence -> ('a * 'b) sequence

  val (-@ )        : 'a element -> 'b element  -> ('a * 'b) sequence

  val sequence     : 'a sequence -> 'a t

  val sequence_of  : 'a t -> 'a list t

  val set          : 'a sequence -> 'a t

  val set_of       : 'a t -> 'a list t

  val choice       : 'a t -> 'b t -> ('a, 'b) Asn_core.sum t

  val bool         : bool t

  val integer      : Z.t t

  val bit_string   : bool array t

  val octet_string : bytes t

  val null         : unit t

  val real         : float t

  val oid          : oid t


  (*Types for time values -- currently just treated as string*)
  (*TODO: Check validity of time values &
  possibly add conversions to and from a time type*)
  val generalized_time : string t

  val utc_time         : string t

  (*Restricted Character String types*)

  val utf8_string      : string t
  val numeric_string   : string t
  val printable_string : string t
  val teletex_string   : string t
  val videotex_string  : string t
  val ia5_string       : string t
  val graphic_string   : string t
  val visible_string   : string t
  val general_string   : string t
  val universal_string : string t
  val bmp_string       : string t

end

module Unstaged : sig
  type encoding

  val ber : encoding

  val der : encoding


  type 'a codec

  val codec : encoding -> 'a t -> 'a codec

  val encode : 'a codec -> 'a -> bytes

  type error = [ `Parse of string ]

  val decode : 'a codec -> bytes -> ('a * bytes, error) result
end

module Staged : sig
  type encoding

  val ber : encoding

  val der : encoding

  type 'a codec

  val codec : encoding -> 'a t -> 'a codec

  val encode : 'a codec -> 'a -> bytes

  type error = [ `Parse of string ]

  val decode : 'a codec -> bytes -> ('a * bytes, error) result
end
