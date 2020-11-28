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


type 'a t

module S : sig
  type 'a element

  val element  : 'a t -> 'a element

  type 'a sequence

  val sequence : 'a sequence -> 'a t

  val set      : 'a sequence -> 'a t

  val choice   : 'a t -> 'b t -> ('a, 'b) Asn_core.sum t

  val bool     : bool t

  val integer  : int64 t

end


type encoding

val ber : encoding

val der : encoding


type 'a codec

val codec : encoding -> 'a t -> 'a codec

val encode : 'a codec -> 'a -> bytes

type error = [ `Parse of string ]

val decode : 'a codec -> bytes -> ('a * bytes, error) result

