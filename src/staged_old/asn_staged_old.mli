type encoding

val ber : encoding

val der : encoding


type 'a codec

val codec : encoding -> 'a Asn_core.asn -> 'a codec
type error = [ `Parse of string ]

val decode : 'a codec -> bytes -> ('a * bytes, error) result

val stage_decoder : 'a codec -> string -> unit