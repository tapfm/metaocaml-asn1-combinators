type encoding

val ber : encoding

val der : encoding


type 'a codec

val codec : encoding -> 'a Asn_core.asn -> 'a codec

val encode : 'a codec -> 'a -> bytes

type error = [ `Parse of string ]

val decode : 'a codec -> bytes -> ('a * bytes, error) result