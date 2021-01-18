(* Adapted from the existing asn1-combinators library *)
(* length * (offset -> bytes -> unit)*)

module Core = Asn_core
module Reader = Asn_unstaged_reader
module Writer = Asn_unstaged_writer
open Asn_unstaged_core


type 'a t = 'a Core.asn

type encoding = {
  mk_decoder : 'a. 'a t -> bytes -> 'a * bytes;
  mk_encoder : 'a. 'a t -> 'a -> writer
}

let ber = {
  mk_decoder = (fun asn b -> Reader.compile Reader.Ber asn b);
  mk_encoder = (fun asn x -> Writer.to_writer Writer.Ber asn x) ;
}

let der = {
  mk_decoder = (fun asn b -> Reader.compile Reader.Der asn b);
  mk_encoder = (fun asn x -> Writer.to_writer Writer.Der asn x);
}

type 'a codec = Codec of (bytes -> ('a * bytes)) * ('a -> writer)

let codec {mk_encoder; mk_decoder} asn = 
  Codec (mk_decoder asn, mk_encoder asn)

let encode (Codec(_, enc)) a = 
  let (n, w) = (enc a) in
  let bs = Bytes.create n in
  w 0 bs;
  bs

type error = Core.error

let decode (Codec(dec, _)) b = 
  try Ok (dec b) with Core.Parse_error err -> Error err