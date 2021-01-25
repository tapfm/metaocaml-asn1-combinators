module Core = Asn_core
module Reader = Asn_staged_reader
module Writer = Asn_staged_writer
open Asn_staged_core


type 'a t = 'a Core.asn

type encoding = {
  mk_decoder : 'a. 'a t -> bytes -> 'a * bytes;
  mk_stager  : 'a. 'a t -> string -> unit;
  mk_encoder : 'a. 'a t -> 'a -> writer
}

let ber = {
  mk_decoder = (fun asn b -> Reader.compile Ber asn b);
  mk_stager  = (fun asn s -> Reader.stage s Ber asn);
  mk_encoder = (fun asn x -> Writer.to_writer Ber asn x) ;
}

let der = {
  mk_decoder = (fun asn b -> Reader.compile Ber asn b);
  mk_stager = (fun asn s -> Reader.stage s Der asn);
  mk_encoder = (fun asn x -> Writer.to_writer Der asn x);
}

type 'a codec = Codec of (bytes -> 'a * bytes) * (string -> unit) * ('a -> writer)

let codec {mk_decoder; mk_stager; mk_encoder} asn = 
  Codec (mk_decoder asn, mk_stager asn, mk_encoder asn)

let encode (Codec(_, _, enc)) a = 
  let (n, w) = (enc a) in
  let bs = Bytes.create n in
  w 0 bs;
  bs

type error = Core.error

let stage_decoder (Codec(_, stg, _)) s = 
  stg s

let decode (Codec(dec, _, _)) b = 
  Ok(dec b)