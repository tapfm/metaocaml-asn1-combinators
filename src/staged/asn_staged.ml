module Core = Asn_core
module Reader = Asn_staged_reader
open Asn_staged_core


type 'a t = 'a Core.asn

type encoding = {
  mk_decoder : 'a. 'a t -> bytes -> 'a * bytes;
  mk_stager  : 'a. 'a t -> string -> unit
}

let ber = {
  mk_decoder = (fun asn b -> Reader.compile Ber asn b);
  mk_stager  = (fun asn s -> Reader.stage s Ber asn)
}

let der = {
  mk_decoder = (fun asn b -> Reader.compile Der asn b);
  mk_stager  = (fun asn s -> Reader.stage s Der asn)
}

type 'a codec = Codec of (bytes -> 'a * bytes) * (string -> unit)

let codec {mk_decoder; mk_stager} asn = 
  Codec (mk_decoder asn, mk_stager asn)


type error = Core.error

let stage_decoder (Codec(_, stg)) s = 
  stg s

let decode (Codec(dec, _)) b = 
  Ok(dec b)