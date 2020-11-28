open Result

module OID  = Asn_oid
module Core = Asn_core

module S = struct
  type 'a t        = 'a Core.asn
  type 'a element  = 'a Core.element
  type 'a sequence = 'a Core.sequence
  include Asn_combinators
end

type oid = OID.t
type 'a t = 'a S.t

type encoding = {
  (*mk_decoder : 'a. 'a t -> bytes -> 'a * bytes;*)
  mk_encoder : 'a. 'a t -> 'a -> Asn_core.writer
}

let ber = {
  (*mk_decoder = assert false ;*)
  mk_encoder = Asn_writer.ber_to_writer ;
}

let der = {
  (*mk_decoder = assert false ;*)
  mk_encoder = Asn_writer.der_to_writer ;
}
(*Temporarily removing the decoder in order to test this *)

(*
type 'a codec = Codec of (bytes -> ('a * bytes)) * ('a -> Core.writer)

let codec {mk_encoder; mk_decoder} asn = 
  Codec (mk_decoder asn, mk_encoder asn)

let encode (Codec(_, enc)) a = 
  let (n, w) = (enc a) in
  let bs = Bytes.create n in
  w 0 bs;
  bs
*)

type 'a codec = Codec of ('a -> Core.writer)

let codec {mk_encoder(*; mk_decoder*)} asn = 
  Codec (mk_encoder asn)

let encode (Codec(enc)) a = 
  let (n, w) = (enc a) in
  let bs = Bytes.create n in
  w 0 bs;
  bs
