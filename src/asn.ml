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
  mk_decoder : 'a. 'a t -> bytes -> 'a * bytes;
  mk_encoder : 'a. 'a t -> 'a -> Asn_core.writer
}

let ber = {
  mk_decoder = (fun asn b -> Asn_reader.compile Asn_reader.Ber asn b);
  mk_encoder = (fun asn x -> Asn_writer.to_writer Asn_writer.Ber asn x) ;
}

let der = {
  mk_decoder = (fun asn b -> Asn_reader.compile Asn_reader.Der asn b);
  mk_encoder = (fun asn x -> Asn_writer.to_writer Asn_writer.Der asn x);
}

type 'a codec = Codec of (bytes -> ('a * bytes)) * ('a -> Core.writer)

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
