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
  mk_encoder : 'a. 'a t -> 'a -> bytes
}