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

module Unstaged   = Asn_unstaged

module Staged_old = Asn_staged_old

module Staged0 = Asn_staged0
