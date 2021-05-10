open Asn_combinators

let headerData = sequence (
  (required integer) @
  (required integer) @
  (required octet_string) -@
  (required integer)
)

let scopedPdu = sequence (
  (required octet_string) @
  (required octet_string) -@
  (required octet_string)
)

let scopedPduData = choice
  scopedPdu
  octet_string

let snmpV3Message = sequence (
  (required integer) @
  (required headerData) @
  (required octet_string) -@
  (required scopedPduData)
)