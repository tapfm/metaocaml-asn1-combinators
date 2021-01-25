(*Adapted from https://raw.githubusercontent.com/mirleft/ocaml-asn1-combinators/master/tests/x509.ml*)
open Asn.S
module OID = Asn_oid

type tBSCertificate = {
  version    : [ `V1 | `V2 | `V3 ] ;
  serial     : int ;
  signature  : OID.t ;
  issuer     : (OID.t * string) list list ;
  validity   : bytes * bytes ;
  subject    : (OID.t * string) list list ;
  pk_info    : OID.t * bytes ;
  issuer_id  : bytes option ;
  subject_id : bytes option ;
  extensions : (OID.t * bool * bytes) list option
}

type certificate = {
  tbs_cert       : tBSCertificate ;
  signature_algo : OID.t ;
  signature      : bytes
}

let def  x = function None -> x | Some y -> y

let def' x = fun y -> if y = x then None else Some y

let extensions =
  let extension =
    sequence (
        (required ~label:"id"       oid) @
        (optional ~label:"critical" bool) -@
        (required ~label:"value"    octet_string) )
  in
  sequence_of extension

let directory_name =
  let (|||) = choice in 
    printable_string ||| 
    utf8_string ||| 
    teletex_string |||
    universal_string |||
    bmp_string |||
    ia5_string

let name =
  let attribute_tv =
  sequence (
      (required ~label:"attr type"  oid) -@
      (* This is ANY according to rfc5280. *)
      (required ~label:"attr value" directory_name)) in
  let rd_name      = set_of attribute_tv in
  let rdn_sequence = sequence_of rd_name in
  rdn_sequence (* A vacuous choice, in the standard. *)

let algorithmIdentifier =
  sequence (
    (required ~label:"algorithm" oid) -@
    (* This is ANY according to rfc5280 *)
    (optional ~label:"params"    null))

let version =
  integer

let certificateSerialNumber = integer

let time =
  (choice utc_time generalized_time)

let validity =
  sequence (
    (required ~label:"not before" time) -@
    (required ~label:"not after"  time))
    

let subjectPublicKeyInfo =
  sequence (
    (required ~label:"algorithm" algorithmIdentifier) -@
    (required ~label:"subjectPK" bit_string) )

let uniqueIdentifier = bit_string

let tBSCertificate =
  sequence @@
      (optional ~label:"version"       @@ explicit 0 version) (* default v1 *)
    @ (required ~label:"serialNumber"  @@ certificateSerialNumber)
    @ (required ~label:"signature"     @@ algorithmIdentifier)
    @ (required ~label:"issuer"        @@ name)
    @ (required ~label:"validity"      @@ validity)
    @ (required ~label:"subject"       @@ name)
    @ (required ~label:"subjectPKInfo" @@ subjectPublicKeyInfo)
      (* if present, version is v2 or v3 *)
    @ (optional ~label:"issuerUID"     @@ implicit 1 uniqueIdentifier)
      (* if present, version is v2 or v3 *)
    @ (optional ~label:"subjectUID"    @@ implicit 2 uniqueIdentifier)
      (* v3 if present *)
   -@ (optional ~label:"extensions"    @@ explicit 3 extensions)

let certificate =
  sequence (
    (required ~label:"tbsCertificate"     tBSCertificate) @
    (required ~label:"signatureAlgorithm" algorithmIdentifier) -@
    (required ~label:"signatureValue"     bit_string))