open Asn.S
module OID = Asn_oid

let int32 = integer

let uint32 = integer

let microseconds = integer

let kerberosString = ia5_string

let realm = kerberosString

let principalName = sequence (
  (required @@ explicit 0 int32) -@
  (required @@ explicit 1 @@ sequence_of kerberosString)
)

let kerberosTime = generalized_time

let hostAddress = sequence (
  (required @@ explicit 0 @@ int32) -@
  (required @@ explicit 1 @@ octet_string)
)

let hostAddresses = sequence_of hostAddress


let authorizationData = sequence_of @@ sequence (
  (required @@ explicit 0 @@ int32) -@
  (required @@ explicit 1 @@ octet_string)
)

let pa_data = sequence (
  (required @@ explicit 1 @@ int32) -@
  (required @@ explicit 2 @@ octet_string)
)

let kerberosFlags = bit_string

let encryptedData = sequence (
  (required @@ explicit 0 @@ int32) @
  (required @@ explicit 1 @@ uint32) -@
  (required @@ explicit 2 @@ octet_string)
)

let encryptionKey = sequence (
  (required @@ explicit 0 @@ int32) -@
  (required @@ explicit 1 @@ octet_string)
)

let checksum = sequence (
  (required @@ explicit 0 @@ int32) -@
  (required @@ explicit 1 @@ octet_string)
)

let ticket = explicit ~cls:`Application 1 @@ sequence (
  (required @@ explicit 0 @@ integer) @
  (required @@ explicit 1 @@ realm) @
  (required @@ explicit 2 @@ principalName) -@
  (required @@ explicit 3 @@ encryptedData)
)

let ticketFlags = kerberosFlags

let transitedEncoding = sequence (
  (required @@ explicit 0 @@ int32) -@
  (required @@ explicit 1 @@ octet_string)
)

let encTicketPart = explicit ~cls:`Application 3 @@ sequence (
  (required @@ explicit  0 @@ ticketFlags) @
  (required @@ explicit  1 @@ encryptionKey) @
  (required @@ explicit  2 @@ realm) @
  (required @@ explicit  3 @@ principalName) @
  (required @@ explicit  4 @@ transitedEncoding) @
  (required @@ explicit  5 @@ kerberosTime) @
  (optional @@ explicit  6 @@ kerberosTime) @
  (required @@ explicit  7 @@ kerberosTime) @
  (optional @@ explicit  8 @@ kerberosTime) @
  (optional @@ explicit  9 @@ hostAddresses) -@
  (optional @@ explicit 10 @@ authorizationData)
)

let kdcOptions = kerberosFlags

let kdc_req_body = sequence (
  (required @@ explicit 0  @@ kdcOptions) @
  (optional @@ explicit 1  @@ principalName) @
  (optional @@ explicit 2  @@ realm) @
  (optional @@ explicit 3  @@ principalName) @
  (optional @@ explicit 4  @@ kerberosTime) @
  (required @@ explicit 5  @@ kerberosTime) @
  (optional @@ explicit 6  @@ kerberosTime) @
  (required @@ explicit 7  @@ uint32) @
  (required @@ explicit 8  @@ sequence_of int32) @
  (optional @@ explicit 9  @@ hostAddresses) @
  (optional @@ explicit 10 @@ encryptedData) -@
  (optional @@ explicit 11 @@ sequence_of ticket)
)

let kdc_req = sequence (
  (required @@ explicit 1 @@ integer) @
  (required @@ explicit 2 @@ integer) @
  (optional @@ explicit 3 @@ sequence_of pa_data) -@
  (required @@ explicit 4 @@ kdc_req_body)
)

let as_req = explicit ~cls:`Application 11 kdc_req

let tgs_req = explicit ~cls:`Application 13 kdc_req

let kdc_rep = sequence (
  (required @@ explicit 0 @@ integer) @
  (required @@ explicit 1 @@ integer) @
  (optional @@ explicit 2 @@ sequence_of pa_data) @
  (required @@ explicit 3 @@ realm) @
  (required @@ explicit 4 @@ principalName) @
  (required @@ explicit 5 @@ ticket) -@
  (required @@ explicit 6 @@ encryptedData)
)

let as_rep = explicit ~cls:`Application 11 @@ kdc_rep

let tgs_rep = explicit ~cls:`Application 13 @@ kdc_rep

let lastReq = sequence_of @@ sequence (
  (required @@ explicit 0 @@ int32) -@
  (required @@ explicit 1 @@ kerberosTime)
)

let encKDCRepPart = sequence (
  (required @@ explicit  0 @@ encryptionKey) @
  (required @@ explicit  1 @@ lastReq) @
  (required @@ explicit  2 @@ uint32) @
  (optional @@ explicit  3 @@ kerberosTime) @
  (required @@ explicit  4 @@ ticketFlags) @
  (required @@ explicit  5 @@ kerberosTime) @
  (optional @@ explicit  6 @@ kerberosTime) @
  (required @@ explicit  7 @@ kerberosTime) @
  (optional @@ explicit  8 @@ kerberosTime) @
  (required @@ explicit  9 @@ realm) @
  (required @@ explicit 10 @@ principalName) -@
  (optional @@ explicit 11 @@ hostAddresses)
)

let encASRepPart = explicit ~cls:`Application 25 @@ encKDCRepPart

let encTGSRepPart = explicit ~cls:`Application 26 @@ encKDCRepPart

let apOptions = kerberosFlags

let ap_req = explicit ~cls:`Application 14 @@ sequence (
  (required @@ explicit 0 @@ integer) @
  (required @@ explicit 1 @@ integer) @
  (required @@ explicit 2 @@ apOptions) @
  (required @@ explicit 3 @@ ticket) -@
  (required @@ explicit 4 @@ encryptedData) 
)

let authenticator = explicit ~cls:`Application 2 @@ sequence (
  (required @@ explicit 0 @@ integer) @
  (required @@ explicit 1 @@ realm) @
  (required @@ explicit 2 @@ principalName) @
  (optional @@ explicit 3 @@ checksum) @
  (required @@ explicit 4 @@ microseconds) @
  (required @@ explicit 5 @@ kerberosTime) @
  (optional @@ explicit 6 @@ encryptionKey) @
  (optional @@ explicit 7 @@ uint32) -@
  (optional @@ explicit 8 @@ authorizationData)
)

let ap_rep = explicit ~cls:`Application 15 @@ sequence (
  (required @@ explicit 0 @@ integer) @
  (required @@ explicit 1 @@ integer) -@
  (required @@ explicit 2 @@ encryptedData)
)

let encAPRepPart = explicit ~cls:`Application 27 @@ sequence (
  (required @@ explicit 0 @@ kerberosTime) @
  (required @@ explicit 1 @@ microseconds) @
  (optional @@ explicit 2 @@ encryptionKey) -@
  (optional @@ explicit 3 @@ uint32)
)

let krb_Safe_Body = sequence (
  (required @@ explicit 0 @@ octet_string) @
  (optional @@ explicit 1 @@ kerberosTime) @
  (optional @@ explicit 2 @@ microseconds) @
  (optional @@ explicit 3 @@ uint32) @
  (required @@ explicit 4 @@ hostAddress) -@
  (optional @@ explicit 5 @@ hostAddress)
)

let krb_safe = explicit ~cls:`Application 20 @@ sequence (
  (required @@ explicit 0 @@ integer) @
  (required @@ explicit 1 @@ integer) @
  (required @@ explicit 2 @@ krb_Safe_Body) -@
  (required @@ explicit 3 @@ checksum)
)
let krb_priv = explicit ~cls:`Application 21 @@ sequence (
  (required @@ explicit 0 @@ integer) @
  (required @@ explicit 1 @@ integer) -@
  (required @@ explicit 3 @@ encryptedData)
)

let encKrbPrivPart = explicit ~cls:`Application 28 @@ sequence (
  (required @@ explicit 0 @@ octet_string) @
  (optional @@ explicit 1 @@ kerberosTime) @
  (optional @@ explicit 2 @@ microseconds) @
  (optional @@ explicit 3 @@ uint32) @
  (required @@ explicit 4 @@ hostAddress) -@
  (optional @@ explicit 5 @@ hostAddress)
)

let krb_cred = explicit ~cls:`Application 22 @@ sequence (
  (required @@ explicit 0 @@ integer) @
  (required @@ explicit 1 @@ integer) @
  (required @@ explicit 2 @@ sequence_of ticket) -@
  (required @@ explicit 3 @@ encryptedData)
)

let krbCredInfo = sequence (
  (required @@ explicit  0 @@ encryptionKey) @
  (optional @@ explicit  1 @@ realm) @
  (optional @@ explicit  2 @@ principalName) @
  (optional @@ explicit  3 @@ ticketFlags) @
  (optional @@ explicit  4 @@ kerberosTime) @
  (optional @@ explicit  5 @@ kerberosTime) @
  (optional @@ explicit  6 @@ kerberosTime) @
  (optional @@ explicit  7 @@ kerberosTime) @
  (optional @@ explicit  8 @@ realm) @
  (optional @@ explicit  9 @@ principalName) -@
  (optional @@ explicit 10 @@ hostAddresses)
)

let encKrbCredPart = explicit ~cls:`Application 29 @@ sequence (
  (required @@ explicit 0 @@ sequence_of krbCredInfo) @
  (optional @@ explicit 1 @@ uint32) @
  (optional @@ explicit 2 @@ kerberosTime) @
  (optional @@ explicit 3 @@ microseconds) @
  (optional @@ explicit 4 @@ hostAddress) -@
  (optional @@ explicit 5 @@ hostAddress)
)

let krb_error = explicit ~cls:`Application 30 @@ sequence (
  (required @@ explicit  0 @@ integer) @
  (required @@ explicit  1 @@ integer) @
  (optional @@ explicit  2 @@ kerberosTime) @
  (optional @@ explicit  3 @@ microseconds) @
  (required @@ explicit  4 @@ kerberosTime) @
  (required @@ explicit  5 @@ microseconds) @
  (required @@ explicit  6 @@ uint32) @
  (optional @@ explicit  7 @@ realm) @
  (optional @@ explicit  8 @@ principalName) @
  (required @@ explicit  9 @@ realm) @
  (required @@ explicit 10 @@ principalName) @
  (optional @@ explicit 11 @@ kerberosString) -@
  (optional @@ explicit 12 @@ octet_string)
)

let method_data = sequence_of pa_data

let typed_data = sequence_of @@ sequence (
  (required @@ explicit 0 @@ int32) -@
  (optional @@ explicit 1 @@ octet_string)
)

let pa_enc_timestamp = encryptedData

let pa_enc_ts_enc = sequence (
  (required @@ explicit 0 @@ kerberosTime) -@
  (optional @@ explicit 1 @@ microseconds)
)

let etype_info_entry = sequence (
  (required @@ explicit 0 @@ int32) -@
  (optional @@ explicit 1 @@ octet_string)
)

let etype_info = sequence_of etype_info_entry

let etype_info2_entry = sequence (
  (required @@ explicit 0 @@ int32) @ 
  (optional @@ explicit 1 @@ kerberosString) -@
  (optional @@ explicit 2 @@ octet_string)
)

let etype_info2 = sequence_of etype_info2_entry

let ad_if_relevant = authorizationData

let ad_kdcIssued = sequence (
  (required @@ explicit 0 @@ checksum) @
  (optional @@ explicit 1 @@ realm) @
  (optional @@ explicit 2 @@ principalName) -@
  (required @@ explicit 3 @@ authorizationData)
)

let ad_and_or = sequence (
  (required @@ explicit 0 @@ int32) -@
  (required @@ explicit 1 @@ authorizationData)
)

let ad_mandatory_for_kdc = authorizationData