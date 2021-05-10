open Asn_combinators  
module OID = Asn_oid

let messageID = integer

let ldapString = octet_string

let ldapn = ldapString

let uri = ldapString

let referral = sequence_of uri

let saslCredentials = sequence (
  (required ldapString) -@
  (optional octet_string)
)

let authenticationChoice = choice
  (explicit 0 @@ octet_string)
  (explicit 3 @@ saslCredentials)


let bind_request = explicit ~cls:`Application 0 @@ sequence (
  (required integer) @
  (required ldapn) -@
  (required authenticationChoice)
)

let ldapResult = sequence (
  (required integer) @
  (required ldapn) @
  (required ldapString) -@
  (optional referral)
)

let bind_response = explicit ~cls:`Application 1 @@ sequence (
  (*COMPONENTS OF ldapResult*)
  (required integer) @
  (required ldapn) @
  (required ldapString) @
  (optional referral) -@
  (optional octet_string)
)

let unbindRequest = explicit ~cls:`Application 2 null

let attributeDescription = ldapString

let attributeValue = octet_string

let assertionValue = octet_string

let attributeValueAssertion = sequence (
  (required attributeDescription) -@
  (required assertionValue)
)

let substringFilter = sequence (
  (required attributeDescription) -@
  (required @@ sequence_of (choice
    (explicit 0 assertionValue) @@ choice
    (explicit 1 assertionValue)
    (explicit 2 assertionValue)))
)

let matchingRuleId = ldapString

let matchingRuleAssertion = sequence (
  (optional @@ explicit 1 @@ matchingRuleId) @
  (optional @@ explicit 2 @@ attributeDescription) @
  (required @@ explicit 3 @@ assertionValue) -@
  (required @@ explicit 4 @@ bool)
)

let rec filter = choice
    (explicit 0 @@ set_of octet_string)     @@ choice
    (explicit 1 @@ set_of octet_string)     @@ choice
    (explicit 2 @@ octet_string)            @@ choice
    (explicit 3 @@ attributeValueAssertion) @@ choice
    (explicit 4 @@ substringFilter)         @@ choice
    (explicit 5 @@ attributeValueAssertion) @@ choice
    (explicit 6 @@ attributeValueAssertion) @@ choice
    (explicit 7 @@ attributeDescription)    @@ choice
    (explicit 8 @@ attributeValueAssertion)
    (explicit 9 @@ matchingRuleAssertion)


let attributeSelection = sequence_of ldapString
  

let searchRequest = explicit ~cls:`Application 3 @@ sequence (
  (required @@ ldapn) @
  (required @@ integer) @
  (required @@ integer) @
  (required @@ integer) @
  (required @@ integer) @
  (required @@ bool)    @
  (required @@ filter) -@
  (required @@ attributeSelection)
)

let partialAttribute = sequence (
  (required attributeDescription) -@
  (required assertionValue)
)

let attribute = partialAttribute

let partialAttributeList = sequence_of partialAttribute

let searchResultEntry = explicit ~cls:`Application 4 @@ sequence (
  (required ldapn) -@
  (required partialAttributeList)
)

let searchResultDone = explicit ~cls:`Application 5 ldapResult

let searchResultReference = explicit ~cls:`Application 19 @@ sequence_of @@ uri

let modifyRequest = explicit ~cls:`Application 6 @@ sequence (
  (required @@ ldapn) -@
  (required @@ sequence_of (
    sequence (
      (required integer) -@
      (required partialAttribute)
    )
  ))
)

let modifyResponse = explicit ~cls:`Application 7 ldapResult

let attributeList = sequence_of attribute

let addRequest = explicit ~cls:`Application 8 @@ sequence (
  (required ldapn) -@
  (required attributeList)
)

let addResponse = explicit ~cls:`Application 9 @@ ldapResult

let delRequest = explicit ~cls:`Application 10 @@ ldapn

let delResponse = explicit ~cls:`Application 11 @@ ldapResult

let relativeLDAPN = ldapString

let modifyDNRequest = explicit ~cls:`Application 12 @@ sequence (
  (required @@ ldapn) @
  (required @@ relativeLDAPN) @
  (required @@ bool) -@
  (optional @@ explicit 0 @@ ldapn)
)

let modifyDNResponse = explicit ~cls:`Application 13 @@ ldapResult

let compareRequest = explicit ~cls:`Application 14 @@ sequence (
  (required ldapn) -@
  (required attributeValueAssertion)
)

let compareResponse = explicit ~cls:`Application 15 @@ ldapResult

let abandonRequest = explicit ~cls:`Application 16 @@ messageID

let ldapOID = octet_string

let extendedRequest = explicit ~cls:`Application 23 @@ sequence (
  (required @@ explicit 0 @@ ldapOID) -@
  (optional @@ explicit 1 @@ octet_string)
)

let extendedResponse = explicit ~cls:`Application 24 @@ sequence (
  (*COMPONENTS OF ldapResult*)
  (required integer) @
  (required ldapn) @
  (required ldapString) @
  (optional referral) @
  (optional @@ explicit 10 @@ ldapOID) -@
  (optional @@ explicit 11 @@ octet_string)
)

let intermediateResponse = explicit ~cls:`Application 25 @@ sequence (
  (optional @@ explicit 0 @@ ldapOID) -@
  (optional @@ explicit 1 @@ octet_string)
)


let ldapMessage = sequence (
  (required messageID) -@
  (required @@ choice
    bind_request          @@ choice
    bind_response         @@ choice
    unbindRequest         @@ choice
    searchRequest         @@ choice
    searchResultDone      @@ choice
    searchResultReference @@ choice
    modifyRequest         @@ choice
    modifyResponse        @@ choice
    addRequest            @@ choice
    addResponse           @@ choice
    delRequest            @@ choice
    delResponse           @@ choice
    modifyDNRequest       @@ choice
    modifyDNResponse      @@ choice
    compareRequest        @@ choice
    compareResponse       @@ choice
    abandonRequest        @@ choice
    extendedRequest       @@ choice
    extendedResponse
    intermediateResponse
  )
)