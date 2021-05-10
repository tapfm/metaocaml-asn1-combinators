open Asn_core

let random_string lo hi = 
  String.init ((Random.int 20) + 1)
    (fun _ -> Char.chr (lo + Random.(int (hi -lo))))

let replicate n f a = 
  let rec loop acc n = 
    if n <= 0 then acc else loop (f a :: acc) (pred n) in
  loop [] n


let r_prim : type a. a prim -> a = function 
    | Bool -> Random.bool ()
    | Int -> Z.of_int (Random.int 256)
    | Bits -> Array.init ((Random.int 20) + 1) (fun _ -> Random.bool ())
    | Octets -> Bytes.of_string @@ random_string 0 256
    | Null -> ()
    | OID ->  let rec replicate_l n = 
                if n < 1 then [] else Random.(int 256) :: replicate_l (pred n) in 
              Random.(Asn_oid.(base (int 3) (int 40) <|| replicate_l (int 10) ))
    | CharString -> random_string 32 127
    | Real -> Random.float 100000.
    | Time -> random_string 32 127

let rec r_element : type a. ?full:bool -> a element -> a = fun ?full:(b=false) -> function
    | Required (_, asn) -> r_asn ~full:b asn
    | Optional (_, asn) -> if not b && Random.int 3 = 0 then None else Some (r_asn ~full:b asn)

and r_seq : type a. ?full:bool -> a sequence -> a = fun ?full:(b=false) -> function
    | Last e       -> r_element ~full:b e
    | Pair (e, es) -> (r_element ~full:b e, r_seq ~full:b es)

and r_seq_of : type a. ?full:bool -> a asn -> a list = fun ?full:(b=false) asn ->
  replicate Random.(int 10) (r_asn ~full:b) asn

and r_asn : type a. ?full:bool -> a asn -> a = fun ?full:(b=false) -> function
    | Sequence asns       -> r_seq ~full:b asns
    | Set asns            -> r_seq ~full:b asns
    | Sequence_of asn     -> r_seq_of ~full:b asn
    | Set_of asn          -> r_seq_of ~full:b asn
    | Choice (asn1, asn2) ->
      if Random.bool() then L (r_asn ~full:b asn1) else R (r_asn ~full:b asn2)
    | Implicit (_, asn)   -> r_asn ~full:b asn
    | Explicit (_, asn)   -> r_asn ~full:b asn
    | Prim p              -> r_prim p
