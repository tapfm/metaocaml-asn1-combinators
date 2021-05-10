let protocol = 
  let open Asn.S in
  sequence @@ single @@ required @@
  sequence @@ single @@ required @@
  sequence (
    (required @@ sequence (
      (required @@ sequence (
        (required @@ integer) -@
        (required @@ sequence (
          (required @@ integer) -@
          (required @@ integer)
        ))
      )) -@
      (required @@ integer)
    )) @

    (required @@ 
      sequence @@ single @@ required @@ 
        sequence @@ single @@ required @@ 
        sequence @@ single @@ required @@ 
          integer
    ) @

    (required @@ integer) -@

    (required @@ sequence (
      (required integer) -@
      (required integer)
    ))
  )

let protocol_core = 
  let open Asn_combinators in
  sequence @@ single @@ required @@
  sequence @@ single @@ required @@
  sequence (
    (required @@ sequence (
      (required @@ sequence (
        (required @@ integer) -@
        (required @@ sequence (
          (required @@ integer) -@
          (required @@ integer)
        ))
      )) -@
      (required @@ integer)
    )) @

    (required @@ 
      sequence @@ single @@ required @@ 
        sequence @@ single @@ required @@ 
        sequence @@ single @@ required @@ 
          integer
    ) @

    (required @@ integer) -@

    (required @@ sequence (
      (required integer) -@
      (required integer)
    ))
  )

let () =
  let codec = Asn.Staged.codec Asn.Staged.ber protocol in 
  Asn.Staged.stage_decoder codec "tmp/hnis_staged.ml";
  let codec_old = Asn.Staged_old.codec Asn.Staged_old.ber protocol in
  Asn.Staged_old.stage_decoder codec_old "tmp/hnis_staged_old.ml"