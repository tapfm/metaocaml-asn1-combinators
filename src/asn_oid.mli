type t = private Oid of int * int * int list

val compare     : t -> t -> int
val equal       : t -> t -> bool

val base        : int -> int -> t
val base_opt    : int -> int -> t option
val (<|)        : t -> int -> t
val (<||)       : t -> int list -> t

val to_nodes    : t -> int * int * int list
val of_nodes    : int -> int -> int list -> t option
val pp          : Format.formatter -> t -> unit
val of_string   : string -> t option
