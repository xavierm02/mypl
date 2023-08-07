type t = private Nat of int [@@unboxed] [@@deriving sexp]

exception Negative_integer

val of_int_exn : int -> t
val to_int : t -> int
