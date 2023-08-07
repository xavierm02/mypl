open! Core

type t = Nat of int [@@unboxed] [@@deriving sexp]

exception Negative_integer

let of_int_exn i = if i < 0 then raise Negative_integer else Nat i
let to_int (Nat i) = i
