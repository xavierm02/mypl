open! Mycore
open Calculus.Interfaces

type bin_op =
  | Add
  | Sub
  | Mult
  | Div
[@@deriving sexp]

module type S = sig
  include CalculusWithSubcalculi.S1

  module Constructors : sig
    val int : int -> t
    val bin_op : bin_op -> t -> t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mult : t -> t -> t
    val div : t -> t -> t
    val if_zero : t -> t -> t -> t
  end
end

module Extend : functor (Subcalculus : Calculus) ->
  S with module Subcalculus = Subcalculus

module Inextensible : module type of Extend (Empty)
include module type of Inextensible
