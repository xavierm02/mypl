(* open! Mycore
open Modular_calculi
open Qwe__qwe
open Calculus_intf

module FieldContent : sig
  type 'a t = private
    | Lambda
    | Def of 'a
  [@@deriving sexp, map]

  val map : f:('a -> 'b) -> 'a t -> 'b t
end

module type S = sig
  include CalculusWithSubcalculi.S1

  module Constructors : sig
    val up : Nat.t -> t
    val frame : t FieldContent.t Id.Map.t -> t
    val proj : t -> Id.t -> t
    val join : t -> t -> t
  end
end

module Extend : functor (Subcalculus : Calculus) ->
  S with module Subcalculus := Subcalculus

module Inextensible : module type of Extend (Combinators.Empty)
include module type of Inextensible
 *)