open! Mycore
open Morphism.Interfaces

module type Coerced = sig
  type 'a t [@@deriving sexp_of]
  type ('a, 'b) coercion

  val wrap : calculus:'a CalculusUid.t -> 'a -> 'a t
  val coerce : f:('a, 'b) coercion -> 'a t -> 'b t
  val unwrap : 'a t -> 'a
  val map_in_source : f:PolymFun.t -> 'a t -> 'a t
end

module type Coerced_of_Coercion = functor (Coercion : GeneratedConcreteCategory) ->
  Coerced with type ('a, 'b) coercion = ('a, 'b) Coercion.t
