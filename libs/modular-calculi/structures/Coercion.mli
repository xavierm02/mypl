module Interfaces = Morphism_intf
open Interfaces

module Naive : sig
  module Make : functor (Fun : GeneratedConcreteCategory) ->
    GeneratedConcreteCategory with type ('a, 'b) generator = ('a, 'b) Fun.generator
end

include module type of Naive.Make (Morphism.Sequence)
