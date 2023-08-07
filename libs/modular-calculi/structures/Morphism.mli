module Interfaces = Morphism_intf
open Interfaces
module Function : ConcreteCategory
include module type of Function

module Sequence : sig
  module Make : functor (Generator : ConcreteCategory) ->
    GeneratedConcreteCategory with type ('a, 'b) generator = ('a, 'b) Generator.t

  include GeneratedConcreteCategory with type ('a, 'b) generator = ('a, 'b) Function.t
end
