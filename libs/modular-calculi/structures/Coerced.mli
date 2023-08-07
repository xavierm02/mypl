module Interfaces = Coerced_intf
open Interfaces

module Naive : sig
  module Make : Coerced_of_Coercion
  include module type of Make (Coercion)
end

include module type of Naive.Make (Coercion)
