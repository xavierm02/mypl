open! Mycore
module Interfaces = Morphism_intf

(* TODO: can use arbitrary memory, not linearly bounded by the number of values of type Type.t *)
module Naive = struct
  include Coercion__Naive
  include Make (Morphism.Sequence)
end

include Naive.Make (Morphism.Sequence)
