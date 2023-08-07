open! Mycore
module Interfaces = Morphism_intf
module Function = Morphism__Function
include Function

(* TODO: Some operations that could be O(1) or O(log n) are O(n) *)
module Sequence = struct
  include Morphism__Sequence
  include Make (Function)
end
