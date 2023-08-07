module Interfaces = Coerced_intf

(* TODO: cache the result of unwrap? *)
module Naive = struct
  include Coerced__Naive
  include Make (Coercion)
end

include Naive
