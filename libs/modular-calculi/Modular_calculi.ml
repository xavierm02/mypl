module Interfaces = struct
  include Calculus.Interfaces
  include Morphism.Interfaces
  include Coercion.Interfaces
  include Coerced.Interfaces
end

module Calculus = Calculus
module Morphism = Morphism
module Coercion = Coercion
module Coerced = Coerced
