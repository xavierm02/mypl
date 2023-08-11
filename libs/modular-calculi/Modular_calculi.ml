module Interfaces = struct
  include Calculus.Interfaces
  include Morphism.Interfaces
  include Coercion.Interfaces
  include Coerced.Interfaces
end

module Combinators = struct
  module Empty = Empty
end

module Examples = struct
  module Arith = Arith
end

module Calculus = Calculus
module Morphism = Morphism
module Coercion = Coercion
module Coerced = Coerced

module Parsing_utils = Parsing_utils