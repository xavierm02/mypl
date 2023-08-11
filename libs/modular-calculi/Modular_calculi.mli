(** The {!Interfaces} module gathers all the interfaces, and is meant to be opened. *)
module Interfaces : sig
  (** {1 Calculi} *)

  include module type of Calculus.Interfaces

  (** {1 Morphisms} *)

  include module type of Morphism.Interfaces

  (** {1 Coercions} *)

  include module type of Coercion.Interfaces

  (** {1 Coerced terms} *)

  include module type of Coerced.Interfaces
end

module Combinators : sig
  module Empty = Empty
end

module Examples : sig
  module Arith = Arith
end

module Calculus = Calculus
module Morphism = Morphism
module Coercion = Coercion
module Coerced = Coerced

(** The goal of this Modular_calculi is to TODO *)

(** Interfaces relevant to defining modular calculi *)

(** qwwe *)

module Parsing_utils = Parsing_utils