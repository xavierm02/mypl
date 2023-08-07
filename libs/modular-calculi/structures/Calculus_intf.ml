open! Mycore

module type Parsable = sig
  type t

  val parser : t Angstrom.t
end

module type Evaluable = sig
  type t

  val is_value : t -> bool
  val eval : t -> t

  (** Equations:
      - [t |> eval |> is_value = true], whenever [eval t] terminates
      - [eval t = t] whenever [is_value t = true] *)
end

module type CalculusTypeAndUid = sig
  type t

  (** TODO explain *)
  val uid : t CalculusUid.t
end

module type MapSubterms = sig
  include CalculusTypeAndUid

  (** [map_direct_subterms ~f t] replaces each direct subterm [t0 : Calc0.t] by [f ~typ:Calc0.typ t0] in [t]. *)
  val map_direct_subterms : f:PolymFun.t -> t -> t

  (** [map_maximal_subterms_in_calculus ~calculus ~f t] replaces each subterm [t0 : Calc0.t] that lives in the calculus [Calc0] whose [uid] is [target] by [f t0] *)
  val map_maximal_subterms_in_calculus
    :  calculus:'a CalculusUid.t
    -> f:('a -> 'a)
    -> t
    -> t
end

module type Calculus = sig
  type t

  include Sexpable.S with type t := t (** @closed *)

  include Parsable with type t := t
  include Evaluable with type t := t
  include MapSubterms with type t := t

  (** [to_coerced t] returns [u : t Coerced.t]  such that [u |> Coerced.unwrap = t] and the top-level constructor of [u] is *)
  val to_coerced : t -> t Coerced.t

  (** - [t |> eval_to_coerced = t |> eval |> to_coerced] *)
  val eval_to_coerced : t -> t Coerced.t
end

module MapSubtermsMonom = struct
  module type S0 = sig
    type t

    val map_direct_subterms_monom : f:(t -> t) -> t -> t
  end

  module type S1 = sig
    type t

    module Subcalculus : T

    val map_direct_subterms_monom
      :  f:(t -> t)
      -> f_subcalculus:(Subcalculus.t -> Subcalculus.t)
      -> t
      -> t
  end

  module type S2 = sig
    type t

    module Subcalculus1 : T
    module Subcalculus2 : T

    val map_direct_subterms_monom
      :  f:(t -> t)
      -> f_subcalculus1:(Subcalculus1.t -> Subcalculus1.t)
      -> f_subcalculus2:(Subcalculus2.t -> Subcalculus2.t)
      -> t
      -> t
  end
end

module CalculusWithSubcalculi = struct
  module type S0 = sig
    include Calculus
    include MapSubtermsMonom.S0 with type t := t
  end

  module type S1 = sig
    include Calculus
    include MapSubtermsMonom.S1 with type t := t

    val of_subcalculus : Subcalculus.t -> t
    val of_subcalculus_coercion : (Subcalculus.t, t) Coercion.t
  end

  module type S2 = sig
    include Calculus
    include MapSubtermsMonom.S2 with type t := t

    val of_subcalculus1 : Subcalculus1.t -> t
    val of_subcalculus1_coercion : (Subcalculus1.t, t) Coercion.t
    val of_subcalculus2 : Subcalculus2.t -> t
    val of_subcalculus2_coercion : (Subcalculus2.t, t) Coercion.t
  end
end
