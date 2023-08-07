open! Mycore

module type Category = sig
  (** A value [f : ('a, 'b) t] represent the function [(apply f) : 'a -> 'b] with ['a] and ['b] known at runtime, and the assurance that this function is a morphism with respect to the interface [Calculus], i.e. that it distributes over / commutes with all operations of the [Calculus] interface.
      For example, [f : Calc1.t -> Calc2.t] being a morphism implies that
      {[
        Calc2.eval (f x) = f (Calc1.eval x)
      ]}
      and
      {[
        Calc2.is_value (f t1) = Calc1.is_value t1
      ]} *)
  type ('a, 'b) t [@@deriving sexp_of]

  val source : ('a, 'b) t -> 'a CalculusUid.t
  val target : ('a, 'b) t -> 'b CalculusUid.t
  val id : 'a CalculusUid.t -> ('a, 'a) t
  val compose : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
end

module type ConcreteCategory = sig
  include Category

  val apply : ('a, 'b) t -> 'a -> 'b

  (** {%html: <img src="https://cdn.pixabay.com/photo/2012/04/12/22/25/warning-sign-30915_960_720.png" style="height: 2em;" /> %}
      This function as an [_unsafe] suffix to emphasize that {b the onus is on the caller to check that the function is indeed a morphism}. *)
  val of_fun_unsafe
    :  source:'a CalculusUid.t
    -> target:'b CalculusUid.t
    -> ('a -> 'b)
    -> ('a, 'b) t
end

module type DecomposableConcreteCategory = sig
  include ConcreteCategory

  (** [decompose ~midpoint f] returns [Some (f2, f1)] if there exists a decomposition [f = compose f2 f1] of [f], and [None] otherwise. *)
  val decompose
    :  midpoint:'b CalculusUid.t
    -> ('a, 'c) t
    -> (('b, 'c) t * ('a, 'b) t) option

  (** [decompose_non_trivial ~midpoint f] returns [decompose ~midpoint f] if [midpoint != source f] and [midpoint != target f], and [None] otherwise. *)
  val decompose_non_trivial
    :  midpoint:'b CalculusUid.t
    -> ('a, 'c) t
    -> (('b, 'c) t * ('a, 'b) t) option
end

module type GeneratedConcreteCategory = sig
  include DecomposableConcreteCategory

  type ('a, 'b) generator

  val of_generator : ('a, 'b) generator -> ('a, 'b) t
end
