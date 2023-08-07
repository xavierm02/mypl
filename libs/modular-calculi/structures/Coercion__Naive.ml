open! Mycore
open Morphism.Interfaces

module Make (Fun : GeneratedConcreteCategory) = struct
  type ('a, 'b) t = Coercion of ('a, 'b) Fun.t [@@unboxed] [@@deriving sexp_of]
  type ('a, 'b) generator = ('a, 'b) Fun.generator

  let of_generator f = Coercion (Fun.of_generator f)
  let source (Coercion f) = Fun.source f
  let target (Coercion f) = Fun.target f
  let id typ = Coercion (Fun.id typ)
  let of_fun_unsafe ~source ~target f = Coercion (Fun.of_fun_unsafe ~source ~target f)

  let apply : type a b. (a, b) t -> a -> b =
    fun (Coercion f) x ->
    match CalculusUid.same_witness (Fun.source f) (Fun.target f) with
    | Some T -> x
    | None -> Fun.apply f x
  ;;

  let compose : type a b c. (b, c) t -> (a, b) t -> (a, c) t =
    fun (Coercion g) (Coercion f) ->
    let source = Fun.source f in
    let target = Fun.target g in
    match CalculusUid.same_witness source target with
    | Some T -> Coercion (Fun.id source)
    | None -> Coercion (Fun.compose g f)
  ;;

  let lift_decompose decompose ~midpoint (Coercion f) =
    match decompose ~midpoint f with
    | Some (f1, f2) -> Some (Coercion f1, Coercion f2)
    | None -> None
  ;;

  let decompose ~midpoint f = lift_decompose Fun.decompose ~midpoint f

  let decompose_non_trivial ~midpoint f =
    lift_decompose Fun.decompose_non_trivial ~midpoint f
  ;;
end
