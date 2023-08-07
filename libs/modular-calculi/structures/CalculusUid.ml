open! Mycore

type 'a t = 'a Type_equal.Id.t [@@deriving sexp_of]

let create = Type_equal.Id.create

let create_from_functor ~functor_name ~arg_names sexp_of_t =
  let name = functor_name ^ "(" ^ String.concat ~sep:", " arg_names ^ ")" in
  Type_equal.Id.create ~name sexp_of_t
;;

let same_witness = Type_equal.Id.same_witness
let name = Type_equal.Id.name
