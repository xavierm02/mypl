open! Mycore
open Morphism.Interfaces

module Make (Coercion : GeneratedConcreteCategory) = struct
  type ('a, 'b) coercion = ('a, 'b) Coercion.t

  type 'b t =
    | Coerced :
        { in_source : 'a
        ; coercion : ('a, 'b) Coercion.t
        }
        -> 'b t
  [@@deriving sexp_of]

  let wrap ~calculus t = Coerced { in_source = t; coercion = Coercion.id calculus }
  let unwrap (Coerced t) = t.in_source |> Coercion.apply t.coercion
  let coerce ~f (Coerced t) = Coerced { t with coercion = Coercion.compose f t.coercion }

  let map_in_source ~f (Coerced t) =
    Coerced
      { t with
        in_source = PolymFun.apply ~f ~typ:(Coercion.source t.coercion) t.in_source
      }
  ;;
end
