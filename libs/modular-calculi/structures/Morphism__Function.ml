open! Mycore

type ('a, 'b) t =
  { source : 'a CalculusUid.t
  ; target : 'b CalculusUid.t
  ; apply : 'a -> 'b
  }
[@@deriving sexp_of]

let source f = f.source
let target f = f.target
let apply f t = f.apply t
let id typ = { source = typ; target = typ; apply = Fn.id }

let compose g f =
  { source = f.source; target = g.target; apply = Fn.compose g.apply f.apply }
;;

let of_fun_unsafe ~source ~target f = { source; target; apply = f }
