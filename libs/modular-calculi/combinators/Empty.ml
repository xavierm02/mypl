open! Mycore
open Calculus.Interfaces

(* TODO *)
(* module type S = [%import: (module Empty.S)] *)
module type S = sig
  type t = private | [@@deriving sexp_of]

  include Calculus with type t := t
end

module Inextensible = struct
  type t = | [@@deriving sexp]

  let absurd (t : t) =
    match t with
    | _ -> .
  ;;

  let uid : t CalculusUid.t = CalculusUid.create ~name:"Empty" absurd

  let parser =
    Angstrom.fail "The Empty calculus is empty!" (* TODO better error message *)
  ;;

  let is_value = absurd
  let eval = absurd
  let to_coerced = absurd
  let eval_to_coerced = absurd
  let map_direct_subterms ~f:_ = absurd
  let map_maximal_subterms_in_calculus ~calculus:_ ~f:_ = absurd
end

include Inextensible
