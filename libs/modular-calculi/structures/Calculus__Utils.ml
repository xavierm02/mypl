open! Mycore
open Calculus_intf

module MapSubterms_of_MapSubtermsMonom = struct
  module Of1 (M : sig
      include CalculusTypeAndUid
      module Subcalculus : MapSubterms
      include MapSubtermsMonom.S1 with type t := t and module Subcalculus := Subcalculus
    end) =
  struct
    include M

    let map_direct_subterms ~(f : PolymFun.t) t =
      M.map_direct_subterms_monom
        ~f:(PolymFun.apply ~f ~typ:M.uid)
        ~f_subcalculus:(PolymFun.apply ~f ~typ:Subcalculus.uid)
        t
    ;;

    let rec map_maximal_subterms_in_calculus
      : type a. calculus:a CalculusUid.t -> f:(a -> a) -> t -> t
      =
      fun ~calculus ~f t ->
      match CalculusUid.same_witness calculus M.uid with
      | Some T -> f t
      | None ->
        M.map_direct_subterms_monom
          ~f:(map_maximal_subterms_in_calculus ~calculus ~f)
          ~f_subcalculus:(Subcalculus.map_maximal_subterms_in_calculus ~calculus ~f)
          t
    ;;
  end
end

module Test (Calculus : Calculus) = struct
  type t = Calculus.t

  open Fn

  let parse = Parsing_utils.parse_of_parser Calculus.parser
  let print t = t |> Calculus.sexp_of_t |> Sexp.to_string |> print_endline
  let test_parse str = str |> tap print_endline |> parse |> print

  let test_parse_eval str =
    str |> tap print_endline |> parse |> tap print |> Calculus.eval |> print
  ;;
end
