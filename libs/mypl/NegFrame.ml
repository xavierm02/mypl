open! Mycore
open Modular_calculi
open Calculus.Interfaces

module FieldContent = struct
  type 'a t =
    | Lambda
    | Def of 'a
  [@@deriving sexp, map]

  let of_option o =
    match o with
    | Some t -> Def t
    | None -> Lambda
  ;;

  let map ~f t = map f t
end

(* TODO: ppx_import fails *)
(* module type S = [%import: (module NegFrame.S)] *)
module type S = sig
  include CalculusWithSubcalculi.S1

  module Constructors : sig
    val up : Nat.t -> t
    val frame : t FieldContent.t Id.Map.t -> t
    val proj : t -> Id.t -> t
    val join : t -> t -> t
  end
end

module Extend (Subcalculus : Calculus) = struct
  module Subcalculus = Subcalculus

  module Type = struct
    type t =
      (* TODO find a way to use ppx_import that removes the "private" keyword *)
      | Up of Nat.t
      | Frame of t FieldContent.t Id.Map.t
      | Proj of t * Id.t
      | Join of t * t
      | OfSubcalculus of Subcalculus.t
    [@@deriving sexp_of]

    let t_of_sexp _ = failwith "TODO"

    let uid : t Calculus.Uid.t =
      Calculus.Uid.create_from_functor
        ~functor_name:"Frame"
        ~arg_names:[ Calculus.Uid.name Subcalculus.uid ]
        sexp_of_t
    ;;
  end

  include Type

  module Constructors = struct
    let up n = Up n
    let frame x = Frame x
    let frame_of_alist_exn l = l |> Id.Map.of_alist_exn |> frame
    let proj t x = Proj (t, x)
    let join t1 t2 = Join (t1, t2)
  end

  open Constructors

  let of_subcalculus t = OfSubcalculus t

  module Parser = struct
    open Parsing_utils
    open Angstrom

    let id = take_while1 is_lowercase_letter
    let s = maybe_spaces

    let term =
      fix (fun term ->
        let field =
          lift2
            (fun x v -> x, FieldContent.of_option v)
            id
            (maybe (s *> char '=' *> s *> term))
        in
        let fields = sep_by (s *> char ';' *> s) field in
        let frame = lift frame_of_alist_exn (in_braces (s *> fields <* s)) in
        let subcalculus = Subcalculus.parser >>| of_subcalculus in
        subcalculus <|> frame)
    ;;
  end

  let parser = Parser.term
  let parse str = Parsing_utils.parse_of_parser parser str

  include (
    Calculus.Utils.MapSubterms_of_MapSubtermsMonom.Of1 (struct
      include Type
      module Subcalculus = Subcalculus

      let map_direct_subterms_monom ~f ~f_subcalculus t =
        match t with
        | Up _ -> t
        | Frame fr ->
          let f = FieldContent.map ~f in
          Frame (Map.map fr ~f)
        | Proj (t0, x) -> Proj (f t0, x)
        | Join (t1, t2) -> Join (f t1, f t2)
        | OfSubcalculus t0 -> OfSubcalculus (f_subcalculus t0)
      ;;
    end) :
      sig
        include MapSubtermsMonom.S1 with type t := t and module Subcalculus := Subcalculus
        include MapSubterms with type t := t
      end)

  (* let map_direct_subterms ~(f : PolymFun.t) t =
     map_direct_subterms_monom
     ~f:(PolymFun.apply ~f ~typ)
     ~f_subcalculus:(PolymFun.apply ~f ~typ:Subcalculus.typ)
     t
     ;;

     let rec map_maximal_subterms_of_typ : type a. typ:a Type.t -> f:(a -> a) -> t -> t =
     fun ~typ ~f t ->
     match Type.same_witness typ self_typ with
     | Some T -> f t
     | None ->
     map_direct_subterms_monom
     ~f:(map_maximal_subterms_of_typ ~typ ~f)
     ~f_subcalculus:(Subcalculus.map_maximal_subterms_of_typ ~typ ~f)
     t
     ;; *)

  let is_value t =
    match t with
    | Up _ | Frame _ -> true
    | Proj _ | Join _ -> false
    | OfSubcalculus t0 -> Subcalculus.is_value t0
  ;;

  let of_subcalculus_coercion =
    Coercion.of_fun_unsafe ~source:Subcalculus.uid ~target:uid of_subcalculus
  ;;

  let to_coerced t =
    match t with
    | _ -> failwith "TODO"
  ;;

  let rec subst s t =
    (* TODO rewrite with map_direct_subterms_monom ? *)
    match t with
    | Up (Nat n) -> List.nth s n |> Option.value ~default:t
    | Frame fr ->
      let f = FieldContent.map ~f:(subst (t :: s)) in
      Frame (Map.map fr ~f)
    | Proj (t0, x) -> Proj (subst s t0, x)
    | Join (t1, t2) -> Join (subst s t1, subst s t2)
    | OfSubcalculus t0 ->
      OfSubcalculus
        (Subcalculus.map_maximal_subterms_in_calculus ~calculus:uid ~f:(subst s) t0)

  and eval t =
    match t with
    | Up _ | Frame _ -> t
    | Proj (t0, x) ->
      let t0' = eval t0 in
      let fr =
        match t0' with
        | Frame fr_ -> fr_
        | _ -> failwith "Error"
      in
      let u =
        match Map.find fr x with
        | None | Some FieldContent.Lambda -> failwith "Error"
        | Some (FieldContent.Def u_) -> u_
      in
      u |> subst [ Frame fr ] |> eval
    | Join (t1, t2) ->
      let t1' = eval t1 in
      let t2' = eval t2 in
      let fr =
        match t1', t2' with
        | Frame fr1, Frame fr2 -> failwith "TODO"
        | _, _ -> failwith "Error"
      in
      Frame fr
    | OfSubcalculus t0 -> t0 |> Subcalculus.eval |> of_subcalculus
  ;;

  let eval_to_coerced t =
    match t with
    | OfSubcalculus t0 ->
      t0 |> Subcalculus.eval_to_coerced |> Coerced.coerce ~f:(failwith "TODO")
    | Up _ | Frame _ | Proj _ | Join _ -> t |> Coerced.wrap ~calculus:uid
  ;;
end

module Inextensible = Extend (Combinators.Empty)
include Inextensible

let%expect_test "{a = 1; b = {c = Up1.a}}.b.c = 1" =
  let open Extend (Examples.Arith.Inextensible) in
  let t =
    Proj
      ( Proj
          ( Constructors.frame_of_alist_exn
              [ "a", Def (OfSubcalculus (Examples.Arith.Inextensible.Constructors.int 1))
              ; "b", Def (Constructors.frame_of_alist_exn [ "c", Def (Proj (Up (Nat.of_int_exn 1), "a")) ])
              ]
          , "b" )
      , "c" )
  in
  eval t |> sexp_of_t |> print_s;
  [%expect {| (OfSubcalculus (Int 1)) |}]
;;
