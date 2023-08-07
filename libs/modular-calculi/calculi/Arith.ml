open! Mycore
open Calculus.Interfaces

type%import bin_op = Arith.bin_op [@@deriving sexp]

(* TODO: ppx_import fails *)
(* module type S = [%import: (module Arith.S)] *)
module type S = sig
  include CalculusWithSubcalculi.S1

  module Constructors : sig
    val int : int -> t
    val bin_op : bin_op -> t -> t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mult : t -> t -> t
    val div : t -> t -> t
    val if_zero : t -> t -> t -> t
  end
end

module Extend (Subcalculus : Calculus) = struct
  module Subcalculus = Subcalculus

  module Type = struct
    type t =
      | Int of int
      | BinOp of bin_op * t * t
      | IfZero of t * t * t
      | OfSubcalculus of Subcalculus.t
    [@@deriving sexp_of]

    let t_of_sexp _ = failwith "TODO t_of_sexp"

    let uid : t CalculusUid.t =
      CalculusUid.create_from_functor
        ~functor_name:"Arith"
        ~arg_names:[ CalculusUid.name Subcalculus.uid ]
        sexp_of_t
    ;;
  end

  include Type

  module Constructors = struct
    let int i = Int i
    let bin_op op t1 t2 = BinOp (op, t1, t2)
    let add t1 t2 = bin_op Add t1 t2
    let sub t1 t2 = bin_op Sub t1 t2
    let mult t1 t2 = bin_op Mult t1 t2
    let div t1 t2 = bin_op Div t1 t2
    let if_zero t1 t2 t3 = IfZero (t1, t2, t3)
  end

  open Constructors

  let of_subcalculus t = OfSubcalculus t

  module Parser = struct
    open Angstrom
    open Parsing_utils

    let add = char '+' *> return add
    let sub = char '-' *> return sub
    let mul = char '*' *> return mult
    let div = char '/' *> return div

    let expr =
      (* TODO handle spaces *)
      fix (fun expr ->
        let atom = in_parens expr <|> (integer >>| int) in
        let product = chainl1 atom (mul <|> div) in
        let sum = chainl1 product (add <|> sub) in
        let subcalculus = Subcalculus.parser >>| of_subcalculus in
        sum <|> subcalculus)
    ;;
  end

  let parser = Parser.expr

  include (
    Calculus__Utils.MapSubterms_of_MapSubtermsMonom.Of1 (struct
      include Type
      module Subcalculus = Subcalculus

      let map_direct_subterms_monom ~f ~f_subcalculus t =
        match t with
        | Int _ -> t
        | BinOp (op, t1, t2) -> bin_op op (f t1) (f t2)
        | IfZero (t1, t2, t3) -> if_zero (f t1) (f t2) (f t3)
        | OfSubcalculus t0 -> of_subcalculus (f_subcalculus t0)
      ;;
    end) :
      sig
        include MapSubtermsMonom.S1 with type t := t and module Subcalculus := Subcalculus
        include MapSubterms with type t := t
      end)

  let of_subcalculus_coercion =
    Coercion.of_fun_unsafe ~source:Subcalculus.uid ~target:uid of_subcalculus
  ;;

  let is_value t =
    match t with
    | Int _ -> true
    | BinOp _ | IfZero _ -> false
    | OfSubcalculus t_in_subcalculus -> t_in_subcalculus |> Subcalculus.is_value
  ;;

  let eval_bin_op op i1 i2 =
    match op with
    | Add -> i1 + i2
    | Sub -> i1 - i2
    | Mult -> i1 * i2
    | Div -> i1 / i2
  ;;

  let rec eval (t : t) : t =
    match t with
    | Int i -> Int i
    | BinOp (op, t1, t2) ->
      let t1' = eval t1 in
      let t2' = eval t2 in
      (match t1', t2' with
       | Int i1, Int i2 -> Int (eval_bin_op op i1 i2)
       | _ -> failwith "Error!")
    | IfZero (t1, t2, t3) ->
      (match eval t1 with
       | Int 0 -> eval t2
       | Int _ -> eval t3
       | _ -> failwith "Error!")
    | OfSubcalculus t0 -> t0 |> Subcalculus.eval |> of_subcalculus
  ;;

  let to_coerced t =
    match t with
    | OfSubcalculus t_in_subcalculus ->
      t_in_subcalculus
      |> Subcalculus.to_coerced
      |> Coerced.coerce ~f:of_subcalculus_coercion
    | Int _ | BinOp _ | IfZero _ -> Coerced.wrap ~calculus:uid t
  ;;

  let eval_to_coerced t =
    match t with
    | OfSubcalculus t_in_subcalculus ->
      t_in_subcalculus
      |> Subcalculus.eval_to_coerced
      |> Coerced.coerce ~f:of_subcalculus_coercion
    | Int _ | BinOp _ | IfZero _ -> eval t |> Coerced.wrap ~calculus:uid
  ;;
end

module Inextensible = Extend (Empty)
include Inextensible
open Calculus__Utils.Test (Inextensible)

let%expect_test "(100+20)+3" =
  test_parse_eval "(100+20)+3";
  [%expect
    {|
    (100+20)+3
    (BinOp Add(BinOp Add(Int 100)(Int 20))(Int 3))
    (Int 123) |}]
;;

let%expect_test "(2+3)*4+5*6" =
  test_parse_eval "(2+3)*4+5*6";
  [%expect
    {|
    (2+3)*4+5*6
    (BinOp Add(BinOp Mult(BinOp Add(Int 2)(Int 3))(Int 4))(BinOp Mult(Int 5)(Int 6)))
    (Int 50) |}]
;;
