open! Mycore
open Morphism_intf

module Make (Generator : ConcreteCategory) = struct
  type ('a, 'b) generator = ('a, 'b) Generator.t

  type ('a, 'b) t =
    | Id : 'a CalculusUid.t -> ('a, 'a) t
    | PostCompose :
        { source_to_midpoint : ('a, 'b) t
        ; midpoint_to_target : ('b, 'c) generator
        }
        -> ('a, 'c) t

  let sexp_of_t _ = failwith "TODO"
  let postcompose g f = PostCompose { source_to_midpoint = f; midpoint_to_target = g }
  let of_generator f = postcompose f (Id (Generator.source f))

  let rec source : type a b. (a, b) t -> a CalculusUid.t =
    fun t ->
    match t with
    | Id src -> src
    | PostCompose { source_to_midpoint; _ } -> source source_to_midpoint
  ;;

  let target : type a b. (a, b) t -> b CalculusUid.t =
    fun t ->
    match t with
    | Id tgt -> tgt
    | PostCompose { midpoint_to_target; _ } -> Generator.target midpoint_to_target
  ;;

  let rec length : type a b. (a, b) t -> int =
    fun t ->
    match t with
    | Id _ -> 0
    | PostCompose { source_to_midpoint; _ } -> length source_to_midpoint
  ;;

  let rec apply : type a b. (a, b) t -> a -> b =
    fun f x ->
    match f with
    | Id _ -> x
    | PostCompose { source_to_midpoint = f1; midpoint_to_target = f2 } ->
      x |> apply f1 |> Generator.apply f2
  ;;

  let of_fun_unsafe ~source ~target f =
    f |> Generator.of_fun_unsafe ~source ~target |> of_generator
  ;;

  let id typ = Id typ

  let rec compose : type a b c. (b, c) t -> (a, b) t -> (a, c) t =
    fun g f ->
    match g with
    | Id _ -> f
    | PostCompose { source_to_midpoint = g1; midpoint_to_target = g2 } ->
      PostCompose { source_to_midpoint = compose g1 f; midpoint_to_target = g2 }
  ;;

  let rec decompose_non_trivial
    : type a b c. midpoint:b CalculusUid.t -> (a, c) t -> ((b, c) t * (a, b) t) option
    =
    fun ~midpoint f ->
    match f with
    | Id _ -> None
    | PostCompose { source_to_midpoint = f1; midpoint_to_target = f2 } ->
      (match CalculusUid.same_witness midpoint (Generator.source f2) with
       | Some T -> Some (of_generator f2, f1)
       | None ->
         decompose_non_trivial ~midpoint f1
         |> Option.map ~f:(fun (f1b, f1a) -> postcompose f2 f1b, f1a))
  ;;

  let map_none_to_eval_thunk ~f x =
    match x with
    | Some _ -> x
    | None -> f ()
  ;;

  let decompose
    : type a b c. midpoint:b CalculusUid.t -> (a, c) t -> ((b, c) t * (a, b) t) option
    =
    fun ~midpoint f ->
    let attempt1 () = decompose_non_trivial ~midpoint f in
    let attempt2 () : ((b, c) t * (a, b) t) option =
      match CalculusUid.same_witness (target f) midpoint with
      | Some T -> Some (Id midpoint, f)
      | None -> None
    in
    let attempt3 () : ((b, c) t * (a, b) t) option =
      match CalculusUid.same_witness midpoint (source f) with
      | Some T -> Some (f, Id midpoint)
      | None -> None
    in
    attempt1 ()
    |> map_none_to_eval_thunk ~f:attempt2
    |> map_none_to_eval_thunk ~f:attempt3
  ;;
end
