open! Mycore
open Calculus.Interfaces

module type S = sig
  type t = private | [@@deriving sexp_of]

  include Calculus with type t := t
end

module Inextensible : S
include module type of Inextensible
