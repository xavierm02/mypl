include Core.Fn (** @closed *)

let tap f x =
  ignore (f x);
  x
;;
