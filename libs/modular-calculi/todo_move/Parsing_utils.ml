open Angstrom

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lowercase_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let maybe_spaces = take_while is_space
let braced ~left ~right p = left *> p <* right
let braced_by_chars ~left ~right p = braced ~left:(char left) ~right:(char right) p
let in_parens p = braced_by_chars ~left:'(' ~right:')' p
let in_braces p = braced_by_chars ~left:'{' ~right:'}' p
let in_brackets p = braced_by_chars ~left:'[' ~right:']' p
let natural_number = take_while1 is_digit >>| int_of_string
let integer = take_while1 is_digit >>| int_of_string (* TODO allow negative *)
let maybe (p : 'a Angstrom.t) : 'a option Angstrom.t = option None (p >>| Option.some)

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

exception ParseError of string

let parse_of_parser parser str =
  match Angstrom.parse_string ~consume:All parser str with
  | Ok t -> t
  | Error err -> raise (ParseError err)
;;
