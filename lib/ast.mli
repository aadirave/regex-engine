type regex_t = 
| Empty
| Char of char
| Concat of regex_t * regex_t
| Choice of regex_t * regex_t
| Star of regex_t

(*
TODO:
  Add 
  | Maybe of regex_t
  | Plus of regex_t
  | Maybe of regex_t
  
  Non trivial
  | AnyChar of regex_t
  | Charset
  | Negated Charset
*)
