open Regex_engine

let rec format_regex = function
  | Ast.Empty ->
      "Empty"
  | Ast.Char c ->
      "Char " ^ String.make 1 c
  | Ast.Concat (a, b) ->
      "Concat(" ^ format_regex a ^ ", " ^ format_regex b ^ ")"
  | Ast.Choice (a, b) ->
      "Choice(" ^ format_regex a ^ ", " ^ format_regex b ^ ")"
  | Ast.Star a ->
      "Star(" ^ format_regex a ^ ")"

let () =
  let s = read_line () in
  let r = Parser.main Lexer.token (Lexing.from_string s) in
  print_endline (format_regex r)
