
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | STAR
    | RPAR
    | LPAR
    | EOF
    | EMPTY
    | CONCAT
    | CHOICE
    | CHAR of (
# 1 "lib/parser.mly"
       (char)
# 22 "lib/parser.ml"
  )
  
end

include MenhirBasics

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState01 : (('s, _menhir_box_main) _menhir_cell1_LPAR, _menhir_box_main) _menhir_state
    (** State 01.
        Stack shape : LPAR.
        Start symbol: main. *)

  | MenhirState04 : ((('s, _menhir_box_main) _menhir_cell1_LPAR, _menhir_box_main) _menhir_cell1_regex, _menhir_box_main) _menhir_state
    (** State 04.
        Stack shape : LPAR regex.
        Start symbol: main. *)

  | MenhirState07 : ((('s, _menhir_box_main) _menhir_cell1_regex, _menhir_box_main) _menhir_cell1_CHOICE, _menhir_box_main) _menhir_state
    (** State 07.
        Stack shape : regex CHOICE.
        Start symbol: main. *)

  | MenhirState08 : (((('s, _menhir_box_main) _menhir_cell1_regex, _menhir_box_main) _menhir_cell1_CHOICE, _menhir_box_main) _menhir_cell1_regex, _menhir_box_main) _menhir_state
    (** State 08.
        Stack shape : regex CHOICE regex.
        Start symbol: main. *)

  | MenhirState09 : ((('s, _menhir_box_main) _menhir_cell1_regex, _menhir_box_main) _menhir_cell1_regex, _menhir_box_main) _menhir_state
    (** State 09.
        Stack shape : regex regex.
        Start symbol: main. *)

  | MenhirState10 : (('s, _menhir_box_main) _menhir_cell1_regex, _menhir_box_main) _menhir_state
    (** State 10.
        Stack shape : regex.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_regex = 
  | MenhirCell1_regex of 's * ('s, 'r) _menhir_state * (
# 13 "lib/parser.mly"
      (Ast.regex_t)
# 70 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_CHOICE = 
  | MenhirCell1_CHOICE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (
# 12 "lib/parser.mly"
      (Ast.regex_t)
# 83 "lib/parser.ml"
) [@@unboxed]

let _menhir_action_1 =
  fun r ->
    (
# 18 "lib/parser.mly"
                    ( r )
# 91 "lib/parser.ml"
     : (
# 12 "lib/parser.mly"
      (Ast.regex_t)
# 95 "lib/parser.ml"
    ))

let _menhir_action_2 =
  fun () ->
    (
# 21 "lib/parser.mly"
        ( Empty )
# 103 "lib/parser.ml"
     : (
# 13 "lib/parser.mly"
      (Ast.regex_t)
# 107 "lib/parser.ml"
    ))

let _menhir_action_3 =
  fun c ->
    (
# 22 "lib/parser.mly"
           ( Char c )
# 115 "lib/parser.ml"
     : (
# 13 "lib/parser.mly"
      (Ast.regex_t)
# 119 "lib/parser.ml"
    ))

let _menhir_action_4 =
  fun r ->
    (
# 23 "lib/parser.mly"
                      ( r )
# 127 "lib/parser.ml"
     : (
# 13 "lib/parser.mly"
      (Ast.regex_t)
# 131 "lib/parser.ml"
    ))

let _menhir_action_5 =
  fun a b ->
    (
# 24 "lib/parser.mly"
                             ( Choice(a, b) )
# 139 "lib/parser.ml"
     : (
# 13 "lib/parser.mly"
      (Ast.regex_t)
# 143 "lib/parser.ml"
    ))

let _menhir_action_6 =
  fun r ->
    (
# 25 "lib/parser.mly"
                 ( Star r )
# 151 "lib/parser.ml"
     : (
# 13 "lib/parser.mly"
      (Ast.regex_t)
# 155 "lib/parser.ml"
    ))

let _menhir_action_7 =
  fun a b ->
    (
# 26 "lib/parser.mly"
                                   ( Concat(a, b) )
# 163 "lib/parser.ml"
     : (
# 13 "lib/parser.mly"
      (Ast.regex_t)
# 167 "lib/parser.ml"
    ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | CHAR _ ->
        "CHAR"
    | CHOICE ->
        "CHOICE"
    | CONCAT ->
        "CONCAT"
    | EMPTY ->
        "EMPTY"
    | EOF ->
        "EOF"
    | LPAR ->
        "LPAR"
    | RPAR ->
        "RPAR"
    | STAR ->
        "STAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState01 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EMPTY ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_2 () in
      _menhir_goto_regex _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_regex : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState04 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState09 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState07 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAR ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | EOF ->
          let r = _v in
          let _v = _menhir_action_1 r in
          MenhirBox_main _v
      | EMPTY ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | CHOICE ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | CHAR _v_0 ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState10
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_regex -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_regex (_menhir_stack, _menhir_s, r) = _menhir_stack in
      let _v = _menhir_action_6 r in
      _menhir_goto_regex _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_07 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_regex as 'stack) -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CHOICE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState07 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EMPTY ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let c = _v in
      let _v = _menhir_action_3 c in
      _menhir_goto_regex _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_09 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_regex -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_regex (_menhir_stack, _menhir_s, a) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_7 a b in
      _menhir_goto_regex _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_08 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_regex, _menhir_box_main) _menhir_cell1_CHOICE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CHAR _ | CHOICE | EMPTY | EOF | LPAR | RPAR ->
          let MenhirCell1_CHOICE (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_regex (_menhir_stack, _menhir_s, a) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_5 a b in
          _menhir_goto_regex _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_4 r in
          _menhir_goto_regex _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | LPAR ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | EMPTY ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | CHOICE ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | CHAR _v_0 ->
          let _menhir_stack = MenhirCell1_regex (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState04
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EMPTY ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
