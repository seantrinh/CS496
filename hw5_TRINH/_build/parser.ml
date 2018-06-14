
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TO
    | TIMES
    | THEN
    | SETREF
    | SET
    | SEMICOLON
    | RPAREN
    | RBRACE
    | PROC
    | PLUS
    | NEWREF
    | MINUS
    | LPAREN
    | LETREC
    | LET
    | LBRACE
    | ISZERO
    | INT of (
# 22 "parser.mly"
       (int)
# 28 "parser.ml"
  )
    | IN
    | IF
    | ID of (
# 23 "parser.mly"
       (string)
# 35 "parser.ml"
  )
    | FOR
    | EQUALS
    | EOF
    | END
    | ELSE
    | DIVIDED
    | DEREF
    | DEBUG
    | COMMA
    | BEGIN
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState95
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState79
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState45
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState25
  | MenhirState21
  | MenhirState20
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState13
  | MenhirState9
  | MenhirState7
  | MenhirState5
  | MenhirState2
  | MenhirState0

# 8 "parser.mly"
  
open Ast

# 122 "parser.ml"

let rec _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 23 "parser.mly"
       (string)
# 134 "parser.ml"
        ))), _, (xs : (string list))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) = 
# 217 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 140 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | DEBUG ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | DEREF ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | FOR ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | ID _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
                | IF ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | INT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
                | ISZERO ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | LET ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | LETREC ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | LPAREN ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | NEWREF ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | PROC ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | SET ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | SETREF ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 23 "parser.mly"
       (string)
# 217 "parser.ml"
        ))), _, (xs : (string list))) = _menhir_stack in
        let _v : (string list) = 
# 197 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 222 "parser.ml"
         in
        _menhir_goto_nonempty_list_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUALS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | DEBUG ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | DEREF ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | FOR ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | ID _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
                | IF ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | INT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
                | ISZERO ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | LET ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | LETREC ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | LPAREN ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | NEWREF ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | PROC ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | SET ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | SETREF ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_nonempty_list_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 148 "parser.mly"
                                                          ( App(e1,e2) )
# 313 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _, (xs : (Ast.expr list))) = _menhir_stack in
        let _v : (Ast.expr list) = 
# 197 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 329 "parser.ml"
         in
        _menhir_goto_nonempty_list_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 130 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( x )
# 345 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr list) = 
# 217 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 357 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | DEBUG ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | DEREF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FOR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LETREC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NEWREF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PROC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SET ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SETREF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DEBUG ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DEREF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FOR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LETREC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEWREF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | PROC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SET ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SETREF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | DEBUG ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | DEREF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FOR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LETREC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NEWREF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | PROC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | SET ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | SETREF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | DEBUG ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | DEREF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FOR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LETREC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEWREF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | PROC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SET ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SETREF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 530 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 23 "parser.mly"
       (string)
# 553 "parser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 215 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 558 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 571 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 23 "parser.mly"
       (string)
# 585 "parser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 195 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 590 "parser.ml"
         in
        _menhir_goto_nonempty_list_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState48 | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState45 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 215 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 662 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 143 "parser.mly"
                                  ( Mul(e1,e2) )
# 677 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FOR | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 141 "parser.mly"
                                 ( Add(e1,e2) )
# 696 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 144 "parser.mly"
                                    ( Div(e1,e2) )
# 711 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FOR | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 142 "parser.mly"
                                  ( Sub(e1,e2) )
# 730 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState56 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 151 "parser.mly"
                                      ( DeRef(e) )
# 760 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState58 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState60 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState62 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 904 "parser.ml"
            ))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 159 "parser.mly"
                                                                                ( For(x, e1, e2, e3) )
# 914 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState64 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState66 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FOR | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 153 "parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 1061 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState69 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 149 "parser.mly"
                                       ( IsZero(e) )
# 1091 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FOR | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1176 "parser.ml"
            ))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 145 "parser.mly"
                                                    ( Let(x,e1,e2) )
# 1184 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FOR | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1267 "parser.ml"
            ))), _, (y : (string list))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 146 "parser.mly"
                                                                                              ( Letrec(x,y,e1,e2) )
# 1277 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState77 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 158 "parser.mly"
                                      ( Sub(Int 0, e) )
# 1307 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DEBUG ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DEREF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | FOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LETREC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | NEWREF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | PROC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState79 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 156 "parser.mly"
                               (e)
# 1364 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SET ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SETREF ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState83 | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DEBUG ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DEREF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | FOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LETREC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NEWREF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PROC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SET ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SETREF ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 195 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1426 "parser.ml"
             in
            _menhir_goto_nonempty_list_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 150 "parser.mly"
                                       ( NewRef(e) )
# 1456 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState87 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (vars : (string list))), _, (e : (Ast.expr))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 147 "parser.mly"
                                                                                                ( Proc(vars,e) )
# 1490 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BEGIN | COMMA | DEBUG | DEREF | ELSE | END | EOF | FOR | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1517 "parser.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 154 "parser.mly"
                                    ( Set(x,e) )
# 1524 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState90 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState92 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 152 "parser.mly"
                                                          ( SetRef(e1,e2) )
# 1613 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState95 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 77 "parser.mly"
       (Ast.prog)
# 1638 "parser.ml"
            ) = 
# 109 "parser.mly"
                 ( AProg e )
# 1642 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 77 "parser.mly"
       (Ast.prog)
# 1649 "parser.ml"
            )) = _v in
            Obj.magic _1
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | TIMES ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs0 : (Ast.expr list)) = _v in
    let _v : (Ast.expr list) = let es =
      let xs = xs0 in
      
# 206 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1675 "parser.ml"
      
    in
    
# 163 "parser.mly"
                                            ( es )
# 1681 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (es : (Ast.expr list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expr) = 
# 155 "parser.mly"
                             ( BeginEnd(es) )
# 1698 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DEBUG ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | DEREF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | FOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LETREC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NEWREF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | PROC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SET ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | SETREF ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | DEBUG ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | DEREF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | FOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | LETREC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | NEWREF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | PROC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | SET ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | SETREF ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | DEBUG ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | DEREF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | FOR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LETREC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MINUS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState16 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | DEBUG ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | DEREF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | FOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | LETREC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | NEWREF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | PROC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | SET ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | SETREF ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | NEWREF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | PROC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | SET ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | SETREF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DEBUG ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DEREF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | FOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LETREC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NEWREF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | PROC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | SET ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | SETREF ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "parser.mly"
       (int)
# 2356 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 22 "parser.mly"
       (int)
# 2364 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 138 "parser.mly"
              ( Int i )
# 2369 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | DEBUG ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | DEREF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | FOR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LETREC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NEWREF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | PROC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SET ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SETREF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 2417 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 23 "parser.mly"
       (string)
# 2425 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 139 "parser.mly"
             ( Var x )
# 2430 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | DEBUG ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | DEREF ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | FOR ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | ID _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | IF ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | INT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | ISZERO ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LETREC ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | NEWREF ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | PROC ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | SET ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | SETREF ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DEBUG ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DEREF ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | FOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | ID _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | IF ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | INT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | ISZERO ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LETREC ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | NEWREF ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | PROC ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | SET ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | SETREF ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 140 "parser.mly"
            ( Debug )
# 2558 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | DEBUG ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | DEREF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FOR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LETREC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NEWREF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | PROC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | SET ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | SETREF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState40 in
        let _v : (Ast.expr list) = 
# 128 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2604 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 77 "parser.mly"
       (Ast.prog)
# 2627 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEBUG ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FOR ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISZERO ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWREF ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PROC ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETREF ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 219 "/Users/seantrinh/.opam/system/lib/menhir/standard.mly"
  


# 2680 "parser.ml"
