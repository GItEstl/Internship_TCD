
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WhileToken
    | VoidToken
    | TrueToken
    | TauToken
    | TailToken
    | SubToken
    | StringToken
    | StartToken
    | SpawnToken
    | SimpleQuoteToken
    | SequenceToken
    | SendToken
    | RightSqBracketToken
    | RightParenthesisToken
    | RightBracketToken
    | ReturnToken
    | ReceiveToken
    | OrToken
    | OddToken
    | NumberToken of (
# 6 "parser.mly"
       (int)
# 30 "parser.ml"
  )
    | NoopToken
    | NewToken
    | MulToken
    | ListToken
    | LetToken
    | LesserToken
    | LeftSqBracketToken
    | LeftParenthesisToken
    | LeftBracketToken
    | IntegerToken
    | InToken
    | IfToken
    | IdentToken of (
# 7 "parser.mly"
       (string)
# 47 "parser.ml"
  )
    | HeadToken
    | GreaterToken
    | FunctionToken
    | FalseToken
    | EvenToken
    | EqualToken
    | ElseToken
    | EOF
    | DoubleQuoteToken
    | DivToken
    | DifferentToken
    | ComaToken
    | ChooseToken
    | ChoiceToken
    | CharValueToken of (
# 8 "parser.mly"
       (string)
# 66 "parser.ml"
  )
    | CharToken
    | ChannelToken
    | CallToken
    | BoolToken
    | AssignToken
    | ArrowToken
    | AndToken
    | AddToken
  
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
  | MenhirState232
  | MenhirState228
  | MenhirState225
  | MenhirState217
  | MenhirState215
  | MenhirState212
  | MenhirState209
  | MenhirState207
  | MenhirState199
  | MenhirState192
  | MenhirState188
  | MenhirState183
  | MenhirState177
  | MenhirState174
  | MenhirState170
  | MenhirState167
  | MenhirState162
  | MenhirState152
  | MenhirState146
  | MenhirState141
  | MenhirState140
  | MenhirState138
  | MenhirState135
  | MenhirState133
  | MenhirState130
  | MenhirState128
  | MenhirState125
  | MenhirState122
  | MenhirState116
  | MenhirState112
  | MenhirState102
  | MenhirState99
  | MenhirState96
  | MenhirState90
  | MenhirState86
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState58
  | MenhirState56
  | MenhirState53
  | MenhirState51
  | MenhirState48
  | MenhirState39
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState33
  | MenhirState30
  | MenhirState28
  | MenhirState24
  | MenhirState21
  | MenhirState16
  | MenhirState12
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  
open Ast

# 161 "parser.ml"

let rec _menhir_goto_valueSeq : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightSqBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (vs : (Ast.ast))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 154 "parser.mly"
                                                                                                                        (ValueNode (vs))
# 182 "parser.ml"
             in
            _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (vs : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 158 "parser.mly"
                                                                                                                        (ValueSeqNode (v,Some(vs)))
# 199 "parser.ml"
         in
        _menhir_goto_valueSeq _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 48 "parser.mly"
      (Ast.ast)
# 208 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (p : (
# 48 "parser.mly"
      (Ast.ast)
# 219 "parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 48 "parser.mly"
      (Ast.ast)
# 224 "parser.ml"
        ) = 
# 59 "parser.mly"
                                                                                                             (ProgramNode (v,p))
# 228 "parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (f : (Ast.ast))), _, (p : (
# 48 "parser.mly"
      (Ast.ast)
# 237 "parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 48 "parser.mly"
      (Ast.ast)
# 242 "parser.ml"
        ) = 
# 58 "parser.mly"
                                                                                                             (ProgramNode (f,p))
# 246 "parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (a : (
# 48 "parser.mly"
      (Ast.ast)
# 260 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 51 "parser.mly"
       (Ast.ast)
# 266 "parser.ml"
            ) = 
# 55 "parser.mly"
                         (a)
# 270 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 51 "parser.mly"
       (Ast.ast)
# 277 "parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run177 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CallToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState177 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IdentToken _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LeftParenthesisToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                | IfToken ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LetToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | NumberToken _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                | OddToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | NewToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState177 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BoolToken ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | ChannelToken ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | CharToken ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | IntegerToken ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | LeftParenthesisToken ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | ListToken ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | StringToken ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | ReceiveToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState177 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IdentToken _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RightParenthesisToken ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, (a : (Ast.ast))), _), (n : (
# 7 "parser.mly"
       (string)
# 443 "parser.ml"
                    ))) = _menhir_stack in
                    let _6 = () in
                    let _4 = () in
                    let _3 = () in
                    let _2 = () in
                    let _v : (Ast.ast) = 
# 94 "parser.mly"
                                                                                                                (ReceiveNode (a,n))
# 452 "parser.ml"
                     in
                    _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177

and _menhir_run102 : _menhir_env -> (((('ttv_tail * _menhir_state) * _menhir_state * (Ast.ast))) * _menhir_state * (Ast.ast)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_goto_exprs : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState207 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 137 "parser.mly"
                                                                                                                                  (ExprNode (e))
# 543 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 143 "parser.mly"
                                                                                                                                   (ExprsNode (e1,Some(e2)))
# 560 "parser.ml"
         in
        _menhir_goto_exprs _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run88 : _menhir_env -> ((((('ttv_tail * _menhir_state)) * _menhir_state * (Ast.ast)))) * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ElseToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | IfToken ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LetToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NumberToken _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | OddToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_goto_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState48 | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ComaToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | NumberToken _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | RightSqBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 157 "parser.mly"
                                                                                                                        (ValueSeqNode (v,None))
# 1064 "parser.ml"
             in
            _menhir_goto_valueSeq _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState228 | MenhirState28 | MenhirState209 | MenhirState207 | MenhirState112 | MenhirState133 | MenhirState199 | MenhirState138 | MenhirState192 | MenhirState177 | MenhirState188 | MenhirState167 | MenhirState170 | MenhirState141 | MenhirState152 | MenhirState146 | MenhirState135 | MenhirState130 | MenhirState128 | MenhirState125 | MenhirState122 | MenhirState116 | MenhirState30 | MenhirState33 | MenhirState34 | MenhirState36 | MenhirState102 | MenhirState99 | MenhirState38 | MenhirState96 | MenhirState51 | MenhirState90 | MenhirState86 | MenhirState53 | MenhirState56 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 138 "parser.mly"
                                                                                                                                  (ValueNode (v))
# 1080 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_body : _menhir_env -> 'ttv_tail -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (Ast.ast)) = _v in
    let ((((_menhir_stack, _menhir_s), _, (ft : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 1094 "parser.ml"
    ))), _, (params : (Ast.ast))) = _menhir_stack in
    let _6 = () in
    let _4 = () in
    let _1 = () in
    let _v : (Ast.ast) = 
# 64 "parser.mly"
                                                                                                                            (FunctionNode (ft,n,params,b))
# 1102 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BoolToken ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState232
    | ChannelToken ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState232
    | CharToken ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState232
    | FunctionToken ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState232
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState232
    | LeftParenthesisToken ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState232
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState232
    | StartToken ->
        _menhir_run226 _menhir_env (Obj.magic _menhir_stack) MenhirState232
    | StringToken ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState232
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState232

and _menhir_goto_choices : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (p : (Ast.ast))), _, (i : (Ast.ast))), _, (cs : (Ast.ast))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ast.ast) = 
# 108 "parser.mly"
                                                                                                                          (ChoicesNode(p,i,Some(cs)))
# 1146 "parser.ml"
         in
        _menhir_goto_choices _menhir_env _menhir_stack _menhir_s _v
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (c : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 101 "parser.mly"
                                                                                                                  (ChooseNode (c))
# 1165 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 123 "parser.mly"
                                                                                                             (UnaryNode (Even,e))
# 1215 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AddToken | AndToken | AssignToken | CallToken | CharValueToken _ | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | GreaterToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LesserToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | ReturnToken | RightBracketToken | RightParenthesisToken | SendToken | SpawnToken | SubToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 125 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Substract,e2))
# 1242 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 127 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Multiply,e2))
# 1259 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 128 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Divide,e2))
# 1270 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken | CallToken | CharValueToken _ | ChooseToken | ComaToken | DoubleQuoteToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | ReturnToken | RightBracketToken | RightParenthesisToken | SendToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 126 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Or,e2))
# 1305 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AndToken | AssignToken | CallToken | CharValueToken _ | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | ReturnToken | RightBracketToken | RightParenthesisToken | SendToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 132 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Lesser,e2))
# 1334 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AddToken | AndToken | AssignToken | CallToken | CharValueToken _ | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | GreaterToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LesserToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | ReturnToken | RightBracketToken | RightParenthesisToken | SendToken | SpawnToken | SubToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 124 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Add,e2))
# 1359 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AndToken | AssignToken | CallToken | CharValueToken _ | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | ReturnToken | RightBracketToken | RightParenthesisToken | SendToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 133 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Greater,e2))
# 1388 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AndToken | AssignToken | CallToken | CharValueToken _ | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | ReturnToken | RightBracketToken | RightParenthesisToken | SendToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 130 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Equal,e2))
# 1421 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AndToken | AssignToken | CallToken | CharValueToken _ | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | ReturnToken | RightBracketToken | RightParenthesisToken | SendToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 131 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Different,e2))
# 1454 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken | CallToken | CharValueToken _ | ChooseToken | ComaToken | DoubleQuoteToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | ReturnToken | RightBracketToken | RightParenthesisToken | SendToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 129 "parser.mly"
                                                                                                                                      (BinaryNode (e1,And,e2))
# 1493 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 120 "parser.mly"
                                                                                                             (UnaryNode (Head,e))
# 1536 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LeftBracketToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                | IfToken ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | LetToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | NumberToken _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                | OddToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightBracketToken ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (cond : (Ast.ast))), _, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 136 "parser.mly"
                                                                                                                                  (IfthenelseExprNode (cond,e1,e2))
# 1700 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState207 | MenhirState96 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | ComaToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | IfToken ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LetToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NumberToken _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | OddToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 142 "parser.mly"
                                                                                                                                   (ExprsNode (e,None))
# 1781 "parser.ml"
             in
            _menhir_goto_exprs _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | IfToken ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LetToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | NumberToken _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | OddToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | InToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LeftParenthesisToken ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (a : (Ast.ast))), _, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 134 "parser.mly"
                                                                                                                           (LetinExprNode (a,e1,e2))
# 1940 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 122 "parser.mly"
                                                                                                            (UnaryNode (Odd,e))
# 1985 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AddToken | AndToken | AssignToken | CallToken | CharValueToken _ | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | GreaterToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LesserToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | ReturnToken | RightBracketToken | RightParenthesisToken | SendToken | SpawnToken | SubToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast) = 
# 119 "parser.mly"
                                                                 (UnaryNode (Negate,e))
# 2012 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 121 "parser.mly"
                                                                                                             (UnaryNode (Tail,e))
# 2055 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LeftBracketToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CallToken ->
                    _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | IfToken ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | LetToken ->
                    _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | NoopToken ->
                    _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | NumberToken _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | OddToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | ReturnToken ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | SendToken ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | SpawnToken ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | WhileToken ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 2191 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 102 "parser.mly"
                                                                                                                          (SpawnNode (f,e))
# 2200 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (n : (
# 7 "parser.mly"
       (string)
# 2241 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 95 "parser.mly"
                                                                                                                     (SendNode (n,e))
# 2250 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CallToken | CharValueToken _ | ChooseToken | DoubleQuoteToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | ReturnToken | RightBracketToken | SendToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast) = 
# 105 "parser.mly"
                                                                                                                        (ReturnNode (e))
# 2293 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
            | IfToken ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | LetToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | NumberToken _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
            | OddToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | InToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LeftBracketToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CallToken ->
                    _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
                | IfToken ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | LetToken ->
                    _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | NoopToken ->
                    _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | NumberToken _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
                | OddToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | ReturnToken ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | SendToken ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | SpawnToken ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | WhileToken ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
            | LeftParenthesisToken ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LeftBracketToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CallToken ->
                    _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
                | IfToken ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | LetToken ->
                    _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | NoopToken ->
                    _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | NumberToken _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
                | OddToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | ReturnToken ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | SendToken ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | SpawnToken ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | WhileToken ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _), (f : (
# 7 "parser.mly"
       (string)
# 2594 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 116 "parser.mly"
                                                                                                                                        (PrefixNode(None,Spawn,Some(f),Some(e)))
# 2604 "parser.ml"
             in
            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _), (n : (
# 7 "parser.mly"
       (string)
# 2645 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 113 "parser.mly"
                                                                                                                                     (PrefixNode(None,Send,Some(n),Some(e)))
# 2655 "parser.ml"
             in
            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NewToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LeftParenthesisToken ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BoolToken ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                    | ChannelToken ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                    | CharToken ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                    | IntegerToken ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                    | LeftParenthesisToken ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                    | ListToken ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                    | StringToken ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState162
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | ReceiveToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LeftParenthesisToken ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IdentToken _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | RightParenthesisToken ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s), _, (a : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 2737 "parser.ml"
                            ))) = _menhir_stack in
                            let _7 = () in
                            let _5 = () in
                            let _4 = () in
                            let _3 = () in
                            let _1 = () in
                            let _v : (Ast.ast) = 
# 114 "parser.mly"
                                                                                                                                   (PrefixNode(Some(a),Receive,Some(n),None))
# 2747 "parser.ml"
                             in
                            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 2826 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 93 "parser.mly"
                                                                                                                   (CallNode (f,e))
# 2834 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 | MenhirState209 | MenhirState112 | MenhirState133 | MenhirState199 | MenhirState192 | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (a : (Ast.ast))), _), (f : (
# 7 "parser.mly"
       (string)
# 2908 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 92 "parser.mly"
                                                                                                                  (BinaryNode (a,Assign,CallNode (f,e)))
# 2917 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CallToken | CharValueToken _ | ChooseToken | DoubleQuoteToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | ReturnToken | RightBracketToken | SendToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Ast.ast))), _, (e : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 91 "parser.mly"
                                                                                                                  (BinaryNode (a,Assign,e))
# 2960 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightBracketToken ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState228 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 3034 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 178 "parser.mly"
                                                                                                                       (CallNode (f,e))
# 3042 "parser.ml"
             in
            (match _menhir_s with
            | MenhirState232 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c : (Ast.ast)) = _v in
                let (_menhir_stack, _menhir_s, (f : (Ast.ast))) = _menhir_stack in
                let _v : (
# 48 "parser.mly"
      (Ast.ast)
# 3053 "parser.ml"
                ) = 
# 60 "parser.mly"
                                                                                                                 (ProgramNode (f,c))
# 3057 "parser.ml"
                 in
                _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
            | MenhirState225 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c : (Ast.ast)) = _v in
                let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
                let _v : (
# 48 "parser.mly"
      (Ast.ast)
# 3068 "parser.ml"
                ) = 
# 61 "parser.mly"
                                                                                                                 (ProgramNode (v,c))
# 3072 "parser.ml"
                 in
                _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | SubToken ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run141 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | SendToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState141 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IdentToken _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ComaToken ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | CharValueToken _v ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
                    | DoubleQuoteToken ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | EvenToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | FalseToken ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | HeadToken ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | IdentToken _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
                    | IfToken ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | LeftParenthesisToken ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | LeftSqBracketToken ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | LetToken ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | NumberToken _v ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
                    | OddToken ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | SubToken ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | TailToken ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | TrueToken ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SpawnToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState141 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IdentToken _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ComaToken ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | CharValueToken _v ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
                    | DoubleQuoteToken ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | EvenToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | FalseToken ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | HeadToken ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | IdentToken _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
                    | IfToken ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | LeftParenthesisToken ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | LeftSqBracketToken ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | LetToken ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | NumberToken _v ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
                    | OddToken ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | SubToken ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | TailToken ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | TrueToken ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | TauToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState141 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.ast) = 
# 112 "parser.mly"
                                                                                                                                       (PrefixNode(None,Tau,None,None))
# 3283 "parser.ml"
         in
        _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141

and _menhir_goto_cst : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (Ast.ast)) = _v in
    let _v : (Ast.ast) = 
# 153 "parser.mly"
                                                                                                                        (ValueNode (c))
# 3301 "parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CharValueToken _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LetToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NumberToken _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | OddToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_tupleDecla : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState217 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (n : (
# 7 "parser.mly"
       (string)
# 3408 "parser.ml"
        ))), _, (idents : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 80 "parser.mly"
                                                                                                            (TupleDeclaNode (n,Some(idents)))
# 3414 "parser.ml"
         in
        _menhir_goto_tupleDecla _menhir_env _menhir_stack _menhir_s _v
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t : (Ast.ast))), _, (idents : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 76 "parser.mly"
                                                                                                            (VariableDeclaNode (t,idents))
# 3432 "parser.ml"
             in
            _menhir_goto_variableDecla _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run226 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IdentToken _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _v
            | IfToken ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | LetToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | NumberToken _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _v
            | OddToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState228)
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

and _menhir_goto_variableDeclas : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CallToken ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
            | ChooseToken ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
            | IfToken ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | LetToken ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | NoopToken ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | NumberToken _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
            | OddToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | ReturnToken ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | SendToken ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | SpawnToken ->
                _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | WhileToken ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (vs : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 83 "parser.mly"
                                                                                                             (VariableDeclasNode (v,Some(vs)))
# 3583 "parser.ml"
         in
        _menhir_goto_variableDeclas _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ChoiceToken ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | RightBracketToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (p : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _2 = () in
                let _v : (Ast.ast) = 
# 109 "parser.mly"
                                                                                                                       (ChoicesNode(p,i,None))
# 3614 "parser.ml"
                 in
                _menhir_goto_choices _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (bi : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 88 "parser.mly"
                                                                                                          (InstrSeqNode (bi,Some(i)))
# 3634 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ElseToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LeftBracketToken ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | CallToken ->
                        _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | CharValueToken _v ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
                    | ChooseToken ->
                        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | DoubleQuoteToken ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | EvenToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | FalseToken ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | HeadToken ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | IdentToken _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
                    | IfToken ->
                        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | LeftParenthesisToken ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | LeftSqBracketToken ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | LetToken ->
                        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | NoopToken ->
                        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | NumberToken _v ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
                    | OddToken ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | ReturnToken ->
                        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | SendToken ->
                        _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | SpawnToken ->
                        _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | SubToken ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | TailToken ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | TrueToken ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | WhileToken ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199)
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
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (cond : (Ast.ast))), _, (i1 : (Ast.ast))), _, (i2 : (Ast.ast))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 98 "parser.mly"
                                                                                                                (IfthenelseInstrNode (cond,i1,i2))
# 3744 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (a : (Ast.ast))), _, (e : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 99 "parser.mly"
                                                                                                         (LetinInstrNode (a,e,i))
# 3771 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 100 "parser.mly"
                                                                                                                              (WhileNode (e,i))
# 3798 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _, (v : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 72 "parser.mly"
                                                                                                             (BodyNode (Some(v),i))
# 3823 "parser.ml"
             in
            _menhir_goto_body _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (i : (Ast.ast))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 71 "parser.mly"
                                                                                                                (BodyNode (None,i))
# 3847 "parser.ml"
             in
            _menhir_goto_body _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CharValueToken _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LetToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NumberToken _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | OddToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
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

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 149 "parser.mly"
                                                                                                                          (TrueNode)
# 3960 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CharValueToken _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LetToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | NumberToken _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | OddToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run113 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IdentToken _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ComaToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | IfToken ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | LetToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | NumberToken _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | OddToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run119 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IdentToken _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ComaToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | IfToken ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | LetToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | NumberToken _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | OddToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CharValueToken _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LetToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NumberToken _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | OddToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (int)
# 4303 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (
# 6 "parser.mly"
       (int)
# 4311 "parser.ml"
    )) = _v in
    let _v : (Ast.ast) = 
# 146 "parser.mly"
                                                                                                                          (IntegerNode(c))
# 4316 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 104 "parser.mly"
                                                                                                                        (NoopNode)
# 4328 "parser.ml"
     in
    _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_run128 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run207 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BoolToken ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | ChannelToken ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | CharToken ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | LeftParenthesisToken ->
        _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | LetToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | StringToken ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CharValueToken _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LetToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | NumberToken _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | OddToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 4504 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "parser.mly"
       (string)
# 4512 "parser.ml"
    )) = _v in
    let _v : (Ast.ast) = 
# 139 "parser.mly"
                                                                                                                                  (AssignNode (n))
# 4517 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CharValueToken _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LetToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | NumberToken _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | OddToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 150 "parser.mly"
                                                                                                                          (FalseNode)
# 4581 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CharValueToken _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LetToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | NumberToken _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | OddToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IdentToken _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DoubleQuoteToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (c : (
# 7 "parser.mly"
       (string)
# 4656 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 148 "parser.mly"
                                                                                                                          (StringNode (c))
# 4663 "parser.ml"
             in
            _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftBracketToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ChoiceToken ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 4706 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (
# 8 "parser.mly"
       (string)
# 4714 "parser.ml"
    )) = _v in
    let _v : (Ast.ast) = 
# 147 "parser.mly"
                                                                                                                         (CharNode (c))
# 4719 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run168 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IdentToken _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | IfToken ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | LetToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | NumberToken _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | OddToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170)
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

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run216 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 4795 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ComaToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IdentToken _v ->
            _menhir_run216 _menhir_env (Obj.magic _menhir_stack) MenhirState217 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState217)
    | RightParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (n : (
# 7 "parser.mly"
       (string)
# 4818 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 79 "parser.mly"
                                                                                                            (TupleDeclaNode (n,None))
# 4823 "parser.ml"
         in
        _menhir_goto_tupleDecla _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_variableDecla : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState212 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BoolToken ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | ChannelToken ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | CharToken ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | IntegerToken ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | LeftParenthesisToken ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | ListToken ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | StringToken ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 84 "parser.mly"
                                                                                                             (VariableDeclasNode (v,None))
# 4862 "parser.ml"
             in
            _menhir_goto_variableDeclas _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212)
    | MenhirState232 | MenhirState225 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BoolToken ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | ChannelToken ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | CharToken ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | FunctionToken ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | IntegerToken ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | LeftParenthesisToken ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | ListToken ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | StartToken ->
            _menhir_run226 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | StringToken ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState225
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState225)
    | _ ->
        _menhir_fail ()

and _menhir_goto_binstruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CallToken ->
        _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | ChooseToken ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | IfToken ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | LetToken ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | NoopToken ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | NumberToken _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | OddToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | ReturnToken ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | SendToken ->
        _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | SpawnToken ->
        _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | WhileToken ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | RightBracketToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (bi : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 87 "parser.mly"
                                                                                                           (InstrSeqNode (bi,None))
# 4956 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192

and _menhir_goto_prefix : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ArrowToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CallToken ->
                _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | ChooseToken ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | IfToken ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | LetToken ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | NoopToken ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | NumberToken _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | OddToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | ReturnToken ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | SendToken ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | SpawnToken ->
                _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | WhileToken ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167)
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_parameters : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (t : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 5052 "parser.ml"
        ))), _, (params : (Ast.ast))) = _menhir_stack in
        let _3 = () in
        let _v : (Ast.ast) = 
# 68 "parser.mly"
                                                                                                            (ParamsNode (t,n,Some(params)))
# 5058 "parser.ml"
         in
        _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LeftBracketToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BoolToken ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | CallToken ->
                    _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | ChannelToken ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | CharToken ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
                | IfToken ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | IntegerToken ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | LeftParenthesisToken ->
                    _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | LetToken ->
                    _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | ListToken ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | NoopToken ->
                    _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | NumberToken _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
                | OddToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | ReturnToken ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | SendToken ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | SpawnToken ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | StringToken ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | WhileToken ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
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

and _menhir_goto_types : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState207 | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (tSeq : (Ast.ast))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 167 "parser.mly"
                                                                                                                        (TupleTNode (tSeq))
# 5170 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t1 : (Ast.ast))), _, (t2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 171 "parser.mly"
                                                                                                                      (TypeSeqNode (t1, Some(t2)))
# 5187 "parser.ml"
         in
        _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_funcType : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IdentToken _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BoolToken ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | ChannelToken ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | CharToken ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | IntegerToken ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | LeftParenthesisToken ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | ListToken ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | StringToken ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState207 | MenhirState12 | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ComaToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BoolToken ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | ChannelToken ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | CharToken ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | IntegerToken ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LeftParenthesisToken ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | ListToken ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | StringToken ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 170 "parser.mly"
                                                                                                                      (TypeSeqNode (t,None))
# 5280 "parser.ml"
             in
            _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightSqBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 166 "parser.mly"
                                                                                                                          (ListTNode (t))
# 5305 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (t : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 175 "parser.mly"
                                                                                                                      (FuncTNode (Some (t)))
# 5321 "parser.ml"
         in
        _menhir_goto_funcType _menhir_env _menhir_stack _menhir_s _v
    | MenhirState24 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IdentToken _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ComaToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BoolToken ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                | ChannelToken ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                | CharToken ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                | IntegerToken ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                | LeftParenthesisToken ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                | ListToken ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                | StringToken ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
            | RightParenthesisToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (t : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 5363 "parser.ml"
                ))) = _menhir_stack in
                let _v : (Ast.ast) = 
# 67 "parser.mly"
                                                                                                            (ParamsNode (t,n,None))
# 5368 "parser.ml"
                 in
                _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (a : (Ast.ast))), _, (t : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 115 "parser.mly"
                                                                                                                                    (PrefixNode(Some(a),New,None,Some(t)))
# 5401 "parser.ml"
             in
            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.ast))), _), _, (t : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 103 "parser.mly"
                                                                                                                       (NewNode (a,t))
# 5427 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState232 | MenhirState225 | MenhirState28 | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IdentToken _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (n : (
# 7 "parser.mly"
       (string)
# 5448 "parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (t : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 75 "parser.mly"
                                                                                                            (VariableDeclaNode (t,StringNode(n)))
# 5454 "parser.ml"
             in
            _menhir_goto_variableDecla _menhir_env _menhir_stack _menhir_s _v
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IdentToken _v ->
                _menhir_run216 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState228 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState217 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 163 "parser.mly"
                                                                                                                          (TypeNode (StringT))
# 5743 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftSqBracketToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BoolToken ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | ChannelToken ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | CharToken ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | IntegerToken ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | LeftParenthesisToken ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | ListToken ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | StringToken ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BoolToken ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | ChannelToken ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | CharToken ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LeftParenthesisToken ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | StringToken ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 161 "parser.mly"
                                                                                                                           (TypeNode (IntegerT))
# 5816 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BoolToken ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | ChannelToken ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | CharToken ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LeftParenthesisToken ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | StringToken ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | VoidToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState16 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast) = 
# 174 "parser.mly"
                                                                                                                      (FuncTNode (None))
# 5849 "parser.ml"
         in
        _menhir_goto_funcType _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 164 "parser.mly"
                                                                                                                          (TypeNode (CharT))
# 5865 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 165 "parser.mly"
                                                                                                                          (TypeNode (ChannT))
# 5877 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 162 "parser.mly"
                                                                                                                          (TypeNode (BooleanT))
# 5889 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

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

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 51 "parser.mly"
       (Ast.ast)
# 5908 "parser.ml"
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
    | BoolToken ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ChannelToken ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CharToken ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FunctionToken ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LeftParenthesisToken ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | StringToken ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "/Users/estelle/.opam/ocaml-base-compiler.4.07.1/lib/menhir/standard.mly"
  

# 5946 "parser.ml"
