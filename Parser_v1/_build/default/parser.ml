
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
    | ReceiveToken
    | OrToken
    | OddToken
    | NumberToken of (
# 6 "parser.mly"
       (int)
# 29 "parser.ml"
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
# 46 "parser.ml"
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
       (char)
# 65 "parser.ml"
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
  | MenhirState124
  | MenhirState118
  | MenhirState114
  | MenhirState104
  | MenhirState101
  | MenhirState98
  | MenhirState92
  | MenhirState88
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState60
  | MenhirState58
  | MenhirState55
  | MenhirState53
  | MenhirState50
  | MenhirState42
  | MenhirState41
  | MenhirState39
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

# 159 "parser.ml"

let rec _menhir_goto_valueSeq : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState42 ->
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
# 152 "parser.mly"
                                                                                                                        (ValueNode (vs))
# 180 "parser.ml"
             in
            _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (vs : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 156 "parser.mly"
                                                                                                                        (ValueSeqNode (v,Some(vs)))
# 197 "parser.ml"
         in
        _menhir_goto_valueSeq _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 47 "parser.mly"
      (Ast.ast)
# 206 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (p : (
# 47 "parser.mly"
      (Ast.ast)
# 217 "parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 47 "parser.mly"
      (Ast.ast)
# 222 "parser.ml"
        ) = 
# 58 "parser.mly"
                                                                                                             (ProgramNode (v,p))
# 226 "parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (f : (Ast.ast))), _, (p : (
# 47 "parser.mly"
      (Ast.ast)
# 235 "parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 47 "parser.mly"
      (Ast.ast)
# 240 "parser.ml"
        ) = 
# 57 "parser.mly"
                                                                                                             (ProgramNode (f,p))
# 244 "parser.ml"
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
# 47 "parser.mly"
      (Ast.ast)
# 258 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 50 "parser.mly"
       (Ast.ast)
# 264 "parser.ml"
            ) = 
# 54 "parser.mly"
                         (a)
# 268 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 50 "parser.mly"
       (Ast.ast)
# 275 "parser.ml"
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
                | DoubleQuoteToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | EvenToken ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | FalseToken ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | HeadToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | IdentToken _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                | IfToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LeftParenthesisToken ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LeftSqBracketToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | LetToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | NumberToken _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                | OddToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | SimpleQuoteToken ->
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
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState177
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
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState177
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
# 439 "parser.ml"
                    ))) = _menhir_stack in
                    let _6 = () in
                    let _4 = () in
                    let _3 = () in
                    let _2 = () in
                    let _v : (Ast.ast) = 
# 93 "parser.mly"
                                                                                                                (ReceiveNode (a,n))
# 448 "parser.ml"
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
    | SimpleQuoteToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState177
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

and _menhir_run104 : _menhir_env -> (((('ttv_tail * _menhir_state) * _menhir_state * (Ast.ast))) * _menhir_state * (Ast.ast)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | SimpleQuoteToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_goto_exprs : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState207 | MenhirState53 ->
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
# 135 "parser.mly"
                                                                                                                                  (ExprNode (e))
# 541 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 141 "parser.mly"
                                                                                                                                   (ExprsNode (e1,Some(e2)))
# 558 "parser.ml"
         in
        _menhir_goto_exprs _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run90 : _menhir_env -> ((((('ttv_tail * _menhir_state)) * _menhir_state * (Ast.ast)))) * _menhir_state * (Ast.ast) -> 'ttv_return =
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
            | DoubleQuoteToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | EvenToken ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | FalseToken ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | HeadToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | IdentToken _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | IfToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | LeftParenthesisToken ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | LeftSqBracketToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | LetToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | NumberToken _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | OddToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | SimpleQuoteToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
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

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SimpleQuoteToken ->
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
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | SimpleQuoteToken ->
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

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SimpleQuoteToken ->
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

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | SimpleQuoteToken ->
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

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | SimpleQuoteToken ->
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

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | SimpleQuoteToken ->
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

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | SimpleQuoteToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | SimpleQuoteToken ->
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

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | SimpleQuoteToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SimpleQuoteToken ->
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

and _menhir_goto_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ComaToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DoubleQuoteToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | FalseToken ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LeftSqBracketToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | NumberToken _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | SimpleQuoteToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
        | RightSqBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 155 "parser.mly"
                                                                                                                        (ValueSeqNode (v,None))
# 1062 "parser.ml"
             in
            _menhir_goto_valueSeq _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState228 | MenhirState28 | MenhirState209 | MenhirState207 | MenhirState114 | MenhirState133 | MenhirState199 | MenhirState138 | MenhirState192 | MenhirState177 | MenhirState188 | MenhirState167 | MenhirState170 | MenhirState141 | MenhirState152 | MenhirState146 | MenhirState135 | MenhirState130 | MenhirState128 | MenhirState124 | MenhirState118 | MenhirState30 | MenhirState33 | MenhirState34 | MenhirState39 | MenhirState104 | MenhirState101 | MenhirState41 | MenhirState98 | MenhirState53 | MenhirState92 | MenhirState88 | MenhirState55 | MenhirState58 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 136 "parser.mly"
                                                                                                                                  (ValueNode (v))
# 1078 "parser.ml"
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
# 1092 "parser.ml"
    ))), _, (params : (Ast.ast))) = _menhir_stack in
    let _6 = () in
    let _4 = () in
    let _1 = () in
    let _v : (Ast.ast) = 
# 63 "parser.mly"
                                                                                                                            (FunctionNode (ft,n,params,b))
# 1100 "parser.ml"
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
# 106 "parser.mly"
                                                                                                                          (ChoicesNode(p,i,Some(cs)))
# 1144 "parser.ml"
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
# 100 "parser.mly"
                                                                                                                  (ChooseNode (c))
# 1163 "parser.ml"
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
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
                                                                                                             (UnaryNode (Even,e))
# 1213 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | AddToken | AndToken | AssignToken | CallToken | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | GreaterToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LesserToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | RightBracketToken | RightParenthesisToken | SendToken | SimpleQuoteToken | SpawnToken | SubToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 123 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Substract,e2))
# 1240 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 125 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Multiply,e2))
# 1257 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 126 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Divide,e2))
# 1268 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken | CallToken | ChooseToken | ComaToken | DoubleQuoteToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | RightBracketToken | RightParenthesisToken | SendToken | SimpleQuoteToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 124 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Or,e2))
# 1303 "parser.ml"
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
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AndToken | AssignToken | CallToken | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | RightBracketToken | RightParenthesisToken | SendToken | SimpleQuoteToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 130 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Lesser,e2))
# 1332 "parser.ml"
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
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | AddToken | AndToken | AssignToken | CallToken | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | GreaterToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LesserToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | RightBracketToken | RightParenthesisToken | SendToken | SimpleQuoteToken | SpawnToken | SubToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 122 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Add,e2))
# 1357 "parser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AndToken | AssignToken | CallToken | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | RightBracketToken | RightParenthesisToken | SendToken | SimpleQuoteToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 131 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Greater,e2))
# 1386 "parser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AndToken | AssignToken | CallToken | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | RightBracketToken | RightParenthesisToken | SendToken | SimpleQuoteToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 128 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Equal,e2))
# 1419 "parser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AndToken | AssignToken | CallToken | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | RightBracketToken | RightParenthesisToken | SendToken | SimpleQuoteToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 129 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Different,e2))
# 1452 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken | CallToken | ChooseToken | ComaToken | DoubleQuoteToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | RightBracketToken | RightParenthesisToken | SendToken | SimpleQuoteToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 127 "parser.mly"
                                                                                                                                      (BinaryNode (e1,And,e2))
# 1491 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 118 "parser.mly"
                                                                                                             (UnaryNode (Head,e))
# 1534 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
                | DoubleQuoteToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | EvenToken ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | FalseToken ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | HeadToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | IdentToken _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
                | IfToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | LeftParenthesisToken ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | LeftSqBracketToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | LetToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | NumberToken _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
                | OddToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | SimpleQuoteToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightBracketToken ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
# 134 "parser.mly"
                                                                                                                                  (IfthenelseExprNode (cond,e1,e2))
# 1698 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState207 | MenhirState98 | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | ComaToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DoubleQuoteToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | EvenToken ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | FalseToken ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | HeadToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | IdentToken _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | IfToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | LeftParenthesisToken ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | LeftSqBracketToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | LetToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | NumberToken _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | OddToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | SimpleQuoteToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 140 "parser.mly"
                                                                                                                                   (ExprsNode (e,None))
# 1779 "parser.ml"
             in
            _menhir_goto_exprs _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DoubleQuoteToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | EvenToken ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | FalseToken ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | HeadToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | IdentToken _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | IfToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | LeftParenthesisToken ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | LeftSqBracketToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | LetToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | NumberToken _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | OddToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | SimpleQuoteToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | SubToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | TailToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | TrueToken ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | InToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LeftParenthesisToken ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
# 132 "parser.mly"
                                                                                                                           (LetinExprNode (a,e1,e2))
# 1938 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
                                                                                                            (UnaryNode (Odd,e))
# 1983 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | AddToken | AndToken | AssignToken | CallToken | ChooseToken | ComaToken | DifferentToken | DoubleQuoteToken | EqualToken | EvenToken | FalseToken | GreaterToken | HeadToken | IdentToken _ | IfToken | InToken | LeftParenthesisToken | LeftSqBracketToken | LesserToken | LetToken | NoopToken | NumberToken _ | OddToken | OrToken | RightBracketToken | RightParenthesisToken | SendToken | SimpleQuoteToken | SpawnToken | SubToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast) = 
# 117 "parser.mly"
                                                                 (UnaryNode (Negate,e))
# 2010 "parser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 119 "parser.mly"
                                                                                                             (UnaryNode (Tail,e))
# 2053 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | DoubleQuoteToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | EvenToken ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | FalseToken ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | HeadToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | IdentToken _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
                | IfToken ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | LeftParenthesisToken ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | LeftSqBracketToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | LetToken ->
                    _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | NoopToken ->
                    _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | NumberToken _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
                | OddToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | SendToken ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | SimpleQuoteToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | SpawnToken ->
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | WhileToken ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 2187 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 101 "parser.mly"
                                                                                                                          (SpawnNode (f,e))
# 2196 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (n : (
# 7 "parser.mly"
       (string)
# 2237 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 94 "parser.mly"
                                                                                                                     (SendNode (n,e))
# 2246 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DoubleQuoteToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | EvenToken ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | FalseToken ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | HeadToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | IdentToken _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
            | IfToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | LeftParenthesisToken ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | LeftSqBracketToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | LetToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | NumberToken _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
            | OddToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | SimpleQuoteToken ->
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | DoubleQuoteToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | EvenToken ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | FalseToken ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | HeadToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | IdentToken _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
                | IfToken ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | LeftParenthesisToken ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | LeftSqBracketToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | LetToken ->
                    _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | NoopToken ->
                    _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | NumberToken _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
                | OddToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | SendToken ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | SimpleQuoteToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState133
                | SpawnToken ->
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState133
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
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | DoubleQuoteToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | EvenToken ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | FalseToken ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | HeadToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | IdentToken _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
                | IfToken ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | LeftParenthesisToken ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | LeftSqBracketToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | LetToken ->
                    _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | NoopToken ->
                    _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | NumberToken _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
                | OddToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | SendToken ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | SimpleQuoteToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | SpawnToken ->
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState138
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
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _), (f : (
# 7 "parser.mly"
       (string)
# 2545 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 114 "parser.mly"
                                                                                                                                        (PrefixNode(None,Spawn,Some(f),Some(e)))
# 2555 "parser.ml"
             in
            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _), (n : (
# 7 "parser.mly"
       (string)
# 2596 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 111 "parser.mly"
                                                                                                                                     (PrefixNode(None,Send,Some(n),Some(e)))
# 2606 "parser.ml"
             in
            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
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
# 2688 "parser.ml"
                            ))) = _menhir_stack in
                            let _7 = () in
                            let _5 = () in
                            let _4 = () in
                            let _3 = () in
                            let _1 = () in
                            let _v : (Ast.ast) = 
# 112 "parser.mly"
                                                                                                                                   (PrefixNode(Some(a),Receive,Some(n),None))
# 2698 "parser.ml"
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
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 2777 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 92 "parser.mly"
                                                                                                                   (CallNode (f,e))
# 2785 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 | MenhirState209 | MenhirState114 | MenhirState133 | MenhirState199 | MenhirState192 | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (a : (Ast.ast))), _), (f : (
# 7 "parser.mly"
       (string)
# 2859 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 91 "parser.mly"
                                                                                                                  (BinaryNode (a,Assign,CallNode (f,e)))
# 2868 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | CallToken | ChooseToken | DoubleQuoteToken | EvenToken | FalseToken | HeadToken | IdentToken _ | IfToken | LeftParenthesisToken | LeftSqBracketToken | LetToken | NoopToken | NumberToken _ | OddToken | RightBracketToken | SendToken | SimpleQuoteToken | SpawnToken | TailToken | TrueToken | WhileToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Ast.ast))), _, (e : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 90 "parser.mly"
                                                                                                                  (BinaryNode (a,Assign,e))
# 2911 "parser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightBracketToken ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 2985 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 176 "parser.mly"
                                                                                                                             (CallNode (f,e))
# 2993 "parser.ml"
             in
            (match _menhir_s with
            | MenhirState232 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c : (Ast.ast)) = _v in
                let (_menhir_stack, _menhir_s, (f : (Ast.ast))) = _menhir_stack in
                let _v : (
# 47 "parser.mly"
      (Ast.ast)
# 3004 "parser.ml"
                ) = 
# 59 "parser.mly"
                                                                                                                 (ProgramNode (f,c))
# 3008 "parser.ml"
                 in
                _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
            | MenhirState225 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c : (Ast.ast)) = _v in
                let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
                let _v : (
# 47 "parser.mly"
      (Ast.ast)
# 3019 "parser.ml"
                ) = 
# 60 "parser.mly"
                                                                                                                 (ProgramNode (v,c))
# 3023 "parser.ml"
                 in
                _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | SubToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_cst : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (Ast.ast)) = _v in
    let _v : (Ast.ast) = 
# 151 "parser.mly"
                                                                                                                        (ValueNode (c))
# 3047 "parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run141 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState141
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
                    | DoubleQuoteToken ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | EvenToken ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | FalseToken ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | HeadToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | IdentToken _v ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
                    | IfToken ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | LeftParenthesisToken ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | LeftSqBracketToken ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | LetToken ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | NumberToken _v ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
                    | OddToken ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                    | SimpleQuoteToken ->
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
    | SimpleQuoteToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState141
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
                    | DoubleQuoteToken ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | EvenToken ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | FalseToken ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | HeadToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | IdentToken _v ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
                    | IfToken ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | LeftParenthesisToken ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | LeftSqBracketToken ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | LetToken ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | NumberToken _v ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
                    | OddToken ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                    | SimpleQuoteToken ->
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
# 110 "parser.mly"
                                                                                                                                       (PrefixNode(None,Tau,None,None))
# 3246 "parser.ml"
         in
        _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState141
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | SimpleQuoteToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | SubToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TailToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | DoubleQuoteToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | EvenToken ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | FalseToken ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | HeadToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | IdentToken _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | IfToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LeftParenthesisToken ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LeftSqBracketToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LetToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | NumberToken _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | OddToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SimpleQuoteToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
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
# 3359 "parser.ml"
        ))), _, (idents : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 79 "parser.mly"
                                                                                                            (TupleDeclaNode (n,Some(idents)))
# 3365 "parser.ml"
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
# 75 "parser.mly"
                                                                                                            (VariableDeclaNode (t,idents))
# 3383 "parser.ml"
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
            | DoubleQuoteToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | EvenToken ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | FalseToken ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | HeadToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | IdentToken _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _v
            | IfToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | LeftParenthesisToken ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | LeftSqBracketToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | LetToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | NumberToken _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _v
            | OddToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState228
            | SimpleQuoteToken ->
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
            | ChooseToken ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | DoubleQuoteToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | EvenToken ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | FalseToken ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | HeadToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | IdentToken _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
            | IfToken ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | LeftParenthesisToken ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | LeftSqBracketToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | LetToken ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | NoopToken ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | NumberToken _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
            | OddToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | SendToken ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | SimpleQuoteToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | SpawnToken ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState209
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
# 82 "parser.mly"
                                                                                                             (VariableDeclasNode (v,Some(vs)))
# 3532 "parser.ml"
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
# 107 "parser.mly"
                                                                                                                       (ChoicesNode(p,i,None))
# 3563 "parser.ml"
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
# 87 "parser.mly"
                                                                                                          (InstrSeqNode (bi,Some(i)))
# 3583 "parser.ml"
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
                    | ChooseToken ->
                        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | DoubleQuoteToken ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | EvenToken ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | FalseToken ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | HeadToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | IdentToken _v ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
                    | IfToken ->
                        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | LeftParenthesisToken ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | LeftSqBracketToken ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | LetToken ->
                        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | NoopToken ->
                        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | NumberToken _v ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
                    | OddToken ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | SendToken ->
                        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | SimpleQuoteToken ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState199
                    | SpawnToken ->
                        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState199
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
# 97 "parser.mly"
                                                                                                                (IfthenelseInstrNode (cond,i1,i2))
# 3691 "parser.ml"
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
# 98 "parser.mly"
                                                                                                         (LetinInstrNode (a,e,i))
# 3718 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
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
# 99 "parser.mly"
                                                                                                                              (WhileNode (e,i))
# 3745 "parser.ml"
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
# 71 "parser.mly"
                                                                                                             (BodyNode (Some(v),i))
# 3770 "parser.ml"
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
# 70 "parser.mly"
                                                                                                                (BodyNode (None,i))
# 3794 "parser.ml"
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

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | SimpleQuoteToken ->
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

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
        | DoubleQuoteToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | EvenToken ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | FalseToken ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | HeadToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | IdentToken _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | IfToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LeftParenthesisToken ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LeftSqBracketToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | LetToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NumberToken _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | OddToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | SimpleQuoteToken ->
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
# 147 "parser.mly"
                                                                                                                          (TrueNode)
# 3907 "parser.ml"
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
        | DoubleQuoteToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | EvenToken ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | FalseToken ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | HeadToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | IdentToken _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | IfToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LeftParenthesisToken ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LeftSqBracketToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | LetToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | NumberToken _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
        | OddToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | SimpleQuoteToken ->
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
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | SimpleQuoteToken ->
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

and _menhir_run115 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                | DoubleQuoteToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | EvenToken ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | FalseToken ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | HeadToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | IdentToken _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | IfToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | LeftParenthesisToken ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | LeftSqBracketToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | LetToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | NumberToken _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | OddToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | SimpleQuoteToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
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

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SimpleQuoteToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (c : (
# 8 "parser.mly"
       (char)
# 4098 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 145 "parser.mly"
                                                                                                                          (CharNode (c))
# 4105 "parser.ml"
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

and _menhir_run121 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                | DoubleQuoteToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | EvenToken ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | FalseToken ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | HeadToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | IdentToken _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | IfToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | LeftParenthesisToken ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | LeftSqBracketToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | LetToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | NumberToken _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | OddToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | SimpleQuoteToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | SubToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | TailToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | TrueToken ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
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

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | DoubleQuoteToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | EvenToken ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | FalseToken ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | HeadToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | IdentToken _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | IfToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LeftParenthesisToken ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LeftSqBracketToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LetToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | NumberToken _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | OddToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | SimpleQuoteToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (int)
# 4251 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (
# 6 "parser.mly"
       (int)
# 4259 "parser.ml"
    )) = _v in
    let _v : (Ast.ast) = 
# 144 "parser.mly"
                                                                                                                              (IntegerNode(c))
# 4264 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 103 "parser.mly"
                                                                                                                        (NoopNode)
# 4276 "parser.ml"
     in
    _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_run128 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | SimpleQuoteToken ->
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

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | SimpleQuoteToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TrueToken ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

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
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
    | IfToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | LeftParenthesisToken ->
        _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | LetToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | SimpleQuoteToken ->
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
        | DoubleQuoteToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EvenToken ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | FalseToken ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | HeadToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | IdentToken _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | IfToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LeftParenthesisToken ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LeftSqBracketToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LetToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | NumberToken _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | OddToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SimpleQuoteToken ->
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

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 4452 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "parser.mly"
       (string)
# 4460 "parser.ml"
    )) = _v in
    let _v : (Ast.ast) = 
# 137 "parser.mly"
                                                                                                                                  (AssignNode (n))
# 4465 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

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
        | DoubleQuoteToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | EvenToken ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FalseToken ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | HeadToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | IdentToken _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | IfToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LeftParenthesisToken ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LeftSqBracketToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LetToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | NumberToken _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | OddToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SimpleQuoteToken ->
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

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 148 "parser.mly"
                                                                                                                          (FalseNode)
# 4529 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | DoubleQuoteToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | EvenToken ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | FalseToken ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | HeadToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | IdentToken _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | IfToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LeftParenthesisToken ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LeftSqBracketToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LetToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | NumberToken _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | OddToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | SimpleQuoteToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | SubToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TailToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TrueToken ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
# 4604 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 146 "parser.mly"
                                                                                                                          (StringNode (c))
# 4611 "parser.ml"
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
            | DoubleQuoteToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | EvenToken ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | FalseToken ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | HeadToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | IdentToken _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | IfToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | LeftParenthesisToken ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | LeftSqBracketToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | LetToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | NumberToken _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
            | OddToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState170
            | SimpleQuoteToken ->
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
# 4723 "parser.ml"
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
# 4746 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 78 "parser.mly"
                                                                                                            (TupleDeclaNode (n,None))
# 4751 "parser.ml"
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
# 83 "parser.mly"
                                                                                                             (VariableDeclasNode (v,None))
# 4790 "parser.ml"
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
    | ChooseToken ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | DoubleQuoteToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | EvenToken ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | FalseToken ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | HeadToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | IdentToken _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | IfToken ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | LeftParenthesisToken ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | LeftSqBracketToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | LetToken ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | NoopToken ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | NumberToken _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
    | OddToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | SendToken ->
        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | SimpleQuoteToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState192
    | SpawnToken ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState192
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
# 86 "parser.mly"
                                                                                                           (InstrSeqNode (bi,None))
# 4882 "parser.ml"
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
            | ChooseToken ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | DoubleQuoteToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | EvenToken ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | FalseToken ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | HeadToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | IdentToken _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | IfToken ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | LeftParenthesisToken ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | LeftSqBracketToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | LetToken ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | NoopToken ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | NumberToken _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
            | OddToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | SendToken ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | SimpleQuoteToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | SpawnToken ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState167
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
# 4976 "parser.ml"
        ))), _, (params : (Ast.ast))) = _menhir_stack in
        let _3 = () in
        let _v : (Ast.ast) = 
# 67 "parser.mly"
                                                                                                            (ParamsNode (t,n,Some(params)))
# 4982 "parser.ml"
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
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | DoubleQuoteToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | EvenToken ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | FalseToken ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | HeadToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | IdentToken _v ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
                | IfToken ->
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | IntegerToken ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | LeftParenthesisToken ->
                    _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | LeftSqBracketToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | LetToken ->
                    _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | ListToken ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | NoopToken ->
                    _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | NumberToken _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
                | OddToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | SendToken ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | SimpleQuoteToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | SpawnToken ->
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState28
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
# 165 "parser.mly"
                                                                                                                        (TupleTNode (tSeq))
# 5092 "parser.ml"
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
# 169 "parser.mly"
                                                                                                                      (TypeSeqNode (t1, Some(t2)))
# 5109 "parser.ml"
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
# 168 "parser.mly"
                                                                                                                      (TypeSeqNode (t,None))
# 5202 "parser.ml"
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
# 164 "parser.mly"
                                                                                                                          (ListTNode (t))
# 5227 "parser.ml"
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
# 173 "parser.mly"
                                                                                                                      (FuncTNode (Some (t)))
# 5243 "parser.ml"
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
# 5285 "parser.ml"
                ))) = _menhir_stack in
                let _v : (Ast.ast) = 
# 66 "parser.mly"
                                                                                                            (ParamsNode (t,n,None))
# 5290 "parser.ml"
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
# 113 "parser.mly"
                                                                                                                                    (PrefixNode(Some(a),New,None,Some(t)))
# 5323 "parser.ml"
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
# 102 "parser.mly"
                                                                                                                       (NewNode (a,t))
# 5349 "parser.ml"
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
# 5370 "parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (t : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 74 "parser.mly"
                                                                                                            (VariableDeclaNode (t,StringNode(n)))
# 5376 "parser.ml"
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
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
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
    | MenhirState67 ->
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
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
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
# 161 "parser.mly"
                                                                                                                          (TypeNode (StringT))
# 5661 "parser.ml"
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
# 159 "parser.mly"
                                                                                                                           (TypeNode (IntegerT))
# 5734 "parser.ml"
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
# 172 "parser.mly"
                                                                                                                      (FuncTNode (None))
# 5767 "parser.ml"
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
# 162 "parser.mly"
                                                                                                                          (TypeNode (CharT))
# 5783 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 163 "parser.mly"
                                                                                                                          (TypeNode (ChannT))
# 5795 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 160 "parser.mly"
                                                                                                                          (TypeNode (BooleanT))
# 5807 "parser.ml"
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
# 50 "parser.mly"
       (Ast.ast)
# 5826 "parser.ml"
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
  

# 5864 "parser.ml"
