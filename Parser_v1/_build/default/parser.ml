
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
# 29 "parser.ml"
  )
    | NewToken
    | MulToken
    | ListToken
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
# 44 "parser.ml"
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
    | DefToken
    | ComaToken
    | ChooseToken
    | ChoiceToken
    | CharValueToken of (
# 8 "parser.mly"
       (string)
# 64 "parser.ml"
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
  | MenhirState216
  | MenhirState212
  | MenhirState209
  | MenhirState201
  | MenhirState199
  | MenhirState196
  | MenhirState192
  | MenhirState190
  | MenhirState183
  | MenhirState176
  | MenhirState171
  | MenhirState161
  | MenhirState158
  | MenhirState154
  | MenhirState151
  | MenhirState137
  | MenhirState131
  | MenhirState127
  | MenhirState126
  | MenhirState124
  | MenhirState121
  | MenhirState118
  | MenhirState115
  | MenhirState111
  | MenhirState108
  | MenhirState105
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
  | MenhirState37
  | MenhirState35
  | MenhirState34
  | MenhirState31
  | MenhirState29
  | MenhirState25
  | MenhirState22
  | MenhirState17
  | MenhirState13
  | MenhirState7
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  
open Ast

# 153 "parser.ml"

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
# 152 "parser.mly"
                                                                                                                        (ValueNode (vs))
# 174 "parser.ml"
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
# 156 "parser.mly"
                                                                                                                        (ValueSeqNode (v,Some(vs)))
# 191 "parser.ml"
         in
        _menhir_goto_valueSeq _menhir_env _menhir_stack _menhir_s _v
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
# 205 "parser.ml"
    ))), _, (params : (Ast.ast))) = _menhir_stack in
    let _6 = () in
    let _4 = () in
    let _1 = () in
    let _v : (Ast.ast) = 
# 61 "parser.mly"
                                                                                                                            (FunctionNode (ft,n,params,b))
# 213 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BoolToken ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | ChannelToken ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | CharToken ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | FunctionToken ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | LeftParenthesisToken ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | StartToken ->
        _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | StringToken ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState216
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState216

and _menhir_goto_choices : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (p : (Ast.ast))), _, (i : (Ast.ast))), _, (cs : (Ast.ast))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ast.ast) = 
# 107 "parser.mly"
                                                                                                                          (ChoicesNode(p,i,Some(cs)))
# 257 "parser.ml"
         in
        _menhir_goto_choices _menhir_env _menhir_stack _menhir_s _v
    | MenhirState126 ->
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
# 276 "parser.ml"
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

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 45 "parser.mly"
      (Ast.ast)
# 291 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (p : (
# 45 "parser.mly"
      (Ast.ast)
# 302 "parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 45 "parser.mly"
      (Ast.ast)
# 307 "parser.ml"
        ) = 
# 56 "parser.mly"
                                                                                                             (ProgramNode (v,p))
# 311 "parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (f : (Ast.ast))), _, (p : (
# 45 "parser.mly"
      (Ast.ast)
# 320 "parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 45 "parser.mly"
      (Ast.ast)
# 325 "parser.ml"
        ) = 
# 55 "parser.mly"
                                                                                                             (ProgramNode (f,p))
# 329 "parser.ml"
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
# 45 "parser.mly"
      (Ast.ast)
# 343 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 48 "parser.mly"
       (Ast.ast)
# 349 "parser.ml"
            ) = 
# 52 "parser.mly"
                         (a)
# 353 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 48 "parser.mly"
       (Ast.ast)
# 360 "parser.ml"
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

and _menhir_run161 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CallToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState161 in
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
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
                | IfToken ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | NumberToken _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
                | OddToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | SubToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | TailToken ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | TrueToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171)
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
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | NewToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState161 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RightParenthesisToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (a : (Ast.ast))), _) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _v : (Ast.ast) = 
# 102 "parser.mly"
                                                                                                                    (NewNode (a))
# 481 "parser.ml"
                 in
                _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | ReceiveToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState161 in
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
# 525 "parser.ml"
                    ))) = _menhir_stack in
                    let _6 = () in
                    let _4 = () in
                    let _3 = () in
                    let _2 = () in
                    let _v : (Ast.ast) = 
# 94 "parser.mly"
                                                                                                                (ReceiveNode (a,n))
# 534 "parser.ml"
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
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161

and _menhir_goto_exprs : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState51 ->
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
# 585 "parser.ml"
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
# 141 "parser.mly"
                                                                                                                                   (ExprsNode (e1,Some(e2)))
# 602 "parser.ml"
         in
        _menhir_goto_exprs _menhir_env _menhir_stack _menhir_s _v
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 617 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 101 "parser.mly"
                                                                                                                          (SpawnNode (f,e))
# 625 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 646 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 93 "parser.mly"
                                                                                                                    (CallNode (f,e))
# 654 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (a : (Ast.ast))), _), (f : (
# 7 "parser.mly"
       (string)
# 675 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 92 "parser.mly"
                                                                                                                   (BinaryNode (a,Assign,CallNode (f,e)))
# 684 "parser.ml"
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
            | NumberToken _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | OddToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | SubToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | TailToken ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | TrueToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

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
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | ChooseToken ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | IfToken ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | NumberToken _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | OddToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | ReturnToken ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | SendToken ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | SequenceToken ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | SpawnToken ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | SubToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | TailToken ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | TrueToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | WhileToken ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | RightBracketToken ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
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
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | TrueToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | RightSqBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 155 "parser.mly"
                                                                                                                        (ValueSeqNode (v,None))
# 1250 "parser.ml"
             in
            _menhir_goto_valueSeq _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState212 | MenhirState29 | MenhirState192 | MenhirState105 | MenhirState111 | MenhirState183 | MenhirState124 | MenhirState176 | MenhirState161 | MenhirState171 | MenhirState151 | MenhirState154 | MenhirState127 | MenhirState137 | MenhirState131 | MenhirState121 | MenhirState118 | MenhirState115 | MenhirState108 | MenhirState31 | MenhirState34 | MenhirState35 | MenhirState37 | MenhirState96 | MenhirState51 | MenhirState90 | MenhirState86 | MenhirState53 | MenhirState56 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 136 "parser.mly"
                                                                                                                                  (ValueNode (v))
# 1266 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState151 ->
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
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState158
            | RightBracketToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (p : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _2 = () in
                let _v : (Ast.ast) = 
# 108 "parser.mly"
                                                                                                                       (ChoicesNode(p,i,None))
# 1297 "parser.ml"
                 in
                _menhir_goto_choices _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (bi : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 88 "parser.mly"
                                                                                                           (InstrSeqNode (bi,Some(i)))
# 1318 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState124 ->
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
                        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | CharValueToken _v ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
                    | ChooseToken ->
                        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | DoubleQuoteToken ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | EvenToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | FalseToken ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | HeadToken ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | IdentToken _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
                    | IfToken ->
                        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | LeftParenthesisToken ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | LeftSqBracketToken ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | NumberToken _v ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
                    | OddToken ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | ReturnToken ->
                        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | SendToken ->
                        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | SequenceToken ->
                        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | SpawnToken ->
                        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | SubToken ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | TailToken ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | TrueToken ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | WhileToken ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | RightBracketToken ->
                        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState183
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
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
    | MenhirState183 ->
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
# 1428 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (i : (Ast.ast))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast) = 
# 86 "parser.mly"
                                                                                                           (InstrSeqNode(NoopNode,Some(i)))
# 1445 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState105 ->
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
# 1466 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _), _, (v : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 69 "parser.mly"
                                                                                                                (BodyNode (Some(v),i))
# 1492 "parser.ml"
             in
            _menhir_goto_body _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
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
# 68 "parser.mly"
                                                                                                                (BodyNode (None,i))
# 1516 "parser.ml"
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

and _menhir_goto_binstruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SequenceToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CallToken ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | CharValueToken _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
        | ChooseToken ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
        | IfToken ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | NumberToken _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
        | OddToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | ReturnToken ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | SendToken ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | SequenceToken ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | SpawnToken ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | SubToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | TailToken ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | TrueToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | WhileToken ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | RightBracketToken ->
            _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
    | RightBracketToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (bi : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 87 "parser.mly"
                                                                                                           (InstrSeqNode (bi,None))
# 1594 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
# 122 "parser.mly"
                                                                                                             (UnaryNode (Even,e))
# 1642 "parser.ml"
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
        | AddToken | AndToken | AssignToken | ComaToken | DifferentToken | EqualToken | GreaterToken | LesserToken | OrToken | RightBracketToken | RightParenthesisToken | SequenceToken | SubToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 124 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Substract,e2))
# 1669 "parser.ml"
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
# 126 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Multiply,e2))
# 1686 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 127 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Divide,e2))
# 1697 "parser.ml"
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
        | AssignToken | ComaToken | RightBracketToken | RightParenthesisToken | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 125 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Or,e2))
# 1732 "parser.ml"
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
        | AndToken | AssignToken | ComaToken | DifferentToken | EqualToken | OrToken | RightBracketToken | RightParenthesisToken | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 131 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Lesser,e2))
# 1761 "parser.ml"
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
        | AddToken | AndToken | AssignToken | ComaToken | DifferentToken | EqualToken | GreaterToken | LesserToken | OrToken | RightBracketToken | RightParenthesisToken | SequenceToken | SubToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 123 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Add,e2))
# 1786 "parser.ml"
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
        | AndToken | AssignToken | ComaToken | DifferentToken | EqualToken | OrToken | RightBracketToken | RightParenthesisToken | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 132 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Greater,e2))
# 1815 "parser.ml"
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
        | AndToken | AssignToken | ComaToken | DifferentToken | EqualToken | OrToken | RightBracketToken | RightParenthesisToken | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 129 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Equal,e2))
# 1848 "parser.ml"
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
        | AndToken | AssignToken | ComaToken | DifferentToken | EqualToken | OrToken | RightBracketToken | RightParenthesisToken | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 130 "parser.mly"
                                                                                                                                      (BinaryNode (e1,Different,e2))
# 1881 "parser.ml"
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
        | AssignToken | ComaToken | OrToken | RightBracketToken | RightParenthesisToken | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 128 "parser.mly"
                                                                                                                                      (BinaryNode (e1,And,e2))
# 1920 "parser.ml"
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
# 119 "parser.mly"
                                                                                                             (UnaryNode (Head,e))
# 1963 "parser.ml"
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
                | NumberToken _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                | OddToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | SubToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | TailToken ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | TrueToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
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
# 134 "parser.mly"
                                                                                                                                  (IfthenelseExprNode (cond,e1,e2))
# 2125 "parser.ml"
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
    | MenhirState171 | MenhirState154 | MenhirState108 | MenhirState96 | MenhirState51 ->
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
            | NumberToken _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | OddToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | SubToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | TailToken ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | TrueToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96
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
# 140 "parser.mly"
                                                                                                                                   (ExprsNode (e,None))
# 2204 "parser.ml"
             in
            _menhir_goto_exprs _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
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
                                                                                                            (UnaryNode (Odd,e))
# 2247 "parser.ml"
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
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DivToken ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AddToken | AndToken | AssignToken | ComaToken | DifferentToken | EqualToken | GreaterToken | LesserToken | OrToken | RightBracketToken | RightParenthesisToken | SequenceToken | SubToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast) = 
# 118 "parser.mly"
                                                                 (UnaryNode (Negate,e))
# 2274 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
                                                                                                             (UnaryNode (Tail,e))
# 2317 "parser.ml"
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
    | MenhirState31 ->
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
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
                | ChooseToken ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
                | IfToken ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | NumberToken _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
                | OddToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | ReturnToken ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | SendToken ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | SequenceToken ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | SpawnToken ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | SubToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | TailToken ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | TrueToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | WhileToken ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | RightBracketToken ->
                    _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
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
    | MenhirState115 ->
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
# 2453 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 95 "parser.mly"
                                                                                                                     (SendNode (n,e))
# 2462 "parser.ml"
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
    | MenhirState118 ->
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
        | RightBracketToken | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ast) = 
# 104 "parser.mly"
                                                                                                                        (ReturnNode (Some (e)))
# 2505 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 ->
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
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | ChooseToken ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | IfToken ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | NumberToken _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | OddToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | ReturnToken ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | SendToken ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | SequenceToken ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | SpawnToken ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | SubToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | TailToken ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | TrueToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | WhileToken ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | RightBracketToken ->
                    _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
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
    | MenhirState131 ->
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
# 2639 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 115 "parser.mly"
                                                                                                                              (PrefixNode(None,Spawn,Some(f),Some(e)))
# 2648 "parser.ml"
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
    | MenhirState137 ->
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
# 2689 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 112 "parser.mly"
                                                                                                                                     (PrefixNode(None,Send,Some(n),Some(e)))
# 2699 "parser.ml"
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
    | MenhirState127 ->
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
                    | RightParenthesisToken ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s), _, (a : (Ast.ast))) = _menhir_stack in
                        let _6 = () in
                        let _5 = () in
                        let _4 = () in
                        let _3 = () in
                        let _1 = () in
                        let _v : (Ast.ast) = 
# 114 "parser.mly"
                                                                                                                                    (PrefixNode(Some(a),New,None,None))
# 2747 "parser.ml"
                         in
                        _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
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
# 2785 "parser.ml"
                            ))) = _menhir_stack in
                            let _7 = () in
                            let _5 = () in
                            let _4 = () in
                            let _3 = () in
                            let _1 = () in
                            let _v : (Ast.ast) = 
# 113 "parser.mly"
                                                                                                                                   (PrefixNode(Some(a),Receive,Some(n),None))
# 2795 "parser.ml"
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
    | MenhirState29 | MenhirState192 | MenhirState105 | MenhirState111 | MenhirState183 | MenhirState176 | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState161 ->
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
        | RightBracketToken | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Ast.ast))), _, (e : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 91 "parser.mly"
                                                                                                                  (BinaryNode (a,Assign,e))
# 2909 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState212 ->
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
# 2983 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 176 "parser.mly"
                                                                                                                       (CallNode (f,e))
# 2991 "parser.ml"
             in
            (match _menhir_s with
            | MenhirState216 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c : (Ast.ast)) = _v in
                let (_menhir_stack, _menhir_s, (f : (Ast.ast))) = _menhir_stack in
                let _v : (
# 45 "parser.mly"
      (Ast.ast)
# 3002 "parser.ml"
                ) = 
# 57 "parser.mly"
                                                                                                                 (ProgramNode (f,c))
# 3006 "parser.ml"
                 in
                _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
            | MenhirState209 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c : (Ast.ast)) = _v in
                let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
                let _v : (
# 45 "parser.mly"
      (Ast.ast)
# 3017 "parser.ml"
                ) = 
# 58 "parser.mly"
                                                                                                                 (ProgramNode (v,c))
# 3021 "parser.ml"
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

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | SendToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState127 in
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
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
                    | DoubleQuoteToken ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | EvenToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | FalseToken ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | HeadToken ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | IdentToken _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
                    | IfToken ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | LeftParenthesisToken ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | LeftSqBracketToken ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | NumberToken _v ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
                    | OddToken ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | SubToken ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | TailToken ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | TrueToken ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
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
        let _menhir_s = MenhirState127 in
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
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
                | IfToken ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | NumberToken _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
                | OddToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | SubToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | TailToken ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | TrueToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
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
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | TauToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState127 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.ast) = 
# 111 "parser.mly"
                                                                                                                                       (PrefixNode(None,Tau,None,None))
# 3215 "parser.ml"
         in
        _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_goto_cst : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (Ast.ast)) = _v in
    let _v : (Ast.ast) = 
# 151 "parser.mly"
                                                                                                                        (ValueNode (c))
# 3233 "parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

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
        | NumberToken _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | OddToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SubToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | TailToken ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | TrueToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53
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
    | MenhirState201 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (n : (
# 7 "parser.mly"
       (string)
# 3297 "parser.ml"
        ))), _, (idents : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 77 "parser.mly"
                                                                                                            (TupleDeclaNode (n,Some(idents)))
# 3303 "parser.ml"
         in
        _menhir_goto_tupleDecla _menhir_env _menhir_stack _menhir_s _v
    | MenhirState199 ->
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
# 73 "parser.mly"
                                                                                                            (VariableDeclaNode (t,idents))
# 3321 "parser.ml"
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

and _menhir_run210 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
            | IfToken ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | NumberToken _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v
            | OddToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | SubToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | TailToken ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | TrueToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState212
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212)
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
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | InToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CallToken ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | CharValueToken _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
            | ChooseToken ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState192
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
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | NumberToken _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
            | OddToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | ReturnToken ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SendToken ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SequenceToken ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SpawnToken ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SubToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | TailToken ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | TrueToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | WhileToken ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | RightBracketToken ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (vs : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 82 "parser.mly"
                                                                                                             (VariableDeclasNode (v,Some(vs)))
# 3471 "parser.ml"
         in
        _menhir_goto_variableDeclas _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ast) = 
# 85 "parser.mly"
                                                                                                           (InstrSeqNode(NoopNode,None))
# 3482 "parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | NumberToken _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | OddToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | SubToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | TailToken ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | TrueToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 147 "parser.mly"
                                                                                                                          (TrueNode)
# 3544 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | NumberToken _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | OddToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | SubToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | TailToken ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | TrueToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
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
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | IfToken ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NumberToken _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | OddToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | SubToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TailToken ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TrueToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
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

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CallToken ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | ChooseToken ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IfToken ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ReturnToken ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SendToken ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SequenceToken ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SpawnToken ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | WhileToken ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | RightBracketToken ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                | IfToken ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | NumberToken _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                | OddToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | SubToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | TailToken ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | TrueToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
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

and _menhir_run118 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CharValueToken _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | DoubleQuoteToken ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | EvenToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | FalseToken ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | HeadToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | IdentToken _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | IfToken ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | LeftParenthesisToken ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | LeftSqBracketToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | RightBracketToken | SequenceToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast) = 
# 103 "parser.mly"
                                                                                                                        (ReturnNode (None))
# 3868 "parser.ml"
         in
        _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | NumberToken _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | OddToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | SubToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TailToken ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TrueToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (int)
# 3929 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (
# 6 "parser.mly"
       (int)
# 3937 "parser.ml"
    )) = _v in
    let _v : (Ast.ast) = 
# 144 "parser.mly"
                                                                                                                          (IntegerNode(c))
# 3942 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

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
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

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
    | NumberToken _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | OddToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SubToken ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TailToken ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TrueToken ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run120 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | DoubleQuoteToken ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | EvenToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | FalseToken ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | HeadToken ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | IdentToken _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | IfToken ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | LeftParenthesisToken ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | LeftSqBracketToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | NumberToken _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | OddToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | SubToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | TailToken ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | TrueToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 4061 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "parser.mly"
       (string)
# 4069 "parser.ml"
    )) = _v in
    let _v : (Ast.ast) = 
# 137 "parser.mly"
                                                                                                                                  (AssignNode (n))
# 4074 "parser.ml"
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
        | NumberToken _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | OddToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SubToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TailToken ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TrueToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
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
# 148 "parser.mly"
                                                                                                                          (FalseNode)
# 4136 "parser.ml"
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
        | NumberToken _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | OddToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SubToken ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TailToken ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TrueToken ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
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
# 4209 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 146 "parser.mly"
                                                                                                                          (StringNode (c))
# 4216 "parser.ml"
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

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 4259 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (
# 8 "parser.mly"
       (string)
# 4267 "parser.ml"
    )) = _v in
    let _v : (Ast.ast) = 
# 145 "parser.mly"
                                                                                                                         (CharNode (c))
# 4272 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run152 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | DoubleQuoteToken ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | EvenToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | FalseToken ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | HeadToken ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | IdentToken _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | IfToken ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LeftParenthesisToken ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LeftSqBracketToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | NumberToken _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | OddToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | SubToken ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | TailToken ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | TrueToken ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
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

and _menhir_run200 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 4346 "parser.ml"
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
            _menhir_run200 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201)
    | RightParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (n : (
# 7 "parser.mly"
       (string)
# 4369 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 76 "parser.mly"
                                                                                                            (TupleDeclaNode (n,None))
# 4374 "parser.ml"
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
    | MenhirState196 | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BoolToken ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | ChannelToken ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | CharToken ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | IntegerToken ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | LeftParenthesisToken ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | ListToken ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | StringToken ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | InToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
                let _2 = () in
                let _v : (Ast.ast) = 
# 81 "parser.mly"
                                                                                                             (VariableDeclasNode (v,None))
# 4419 "parser.ml"
                 in
                _menhir_goto_variableDeclas _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196)
        | InToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 80 "parser.mly"
                                                                                                             (VariableDeclasNode (v,None))
# 4432 "parser.ml"
             in
            _menhir_goto_variableDeclas _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState216 | MenhirState209 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BoolToken ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | ChannelToken ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | CharToken ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | FunctionToken ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | IntegerToken ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | LeftParenthesisToken ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | ListToken ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | StartToken ->
            _menhir_run210 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | StringToken ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209)
    | _ ->
        _menhir_fail ()

and _menhir_goto_parameters : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (t : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 4481 "parser.ml"
        ))), _, (params : (Ast.ast))) = _menhir_stack in
        let _3 = () in
        let _v : (Ast.ast) = 
# 65 "parser.mly"
                                                                                                            (ParamsNode (t,n,Some(params)))
# 4487 "parser.ml"
         in
        _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
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
                | CallToken ->
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | CharValueToken _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
                | ChooseToken ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | DefToken ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState29 in
                    let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BoolToken ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                    | ChannelToken ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                    | CharToken ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                    | IntegerToken ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                    | LeftParenthesisToken ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                    | ListToken ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                    | StringToken ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190)
                | DoubleQuoteToken ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | EvenToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | FalseToken ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | HeadToken ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | IdentToken _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
                | IfToken ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | LeftParenthesisToken ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | LeftSqBracketToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | NumberToken _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
                | OddToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | ReturnToken ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | SendToken ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | SequenceToken ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | SpawnToken ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | SubToken ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | TailToken ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | TrueToken ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | WhileToken ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | RightBracketToken ->
                    _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
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
    | MenhirState4 ->
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
# 4612 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t1 : (Ast.ast))), _, (t2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 169 "parser.mly"
                                                                                                                      (TypeSeqNode (t1, Some(t2)))
# 4629 "parser.ml"
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
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | ChannelToken ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | CharToken ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | IntegerToken ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | LeftParenthesisToken ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | ListToken ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | StringToken ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
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
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (t : (Ast.ast))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast) = 
# 163 "parser.mly"
                                                                                                                          (ChanTNode (t))
# 4696 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState13 | MenhirState4 ->
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
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | ChannelToken ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | CharToken ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | IntegerToken ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | LeftParenthesisToken ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | ListToken ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | StringToken ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 168 "parser.mly"
                                                                                                                      (TypeSeqNode (t,None))
# 4733 "parser.ml"
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
# 4758 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (t : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 173 "parser.mly"
                                                                                                                      (FuncTNode (Some (t)))
# 4774 "parser.ml"
         in
        _menhir_goto_funcType _menhir_env _menhir_stack _menhir_s _v
    | MenhirState25 | MenhirState22 ->
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
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | ChannelToken ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | CharToken ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | IntegerToken ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | LeftParenthesisToken ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | ListToken ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | StringToken ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
            | RightParenthesisToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (t : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 4816 "parser.ml"
                ))) = _menhir_stack in
                let _v : (Ast.ast) = 
# 64 "parser.mly"
                                                                                                            (ParamsNode (t,n,None))
# 4821 "parser.ml"
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
    | MenhirState0 | MenhirState216 | MenhirState209 | MenhirState190 | MenhirState196 ->
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
# 4848 "parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (t : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 72 "parser.mly"
                                                                                                            (VariableDeclaNode (t,StringNode(n)))
# 4854 "parser.ml"
             in
            _menhir_goto_variableDecla _menhir_env _menhir_stack _menhir_s _v
        | LeftParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IdentToken _v ->
                _menhir_run200 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
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
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState201 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
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
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
# 5119 "parser.ml"
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
# 5192 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BoolToken ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ChannelToken ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | CharToken ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LeftParenthesisToken ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | StringToken ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | VoidToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState17 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast) = 
# 172 "parser.mly"
                                                                                                                      (FuncTNode (None))
# 5225 "parser.ml"
         in
        _menhir_goto_funcType _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 162 "parser.mly"
                                                                                                                          (TypeNode (CharT))
# 5241 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BoolToken ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ChannelToken ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | CharToken ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LeftParenthesisToken ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | StringToken ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 160 "parser.mly"
                                                                                                                          (TypeNode (BooleanT))
# 5278 "parser.ml"
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
# 48 "parser.mly"
       (Ast.ast)
# 5297 "parser.ml"
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
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
  

# 5335 "parser.ml"
