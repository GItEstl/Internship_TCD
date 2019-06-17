
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
# 28 "parser.ml"
  )
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
    | ComaToken
    | ChooseToken
    | CharValueToken of (
# 8 "parser.mly"
       (char)
# 62 "parser.ml"
  )
    | CharToken
    | ChannelToken
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
  | MenhirState211
  | MenhirState207
  | MenhirState205
  | MenhirState202
  | MenhirState198
  | MenhirState189
  | MenhirState180
  | MenhirState176
  | MenhirState174
  | MenhirState171
  | MenhirState166
  | MenhirState162
  | MenhirState161
  | MenhirState157
  | MenhirState153
  | MenhirState149
  | MenhirState145
  | MenhirState140
  | MenhirState136
  | MenhirState134
  | MenhirState131
  | MenhirState129
  | MenhirState126
  | MenhirState124
  | MenhirState121
  | MenhirState117
  | MenhirState113
  | MenhirState108
  | MenhirState104
  | MenhirState100
  | MenhirState95
  | MenhirState91
  | MenhirState87
  | MenhirState74
  | MenhirState70
  | MenhirState68
  | MenhirState63
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState47
  | MenhirState45
  | MenhirState44
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState27
  | MenhirState24
  | MenhirState19
  | MenhirState15
  | MenhirState11
  | MenhirState7
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  
open Ast

# 147 "parser.ml"

let rec _menhir_goto_choices : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (Ast.ast))), _, (i : (Ast.ast))), _, (cs : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 94 "parser.mly"
                                                                                                                      (ChoicesNode(i,Some(cs)))
# 161 "parser.ml"
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
# 89 "parser.mly"
                                                                                                                  (ChooseNode (c))
# 180 "parser.ml"
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

and _menhir_goto_instrSeq : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState33 | MenhirState113 | MenhirState129 | MenhirState134 | MenhirState189 | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (iSeq : (Ast.ast)) = _v in
        let _v : (Ast.ast) = 
# 72 "parser.mly"
                                                                                                            (InstrNode (iSeq))
# 202 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (iSeq : (Ast.ast)) = _v in
        let (_menhir_stack, _menhir_s, (bi : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 76 "parser.mly"
                                                                                                             (InstrSeqNode (bi,Some(iSeq)))
# 214 "parser.ml"
         in
        _menhir_goto_instrSeq _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NewToken ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | ReceiveToken ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | SendToken ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | SpawnToken ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | TauToken ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 95 "parser.mly"
                                                                                                                      (ChoicesNode(i,None))
# 246 "parser.ml"
             in
            _menhir_goto_choices _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162)
    | MenhirState134 ->
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
                    | ChooseToken ->
                        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | IdentToken _v ->
                        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                    | IfToken ->
                        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | LeftParenthesisToken ->
                        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | LetToken ->
                        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | SendToken ->
                        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | SpawnToken ->
                        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | WhileToken ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189)
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
    | MenhirState189 ->
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
# 86 "parser.mly"
                                                                                                                (IfthenelseNode (cond,i1,i2))
# 332 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 ->
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
# 87 "parser.mly"
                                                                                                               (LetInNode (a,e,i))
# 359 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
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
# 88 "parser.mly"
                                                                                                                              (WhileNode (e,i))
# 386 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
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
        | RightBracketToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _, (v : (Ast.ast))), _, (i : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 60 "parser.mly"
                                                                                                             (BodyNode (v,i))
# 411 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (b : (Ast.ast)) = _v in
            let ((((_menhir_stack, _menhir_s), _, (ft : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 419 "parser.ml"
            ))), _, (params : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 53 "parser.mly"
                                                                                                                            (FunctionNode (ft,n,params,b))
# 427 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BoolToken ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState211
            | ChannelToken ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState211
            | CharToken ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState211
            | FunctionToken ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState211
            | IdentToken _v ->
                _menhir_run206 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _v
            | IntegerToken ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState211
            | LeftParenthesisToken ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState211
            | ListToken ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState211
            | StringToken ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState211
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState211)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run166 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ChooseToken ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | IdentToken _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
    | IfToken ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | LeftParenthesisToken ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | LetToken ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | SendToken ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | SpawnToken ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | WhileToken ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166

and _menhir_goto_opBinary : _menhir_env -> 'ttv_tail -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | EvenToken ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | FalseToken ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | HeadToken ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | IfToken ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | LeftParenthesisToken ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | LeftSqBracketToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | LetToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NumberToken _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | OddToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | SimpleQuoteToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | SubToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TailToken ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TrueToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 37 "parser.mly"
      (Ast.ast)
# 534 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (p : (
# 37 "parser.mly"
      (Ast.ast)
# 545 "parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 37 "parser.mly"
      (Ast.ast)
# 550 "parser.ml"
        ) = 
# 48 "parser.mly"
                                                                                                             (ProgramNode (v,p))
# 554 "parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
    | MenhirState211 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (f : (Ast.ast))), _, (p : (
# 37 "parser.mly"
      (Ast.ast)
# 563 "parser.ml"
        ))) = _menhir_stack in
        let _v : (
# 37 "parser.mly"
      (Ast.ast)
# 568 "parser.ml"
        ) = 
# 47 "parser.mly"
                                                                                                             (ProgramNode (f,p))
# 572 "parser.ml"
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
# 37 "parser.mly"
      (Ast.ast)
# 586 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 40 "parser.mly"
       (Ast.ast)
# 592 "parser.ml"
            ) = 
# 44 "parser.mly"
                         (a)
# 596 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 40 "parser.mly"
       (Ast.ast)
# 603 "parser.ml"
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

and _menhir_goto_binstruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 | MenhirState113 | MenhirState129 | MenhirState134 | MenhirState189 | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SequenceToken ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack)
        | NewToken | ReceiveToken | RightBracketToken | SendToken | SpawnToken | TauToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (bi : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 71 "parser.mly"
                                                                                                            (InstrNode (bi))
# 632 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SequenceToken ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack)
        | NewToken | ReceiveToken | RightBracketToken | SendToken | SpawnToken | TauToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (bi : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 75 "parser.mly"
                                                                                                             (InstrSeqNode (bi,None))
# 654 "parser.ml"
             in
            _menhir_goto_instrSeq _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_exprs : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 110 "parser.mly"
                                                                                                                                  (ExprsNode (e1,Some (e2)))
# 686 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 115 "parser.mly"
                                                                                                                                   (ExprsNode (e1,Some(e2)))
# 703 "parser.ml"
         in
        _menhir_goto_exprs _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run76 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 126 "parser.mly"
                                                                                                                               (OpBinaryNode (Substract))
# 717 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run77 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 127 "parser.mly"
                                                                                                                              (OpBinaryNode (Or))
# 729 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run78 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 128 "parser.mly"
                                                                                                                            (OpBinaryNode (Multiply))
# 741 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run79 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 133 "parser.mly"
                                                                                                                                 (OpBinaryNode (Lesser))
# 753 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run80 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 134 "parser.mly"
                                                                                                                                 (OpBinaryNode (Greater))
# 765 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run81 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 131 "parser.mly"
                                                                                                                               (OpBinaryNode (Equal))
# 777 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run82 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 129 "parser.mly"
                                                                                                                               (OpBinaryNode (Divide))
# 789 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run83 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 132 "parser.mly"
                                                                                                                                (OpBinaryNode (Different))
# 801 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run84 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 135 "parser.mly"
                                                                                                                                  (OpBinaryNode (Assign))
# 813 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 130 "parser.mly"
                                                                                                                               (OpBinaryNode (And))
# 825 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

and _menhir_run86 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 125 "parser.mly"
                                                                                                                               (OpBinaryNode (Add))
# 837 "parser.ml"
     in
    _menhir_goto_opBinary _menhir_env _menhir_stack _v

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
        | ChooseToken ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | IdentToken _v ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | IfToken ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | LeftParenthesisToken ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | LetToken ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SendToken ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SpawnToken ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | WhileToken ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
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
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | BoolToken | ChannelToken | CharToken | ComaToken | FunctionToken | IdentToken _ | InToken | IntegerToken | LeftParenthesisToken | ListToken | NewToken | ReceiveToken | RightBracketToken | RightParenthesisToken | SendToken | SequenceToken | SpawnToken | StringToken | TauToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (op : (Ast.ast))), _, (e : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 105 "parser.mly"
                                                                     (UnaryNode (op,e))
# 917 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | BoolToken | ChannelToken | CharToken | ComaToken | FunctionToken | IdentToken _ | InToken | IntegerToken | LeftParenthesisToken | ListToken | NewToken | ReceiveToken | RightBracketToken | RightParenthesisToken | SendToken | SequenceToken | SpawnToken | StringToken | TauToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.ast))), (op : (Ast.ast))), _, (e2 : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 106 "parser.mly"
                                                                          (BinaryNode (e1,op,e2))
# 959 "parser.ml"
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | EvenToken ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | FalseToken ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | HeadToken ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | IfToken ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | LeftParenthesisToken ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | LeftSqBracketToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | LetToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | NumberToken _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
                | OddToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | SimpleQuoteToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | SubToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | TailToken ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | TrueToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
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
                    | DoubleQuoteToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | EvenToken ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | FalseToken ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | HeadToken ->
                        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | IfToken ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | LeftParenthesisToken ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | LeftSqBracketToken ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | LetToken ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | NumberToken _v ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                    | OddToken ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | SimpleQuoteToken ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | SubToken ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | TailToken ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | TrueToken ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState95
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
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
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
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
# 109 "parser.mly"
                                                                                                                                  (IfthenelseNode (cond,e1,e2))
# 1182 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | ComaToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DoubleQuoteToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | EvenToken ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | FalseToken ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | HeadToken ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | IfToken ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LeftParenthesisToken ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LeftSqBracketToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LetToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | NumberToken _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | OddToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | SimpleQuoteToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | SubToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | TailToken ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | TrueToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | ComaToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DoubleQuoteToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | EvenToken ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | FalseToken ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | HeadToken ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | IfToken ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LeftParenthesisToken ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LeftSqBracketToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LetToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | NumberToken _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | OddToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | SimpleQuoteToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | SubToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | TailToken ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | TrueToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 114 "parser.mly"
                                                                                                                                   (ExprsNode (e,None))
# 1333 "parser.ml"
             in
            _menhir_goto_exprs _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | InToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LeftParenthesisToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DoubleQuoteToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | EvenToken ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | FalseToken ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | HeadToken ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | IfToken ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | LeftParenthesisToken ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | LeftSqBracketToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | LetToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | NumberToken _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
                | OddToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | SimpleQuoteToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | SubToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | TailToken ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | TrueToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
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
# 107 "parser.mly"
                                                                                                                                 (LetInNode (a,e1,e2))
# 1461 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
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
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | IdentToken _v ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
                | IfToken ->
                    _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | LeftParenthesisToken ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | LetToken ->
                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | SendToken ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | SpawnToken ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | WhileToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 1573 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 90 "parser.mly"
                                                                                                                          (SpawnNode (f,e))
# 1582 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 83 "parser.mly"
                                                                                                                (SendNode (e))
# 1629 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
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
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | IdentToken _v ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
                | IfToken ->
                    _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | LeftParenthesisToken ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | LetToken ->
                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | SendToken ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | SpawnToken ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | WhileToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState129
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
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
                | ChooseToken ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | IdentToken _v ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
                | IfToken ->
                    _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | LeftParenthesisToken ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | LetToken ->
                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | SendToken ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | SpawnToken ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | WhileToken ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (f : (
# 7 "parser.mly"
       (string)
# 1810 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 81 "parser.mly"
                                                                                                                  (CallNode (None,f,e))
# 1817 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (f : (
# 7 "parser.mly"
       (string)
# 1860 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 102 "parser.mly"
                                                                                                                            (PrefixNode(Spawn,Some(f),Some(e)))
# 1869 "parser.ml"
             in
            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 99 "parser.mly"
                                                                                                                             (PrefixNode(Send,None,Some(e)))
# 1916 "parser.ml"
             in
            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 100 "parser.mly"
                                                                                                                             (PrefixNode(Receive,None,Some(e)))
# 1963 "parser.ml"
             in
            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 101 "parser.mly"
                                                                                                                             (PrefixNode(New,None,Some(e)))
# 2010 "parser.ml"
             in
            _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Ast.ast))), _, (e : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 91 "parser.mly"
                                                                                                                  (NewNode (a,e))
# 2057 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.ast))), _), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 82 "parser.mly"
                                                                                                                (ReceiveNode (a,e))
# 2105 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.ast))), _, (f : (
# 7 "parser.mly"
       (string)
# 2148 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 80 "parser.mly"
                                                                                                                  (CallNode (Some(a),f,e))
# 2156 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | NewToken | ReceiveToken | RightBracketToken | SendToken | SequenceToken | SpawnToken | TauToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Ast.ast))), _, (e : (Ast.ast))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ast) = 
# 79 "parser.mly"
                                                                                                              (BinaryNode (a,OpBinaryNode(Assign),e))
# 2201 "parser.ml"
             in
            _menhir_goto_binstruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | BoolToken | ChannelToken | CharToken | FunctionToken | IdentToken _ | IntegerToken | LeftParenthesisToken | ListToken | SequenceToken | StringToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 2242 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _3 = () in
            let _v : (Ast.ast) = 
# 64 "parser.mly"
                                                                                                            (VariableDeclaNode (t,n,Some(e)))
# 2248 "parser.ml"
             in
            _menhir_goto_variableDecla _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AddToken ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | AndToken ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | AssignToken ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | DifferentToken ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DivToken ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | EqualToken ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GreaterToken ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LesserToken ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | MulToken ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OrToken ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (f : (
# 7 "parser.mly"
       (string)
# 2289 "parser.ml"
            ))), _, (e : (Ast.ast))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.ast) = 
# 180 "parser.mly"
                                                                                                                  (CallNode (None,f,e))
# 2296 "parser.ml"
             in
            (match _menhir_s with
            | MenhirState211 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c : (Ast.ast)) = _v in
                let (_menhir_stack, _menhir_s, (f : (Ast.ast))) = _menhir_stack in
                let _v : (
# 37 "parser.mly"
      (Ast.ast)
# 2307 "parser.ml"
                ) = 
# 49 "parser.mly"
                                                                                                                 (ProgramNode (f,c))
# 2311 "parser.ml"
                 in
                _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
            | MenhirState205 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c : (Ast.ast)) = _v in
                let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
                let _v : (
# 37 "parser.mly"
      (Ast.ast)
# 2322 "parser.ml"
                ) = 
# 50 "parser.mly"
                                                                                                                 (ProgramNode (v,c))
# 2326 "parser.ml"
                 in
                _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | SubToken ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_valueSeq : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (vs : (Ast.ast))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 147 "parser.mly"
                                                                                                                        (ValueNode (vs))
# 2361 "parser.ml"
             in
            _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (vs : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 152 "parser.mly"
                                                                                                                        (ValueSeqNode (v,Some(vs)))
# 2378 "parser.ml"
         in
        _menhir_goto_valueSeq _menhir_env _menhir_stack _menhir_s _v
    | MenhirState54 ->
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
# 148 "parser.mly"
                                                                                                                        (ValueNode (vs))
# 2396 "parser.ml"
             in
            _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v
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
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 98 "parser.mly"
                                                                                                                           (PrefixNode(Tau,None,None))
# 2416 "parser.ml"
     in
    _menhir_goto_prefix _menhir_env _menhir_stack _menhir_s _v

and _menhir_run142 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | EvenToken ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | FalseToken ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | HeadToken ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | IfToken ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | LeftParenthesisToken ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | LeftSqBracketToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | LetToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | NumberToken _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
                | OddToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | SimpleQuoteToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | SubToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | TailToken ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | TrueToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
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

and _menhir_run148 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | EvenToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | FalseToken ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | HeadToken ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | IfToken ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LeftParenthesisToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LeftSqBracketToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LetToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | NumberToken _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | OddToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SimpleQuoteToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SubToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | TailToken ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | TrueToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run152 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | EvenToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | FalseToken ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | HeadToken ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | IfToken ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LeftParenthesisToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LeftSqBracketToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LetToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NumberToken _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | OddToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SimpleQuoteToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SubToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | TailToken ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | TrueToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run156 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | EvenToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | FalseToken ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | HeadToken ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | IfToken ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | LeftParenthesisToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | LeftSqBracketToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | LetToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | NumberToken _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
        | OddToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | SimpleQuoteToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | SubToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | TailToken ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | TrueToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_assignableSeq : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (n : (
# 7 "parser.mly"
       (string)
# 2653 "parser.ml"
        ))), _, (ns : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 160 "parser.mly"
                                                                                                                           (AssignSeqNode (StringNode(n),Some(ns)))
# 2659 "parser.ml"
         in
        _menhir_goto_assignableSeq _menhir_env _menhir_stack _menhir_s _v
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (ns : (Ast.ast))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 156 "parser.mly"
                                                                                                                          (AssignNode (ns))
# 2677 "parser.ml"
             in
            _menhir_goto_assignable _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assignable : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AssignToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DoubleQuoteToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | EvenToken ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | FalseToken ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | HeadToken ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | IfToken ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LeftParenthesisToken ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LeftSqBracketToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | LetToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | NumberToken _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | OddToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | SimpleQuoteToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | SubToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | TailToken ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | TrueToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
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
        | AssignToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DoubleQuoteToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | EvenToken ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | FalseToken ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | HeadToken ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | IfToken ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LeftParenthesisToken ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LeftSqBracketToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LetToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NumberToken _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | OddToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | SimpleQuoteToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | SubToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | TailToken ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | TrueToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 | MenhirState113 | MenhirState129 | MenhirState134 | MenhirState189 | MenhirState161 | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AssignToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DoubleQuoteToken ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | EvenToken ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | FalseToken ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | HeadToken ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | IdentToken _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState174 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LeftParenthesisToken ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DoubleQuoteToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | EvenToken ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | FalseToken ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | HeadToken ->
                        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | IfToken ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | LeftParenthesisToken ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | LeftSqBracketToken ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | LetToken ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | NumberToken _v ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
                    | OddToken ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | SimpleQuoteToken ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | SubToken ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | TailToken ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | TrueToken ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState180
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | IfToken ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | LeftParenthesisToken ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | LeftSqBracketToken ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | LetToken ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | NumberToken _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | OddToken ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | ReceiveToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState174 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LeftParenthesisToken ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DoubleQuoteToken ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | EvenToken ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | FalseToken ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | HeadToken ->
                        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | IfToken ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | LeftParenthesisToken ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | LeftSqBracketToken ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | LetToken ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | NumberToken _v ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
                    | OddToken ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | SimpleQuoteToken ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | SubToken ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | TailToken ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | TrueToken ->
                        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | SimpleQuoteToken ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | SubToken ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | TailToken ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | TrueToken ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
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
                | DoubleQuoteToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | EvenToken ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | FalseToken ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | HeadToken ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | IfToken ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | LeftParenthesisToken ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | LeftSqBracketToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | LetToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | NumberToken _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
                | OddToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | SimpleQuoteToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | SubToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | TailToken ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | TrueToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171)
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

and _menhir_reduce30 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
    let _v : (Ast.ast) = 
# 111 "parser.mly"
                                                                                                                                  (ValueNode (v))
# 2994 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
    let _v : (Ast.ast) = 
# 151 "parser.mly"
                                                                                                                        (ValueSeqNode (v,None))
# 3004 "parser.ml"
     in
    _menhir_goto_valueSeq _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FalseToken ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LeftParenthesisToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LeftSqBracketToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NumberToken _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | SimpleQuoteToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TrueToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | EvenToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | FalseToken ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | HeadToken ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | IfToken ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LeftParenthesisToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LeftSqBracketToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LetToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | NumberToken _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | OddToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | SimpleQuoteToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | SubToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TailToken ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TrueToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run114 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | EvenToken ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | FalseToken ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | HeadToken ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | IfToken ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | LeftParenthesisToken ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | LeftSqBracketToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | LetToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | NumberToken _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                | OddToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | SimpleQuoteToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | SubToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | TailToken ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | TrueToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
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
        | DoubleQuoteToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | EvenToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | FalseToken ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | HeadToken ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | IfToken ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | LeftParenthesisToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | LeftSqBracketToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | LetToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | NumberToken _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | OddToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | SimpleQuoteToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | SubToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | TailToken ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | TrueToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState121
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

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IdentToken _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | LeftParenthesisToken ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | EvenToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | FalseToken ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | HeadToken ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | IfToken ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | LeftParenthesisToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | LeftSqBracketToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | LetToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | NumberToken _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
        | OddToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | SimpleQuoteToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | SubToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | TailToken ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | TrueToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 3273 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DoubleQuoteToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | EvenToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | FalseToken ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | HeadToken ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | IfToken ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LeftParenthesisToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LeftSqBracketToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LetToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | NumberToken _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
        | OddToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | SimpleQuoteToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | SubToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | TailToken ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | TrueToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | AssignToken | NewToken ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        | NewToken ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | ReceiveToken ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SendToken ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SpawnToken ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | TauToken ->
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

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 3361 "parser.ml"
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | RightParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (n : (
# 7 "parser.mly"
       (string)
# 3384 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 159 "parser.mly"
                                                                                                                           (AssignSeqNode (StringNode(n),None))
# 3389 "parser.ml"
         in
        _menhir_goto_assignableSeq _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce1 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "parser.mly"
       (string)
# 3402 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (n : (
# 7 "parser.mly"
       (string)
# 3408 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.ast) = 
# 155 "parser.mly"
                                                                                                                          (AssignNode (StringNode(n)))
# 3413 "parser.ml"
     in
    _menhir_goto_assignable _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState54 | MenhirState63 | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ComaToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken | RightSqBracketToken ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState207 | MenhirState202 | MenhirState174 | MenhirState180 | MenhirState176 | MenhirState171 | MenhirState157 | MenhirState153 | MenhirState149 | MenhirState145 | MenhirState136 | MenhirState131 | MenhirState126 | MenhirState121 | MenhirState117 | MenhirState35 | MenhirState108 | MenhirState53 | MenhirState104 | MenhirState100 | MenhirState95 | MenhirState91 | MenhirState87 | MenhirState74 | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ComaToken ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | RightParenthesisToken ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)
        | AddToken | AndToken | AssignToken | DifferentToken | DivToken | EqualToken | GreaterToken | LesserToken | MulToken | OrToken | SubToken ->
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run206 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 3462 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LeftParenthesisToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DoubleQuoteToken ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | EvenToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | FalseToken ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | HeadToken ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | IfToken ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | LeftParenthesisToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | LeftSqBracketToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | LetToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | NumberToken _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
        | OddToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | SimpleQuoteToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | SubToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | TailToken ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | TrueToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState207
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_variableDeclas : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ChooseToken ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | IdentToken _v ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
            | IfToken ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | LeftParenthesisToken ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | LetToken ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | SendToken ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | SpawnToken ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | WhileToken ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v : (Ast.ast))), _, (vs : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 67 "parser.mly"
                                                                                                             (VariableDeclasNode (v,Some(vs)))
# 3560 "parser.ml"
         in
        _menhir_goto_variableDeclas _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IdentToken _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (string)
# 3582 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FalseToken ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LeftParenthesisToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LeftSqBracketToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NumberToken _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | SimpleQuoteToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TrueToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_goto_opUnary : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | EvenToken ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | FalseToken ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | HeadToken ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IfToken ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LeftParenthesisToken ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LeftSqBracketToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | LetToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NumberToken _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | OddToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SimpleQuoteToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | SubToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TailToken ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TrueToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_goto_cst : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (Ast.ast)) = _v in
    let _v : (Ast.ast) = 
# 146 "parser.mly"
                                                                                                                        (ValueNode (c))
# 3662 "parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_variableDecla : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState198 | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BoolToken ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | ChannelToken ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | CharToken ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | IntegerToken ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | LeftParenthesisToken ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | ListToken ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | StringToken ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | SequenceToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (v : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 68 "parser.mly"
                                                                                                             (VariableDeclasNode (v,None))
# 3700 "parser.ml"
             in
            _menhir_goto_variableDeclas _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198)
    | MenhirState211 | MenhirState205 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BoolToken ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | ChannelToken ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | CharToken ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | FunctionToken ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | IdentToken _v ->
            _menhir_run206 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
        | IntegerToken ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | LeftParenthesisToken ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | ListToken ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | StringToken ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 141 "parser.mly"
                                                                                                                          (TrueNode)
# 3745 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 120 "parser.mly"
                                                                                                                                  (OpUnaryNode (Tail))
# 3757 "parser.ml"
     in
    _menhir_goto_opUnary _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 118 "parser.mly"
                                                                                                                                  (OpUnaryNode (Negate))
# 3769 "parser.ml"
     in
    _menhir_goto_opUnary _menhir_env _menhir_stack _menhir_s _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
# 3792 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 139 "parser.mly"
                                                                                                                          (CharNode (c))
# 3799 "parser.ml"
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

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 121 "parser.mly"
                                                                                                                                  (OpUnaryNode (Odd))
# 3823 "parser.ml"
     in
    _menhir_goto_opUnary _menhir_env _menhir_stack _menhir_s _v

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (int)
# 3830 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (
# 6 "parser.mly"
       (int)
# 3838 "parser.ml"
    )) = _v in
    let _v : (Ast.ast) = 
# 138 "parser.mly"
                                                                                                                              (IntegerNode(c))
# 3843 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IdentToken _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LeftParenthesisToken ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FalseToken ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LeftParenthesisToken ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LeftSqBracketToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NumberToken _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | SimpleQuoteToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TrueToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DoubleQuoteToken ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | EvenToken ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FalseToken ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | HeadToken ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IfToken ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LeftParenthesisToken ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LeftSqBracketToken ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LetToken ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NumberToken _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | OddToken ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | SimpleQuoteToken ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | SubToken ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TailToken ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TrueToken ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | EvenToken ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FalseToken ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | HeadToken ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | IfToken ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LeftParenthesisToken ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LeftSqBracketToken ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LetToken ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | NumberToken _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | OddToken ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SimpleQuoteToken ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SubToken ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TailToken ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | TrueToken ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 119 "parser.mly"
                                                                                                                                  (OpUnaryNode (Head))
# 3984 "parser.ml"
     in
    _menhir_goto_opUnary _menhir_env _menhir_stack _menhir_s _v

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 142 "parser.mly"
                                                                                                                          (FalseNode)
# 3996 "parser.ml"
     in
    _menhir_goto_cst _menhir_env _menhir_stack _menhir_s _v

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 122 "parser.mly"
                                                                                                                                 (OpUnaryNode (Even))
# 4008 "parser.ml"
     in
    _menhir_goto_opUnary _menhir_env _menhir_stack _menhir_s _v

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
# 4031 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 140 "parser.mly"
                                                                                                                          (StringNode (c))
# 4038 "parser.ml"
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

and _menhir_goto_parameters : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ast) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (t : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 4064 "parser.ml"
        ))), _, (params : (Ast.ast))) = _menhir_stack in
        let _3 = () in
        let _v : (Ast.ast) = 
# 57 "parser.mly"
                                                                                                            (ParamsNode (t,n,Some(params)))
# 4070 "parser.ml"
         in
        _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
    | MenhirState24 ->
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
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | ChannelToken ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | CharToken ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | IntegerToken ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | LeftParenthesisToken ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | ListToken ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | StringToken ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
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
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (t1 : (Ast.ast))), _, (t2 : (Ast.ast))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.ast) = 
# 169 "parser.mly"
                                                                                                                         (TupleTNode (t1,Some(t2)))
# 4141 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t1 : (Ast.ast))), _, (t2 : (Ast.ast))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ast) = 
# 173 "parser.mly"
                                                                                                                      (TupleTNode (t1, Some(t2)))
# 4158 "parser.ml"
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
# 167 "parser.mly"
                                                                                                                         (ChannTNode (t))
# 4225 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
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
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | ChannelToken ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | CharToken ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | IntegerToken ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | LeftParenthesisToken ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | ListToken ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | StringToken ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 | MenhirState11 ->
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
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | ChannelToken ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | CharToken ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | IntegerToken ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LeftParenthesisToken ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | ListToken ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | StringToken ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
        | RightParenthesisToken ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Ast.ast))) = _menhir_stack in
            let _v : (Ast.ast) = 
# 172 "parser.mly"
                                                                                                                      (TupleTNode (t,None))
# 4296 "parser.ml"
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
# 168 "parser.mly"
                                                                                                                                 (ListTNode (t))
# 4321 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (t : (Ast.ast))) = _menhir_stack in
        let _v : (Ast.ast) = 
# 177 "parser.mly"
                                                                                                                      (FuncTNode (Some (t)))
# 4337 "parser.ml"
         in
        _menhir_goto_funcType _menhir_env _menhir_stack _menhir_s _v
    | MenhirState27 | MenhirState24 ->
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
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | ChannelToken ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | CharToken ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | IntegerToken ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | LeftParenthesisToken ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | ListToken ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | StringToken ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
            | RightParenthesisToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (t : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 4379 "parser.ml"
                ))) = _menhir_stack in
                let _v : (Ast.ast) = 
# 56 "parser.mly"
                                                                                                            (ParamsNode (t,n,None))
# 4384 "parser.ml"
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
    | MenhirState0 | MenhirState211 | MenhirState205 | MenhirState31 | MenhirState198 ->
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
            | EqualToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DoubleQuoteToken ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | EvenToken ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | FalseToken ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | HeadToken ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | IfToken ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | LeftParenthesisToken ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | LeftSqBracketToken ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | LetToken ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | NumberToken _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _v
                | OddToken ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | SimpleQuoteToken ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | SubToken ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | TailToken ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | TrueToken ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202)
            | BoolToken | ChannelToken | CharToken | FunctionToken | IdentToken _ | IntegerToken | LeftParenthesisToken | ListToken | SequenceToken | StringToken ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (t : (Ast.ast))), (n : (
# 7 "parser.mly"
       (string)
# 4452 "parser.ml"
                ))) = _menhir_stack in
                let _v : (Ast.ast) = 
# 63 "parser.mly"
                                                                                                            (VariableDeclaNode (t,n, None))
# 4457 "parser.ml"
                 in
                _menhir_goto_variableDecla _menhir_env _menhir_stack _menhir_s _v
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
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState211 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
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
# 165 "parser.mly"
                                                                                                                          (TypeNode (StringT))
# 4705 "parser.ml"
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
# 163 "parser.mly"
                                                                                                                           (TypeNode (IntegerT))
# 4778 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BoolToken ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | ChannelToken ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | CharToken ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | IntegerToken ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LeftParenthesisToken ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | ListToken ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | StringToken ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | VoidToken ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState19 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (Ast.ast) = 
# 176 "parser.mly"
                                                                                                                      (FuncTNode (None))
# 4811 "parser.ml"
         in
        _menhir_goto_funcType _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.ast) = 
# 166 "parser.mly"
                                                                                                                          (TypeNode (CharT))
# 4827 "parser.ml"
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
# 164 "parser.mly"
                                                                                                                          (TypeNode (BooleanT))
# 4864 "parser.ml"
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
# 40 "parser.mly"
       (Ast.ast)
# 4883 "parser.ml"
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
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
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
  

# 4921 "parser.ml"
