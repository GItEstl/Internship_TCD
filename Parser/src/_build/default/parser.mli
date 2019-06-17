
(* The type of tokens. *)

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
  | NumberToken of (int)
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
  | IdentToken of (string)
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
  | CharValueToken of (char)
  | CharToken
  | ChannelToken
  | BoolToken
  | AssignToken
  | ArrowToken
  | AndToken
  | AddToken

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.ast)
