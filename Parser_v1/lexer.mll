{
  open Parser
  open Lexing

  exception Error of string
}


let digit = ['0'-'9']
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']
let alphabet = lowercase | uppercase
let char = alphabet | digit | '_'
let id = char char*

rule token = parse
  | '\n'  (* ignore newlines but count them *)
      { new_line lexbuf; token lexbuf }
  | [' ' '\t'] (* ignore whitespaces and tabs *)
      { token lexbuf }
  | '('       {LeftParenthesisToken}
  | ')'       {RightParenthesisToken}
  | '{'       {LeftBracketToken}
  | '}'       {RightBracketToken}
  | '['       {LeftSqBracketToken}
  | ']'       {RightSqBracketToken}
  | "=="      {EqualToken}
  | "="       {AssignToken}
  | "!="      {DifferentToken}
  | ';'       {SequenceToken}
  | ','       {ComaToken}
  | '<'       {LesserToken}
  | '>'       {GreaterToken}
  | '+'       {AddToken}
  | '-'       {SubToken}
  | '*'       {MulToken}
  | '/'       {DivToken}
  | '''       {SimpleQuoteToken}
  | '"'       {DoubleQuoteToken}
  | "&&"      {AndToken}
  | "||"      {OrToken}
  | "->"      {ArrowToken}
  | "func"    {FunctionToken}
  | "if"      {IfToken}
  | "else"    {ElseToken}
  | "in"      {InToken}
  | "true"    {TrueToken}
  | "false"   {FalseToken}
  | "boolean" {BoolToken}
  | "string"  {StringToken}
  | "int"     {IntegerToken}
  | "char"    {CharToken}
  | "channel" {ChannelToken}
  | "list"    {ListToken}
  | "void"    {VoidToken}
  | "let"     {LetToken}
  | "while"   {WhileToken}
  | "receive" {ReceiveToken}
  | "send"    {SendToken}
  | "new"     {NewToken}
  | "spawn"   {SpawnToken}
  | "choose"  {ChooseToken}
  | '|'       {ChoiceToken}
  | "head"    {HeadToken}
  | "tail"    {TailToken}
  | "odd"     {OddToken}
  | "even"    {EvenToken}
  | "tau"     {TauToken}
  | "start"   {StartToken}
  | "call"    {CallToken}
  | "noop"    {NoopToken}
  | digit+ as inum
              {NumberToken (int_of_string inum)}
  | id as text
              {IdentToken text}
  | char as text
              {CharValueToken text}
| eof         { EOF }
| _
{ raise (Error ("Unexpected char: "^(Lexing.lexeme lexbuf)^" at "^(string_of_int (Lexing.lexeme_start
lexbuf))^"-"^(string_of_int (Lexing.lexeme_end lexbuf)))) }
