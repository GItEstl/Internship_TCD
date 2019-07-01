{
  open Parser
  open Lexing

  exception Error of string
}


let digit = ['0'-'9']
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']
let alphabet = lowercase | uppercase
let alphanum = alphabet | digit | '_'
let id = alphanum alphanum*
let char = '\'' alphanum '\''

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
  | "type"    {TypeToken}
  | "list"    {ListToken}
  | "void"    {VoidToken}
  | "while"   {WhileToken}
  | "receive" {ReceiveToken}
  | "send"    {SendToken}
  | "newChan" {NewToken}
  | "spawn"   {SpawnToken}
  | "choose"  {ChooseToken}
  | '|'       {ChoiceToken}
  | "head"    {HeadToken}
  | "tail"    {TailToken}
  | "odd"     {OddToken}
  | "even"    {EvenToken}
  | "tau"     {TauToken}
  | "start"   {StartToken}
  | "return"  {ReturnToken}
  | "def"     {DefToken}
  | digit+ as inum
              {NumberToken (int_of_string inum)}
  | char as text
              {CharValueToken (String.sub text 1 1)}
  | id as text
              {IdentToken text}
| eof         { EOF }
| _
{ raise (Error ("Unexpected char: "^(Lexing.lexeme lexbuf)^" at "^(string_of_int (Lexing.lexeme_start
lexbuf))^"-"^(string_of_int (Lexing.lexeme_end lexbuf)))) }
