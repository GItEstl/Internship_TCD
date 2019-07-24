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
let comments =
  (* Comments end of line *)
  "//" [^'\n']*

rule token = parse
  | '\n'  (* ignore newlines but count them *)
      { new_line lexbuf; token lexbuf }
  | [' ' '\t'] (* ignore whitespaces and tabs *)
      { token lexbuf }
  | comments	{ (token lexbuf) }
  | '('       {LeftParenthesisToken (Lexing.lexeme_start_p lexbuf)}
  | ')'       {RightParenthesisToken (Lexing.lexeme_start_p lexbuf)}
  | '{'       {LeftBracketToken (Lexing.lexeme_start_p lexbuf)}
  | '}'       {RightBracketToken (Lexing.lexeme_start_p lexbuf)}
  | '['       {LeftSqBracketToken (Lexing.lexeme_start_p lexbuf)}
  | ']'       {RightSqBracketToken (Lexing.lexeme_start_p lexbuf)}
  | "=="      {EqualToken (Lexing.lexeme_start_p lexbuf)}
  | "="       {AssignToken (Lexing.lexeme_start_p lexbuf)}
  | "!="      {DifferentToken (Lexing.lexeme_start_p lexbuf)}
  | ';'       {SequenceToken (Lexing.lexeme_start_p lexbuf)}
  | ','       {ComaToken (Lexing.lexeme_start_p lexbuf)}
  | '<'       {LesserToken (Lexing.lexeme_start_p lexbuf)}
  | '>'       {GreaterToken (Lexing.lexeme_start_p lexbuf)}
  | '+'       {AddToken (Lexing.lexeme_start_p lexbuf)}
  | '-'       {SubToken (Lexing.lexeme_start_p lexbuf)}
  | '*'       {MulToken (Lexing.lexeme_start_p lexbuf)}
  | '/'       {DivToken (Lexing.lexeme_start_p lexbuf)}
  | '"'       {DoubleQuoteToken (Lexing.lexeme_start_p lexbuf)}
  | "&&"      {AndToken (Lexing.lexeme_start_p lexbuf)}
  | "||"      {OrToken (Lexing.lexeme_start_p lexbuf)}
  | "->"      {ArrowToken (Lexing.lexeme_start_p lexbuf)}
  | "func"    {FunctionToken (Lexing.lexeme_start_p lexbuf)}
  | "if"      {IfToken (Lexing.lexeme_start_p lexbuf)}
  | "else"    {ElseToken (Lexing.lexeme_start_p lexbuf)}
  | "in"      {InToken (Lexing.lexeme_start_p lexbuf)}
  | "true"    {TrueToken (Lexing.lexeme_start_p lexbuf)}
  | "false"   {FalseToken (Lexing.lexeme_start_p lexbuf)}
  | "boolean" {BoolToken (Lexing.lexeme_start_p lexbuf)}
  | "string"  {StringToken (Lexing.lexeme_start_p lexbuf)}
  | "int"     {IntegerToken (Lexing.lexeme_start_p lexbuf)}
  | "char"    {CharToken (Lexing.lexeme_start_p lexbuf)}
  | "channel" {ChannelToken (Lexing.lexeme_start_p lexbuf)}
  | "type"    {TypeToken (Lexing.lexeme_start_p lexbuf)}
  | "list"    {ListToken (Lexing.lexeme_start_p lexbuf)}
  | "void"    {VoidToken (Lexing.lexeme_start_p lexbuf)}
  | "while"   {WhileToken (Lexing.lexeme_start_p lexbuf)}
  | "receive" {ReceiveToken (Lexing.lexeme_start_p lexbuf)}
  | "send"    {SendToken (Lexing.lexeme_start_p lexbuf)}
  | "newChan" {NewToken (Lexing.lexeme_start_p lexbuf)}
  | "spawn"   {SpawnToken (Lexing.lexeme_start_p lexbuf)}
  | "choose"  {ChooseToken (Lexing.lexeme_start_p lexbuf)}
  | '|'       {ChoiceToken (Lexing.lexeme_start_p lexbuf)}
  | "not"     {NotToken (Lexing.lexeme_start_p lexbuf)}
  | "head"    {HeadToken (Lexing.lexeme_start_p lexbuf)} 
  | "tail"    {TailToken (Lexing.lexeme_start_p lexbuf)}
  | "odd"     {OddToken (Lexing.lexeme_start_p lexbuf)}
  | "even"    {EvenToken (Lexing.lexeme_start_p lexbuf)}
  | "fst"     {FstToken (Lexing.lexeme_start_p lexbuf)} 
  | "snd"     {SndToken (Lexing.lexeme_start_p lexbuf)}
  | "get"     {GetToken (Lexing.lexeme_start_p lexbuf)}
  | "tau"     {TauToken (Lexing.lexeme_start_p lexbuf)}
  | "start"   {StartToken (Lexing.lexeme_start_p lexbuf)}
  | "return"  {ReturnToken (Lexing.lexeme_start_p lexbuf)}
  | "def"     {DefToken (Lexing.lexeme_start_p lexbuf)}
  | digit+ as inum
              {NumberToken (((Lexing.lexeme_start_p lexbuf)),(int_of_string inum))}
  | char as text
              {CharValueToken ((Lexing.lexeme_start_p lexbuf),(String.sub text 1 1))}
  | id as text
              {IdentToken ((Lexing.lexeme_start_p lexbuf),text)}
| eof         { EOF }
| _
{ raise (Error ("Unexpected char: "^(Lexing.lexeme lexbuf)^" at "^(string_of_int (Lexing.lexeme_start
lexbuf))^"-"^(string_of_int (Lexing.lexeme_end lexbuf)))) }
