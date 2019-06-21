open Ast 
open Lexer
open Parser
open Lexing

let report_error filename lexbuf msg =
 let (b,e) = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
 let fc = b.pos_cnum - b.pos_bol + 1 in
 let lc = e.pos_cnum - b.pos_bol + 1 in
 Printf.eprintf "File \"%s\", line %d, characters %d-%d: %s\n" filename b.pos_lnum fc lc msg

 let main fichier =
  let input = open_in fichier in
  let filebuf = Lexing.from_channel input in
  try
  let ast = Parser.main Lexer.token filebuf  in
   print_string (string_of_ast ast)
  with
  | Lexer.Error s ->
      report_error fichier filebuf "lexical error (unexpected character).";
      exit 2
  | Parser.Error ->
      report_error fichier filebuf "syntax error.";
      exit 2