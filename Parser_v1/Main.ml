open Ast 
open Lexing
open Resolve

let report_error filename lexbuf msg =
 let (b,e) = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
 let fc = b.pos_cnum - b.pos_bol + 1 in
 let lc = e.pos_cnum - b.pos_bol + 1 in
 Printf.eprintf "File \"%s\", line %d, characters %d-%d: %s\n" filename b.pos_lnum fc lc msg

 let main file =
  let input = open_in file in
  let filebuf = Lexing.from_channel input in
  try
  let ast = Parser.main Lexer.token filebuf  in
 (*  print_string (string_of_ast ast); *)
  let (envType,envVar) = create_env ([],[]) ast in
  well_formed_envType envType 
  with
  | Lexer.Error s ->
      report_error file filebuf "lexical error (unexpected character).";
      exit 2
  | Parser.Error ->
      report_error file filebuf "syntax error.";
      exit 2
  | Resolve.Multiple_declaration_type (pos,name) ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: The type %s is declared twice\n" file pos.pos_lnum c name;
    exit 2;
 | Resolve.Unknow_error_in_type_checking ->
    Printf.eprintf "Fatal error in type checking";    
    exit 2
 | Resolve.Undeclared_type (pos,name) ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: Incorrect declaration type: unbound value %s\n" file pos.pos_lnum c name;
    exit 2
 | Resolve.Unauthorized_recursivity pos ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: Incorrect declaration type: only channel type can be recursive\n" file pos.pos_lnum c;
    exit 2
      
