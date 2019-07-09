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
  print_string ("File parsed \n");
  (* print_string (string_of_ast ast); *)
  let (envType,envVar,start) = create_env ([],[],None) ast in
  let (envType,envVar) = (List.rev envType, List.rev envVar) in
  print_string ("Environment created \n");
  let nameListType = (well_formed_envType envType) in
  let _ = (well_formed_envVar envVar nameListType) in
  let a = well_formed_start start envVar in
  print_string ("Environment well-formed")
  with
  | Lexer.Error s ->
      report_error file filebuf "lexical error (unexpected character).";
  | Parser.Error ->
      report_error file filebuf "syntax error.";
  | Resolve.Multiple_declaration_type (pos,name) ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: The type %s is declared twice\n" file pos.pos_lnum c name;
 | Resolve.Undeclared_type (pos,name) ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: Incorrect declaration type: unbound value %s\n" file pos.pos_lnum c name;
 | Resolve.Unauthorized_recursivity pos ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: Incorrect declaration type: only channel type can be recursive\n" file pos.pos_lnum c;
 | Resolve.Multiple_declaration_var (pos,name) ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: The name %s is already used by another function/variable\n" file pos.pos_lnum c name;
 | Resolve.Multiple_declaration_param (pos,name) ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: The name %s is used twice in the function paraneters\n" file pos.pos_lnum c name;
 | Resolve.Type_not_found (pos,name) ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: Incorrect type: unbound value %s\n" file pos.pos_lnum c name;
 | Resolve.Function_not_found (pos,name) ->
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, character %d: Incorrect function for start: unbound value %s\n" file pos.pos_lnum c name;
 | Resolve.Unknow_error_in_type_checking ->
    Printf.eprintf "Fatal error in type checking";
      
