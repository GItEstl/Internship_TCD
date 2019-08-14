open Lexing
open Resolve
open TypeChecking
open MainInterpretor

 (* report_error: string -> lexbuf -> string -> unit
Fucntion printing an error
Parameters:
  - filename: path of the file 
  - lexbuf: lexer buffer
  - msg: message to print
*)
let report_error filename lexbuf msg =
  let (b,e) = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d: %s\n" filename b.pos_lnum fc lc msg

 (* main: int -> int -> ?seed:int -> string -> unit
Function compiling a file
Parameter:
  - verbosity: amount of verbosity
  - maxstep: maximum number of steps for the execution
  - seed: integer use to initialize the number generator
  - file: path of the file to compile
*)
let main verbosity maxstep ?seed:(seed = -1) file =
  (* Opening the file *)
  let input = open_in file in
  (* Lexing *)
  let filebuf = Lexing.from_channel input in
  try
  (* Parsing *)
  let ast = Parser.main Lexer.token filebuf  in
  print_string ("File parsed \n");
  (* Creation of the variable and type environments *)
  let (envType,envVar,start) = create_env ast in
  print_string ("Environment created \n");
  (* Checking of the well-formedness *)
  let nameListType = well_formedness_check envType envVar start in
  print_string ("Environment well-formed \n");
  (* Type Checking *)
  type_check_prg envVar envType nameListType ast;
  print_string ("Program type-checked \n");
  (* Initialization of the execution *)
  init_prg ast envType start verbosity seed maxstep false;
  (* Execution *)
  let result = run_prg () in
  print_string ("Program executed \n\n");
  (* Printing of the results *)
  print_string("Result: " ^ result ^ "\n")
  (* Catching of the exceptions *)
  with
   | Lexer.Error _ ->
     report_error file filebuf "lexical error (unexpected character)";
   | Parser.Error ->
     report_error file filebuf "syntax error";
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
     Printf.eprintf "File \"%s\", line %d, character %d: The name %s is used twice in the function parameters\n" file pos.pos_lnum c name;
   | Resolve.Type_not_found (pos,name) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     Printf.eprintf "File \"%s\", line %d, character %d: Incorrect type: unbound value %s\n" file pos.pos_lnum c name;
   | Resolve.Function_not_found (pos,name) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     Printf.eprintf "File \"%s\", line %d, character %d: Incorrect function for start: unbound value %s\n" file pos.pos_lnum c name;
   | Resolve.Unknow_error_in_resolve (m) ->
     Printf.eprintf "File \"%s\": Unknown error during well-formness checking: please check the function %s in the file Resolve.ml\n" file m;
   | TypeChecking.Wrong_type (pos,tfound,texpec) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     let f = string_of_type tfound in
     let e = string_of_type texpec in
     Printf.eprintf "File \"%s\", line %d, character %d: Wrong type: The type %s was found but the type %s was expected\n" file pos.pos_lnum c f e;
   | TypeChecking.Inconsistent_types(pos,t1found,t2found) ->
     let t_then = string_of_type t1found in
     let t_else = string_of_type t2found in
      Printf.eprintf "File \"%s\", line %d: Inconsistent types: The return type %s and %s were found in the then and else branch\n" file pos.pos_lnum t_then t_else;
   | TypeChecking.Unknown_variable(pos,name) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     Printf.eprintf "File \"%s\", line %d, character %d: Unknown variable: unbound value %s\n" file pos.pos_lnum c name;
   | TypeChecking.Unknown_function(pos,namef) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     Printf.eprintf "File \"%s\", line %d, character %d: Unknown function: unbound value %s\n" file pos.pos_lnum c namef;
   | TypeChecking.Not_type_of_the_params(pos,te,tp,namef) ->
     let t_expr = string_of_type te in
     let t_decla = string_of_type tp in
     Printf.eprintf "File \"%s\", line %d: Wrong type of argument in the function %s: the type %s was found but the type %s was expected\n" file pos.pos_lnum namef t_expr t_decla;
   | TypeChecking.Not_type_of_the_return(pos,ta,tf,namef) ->
     let t_assign = string_of_type ta in
     let t_func = string_of_type tf in
     Printf.eprintf "File \"%s\", line %d: Incorrect assignment of the function return: the function %s returns the type %s but the type %s was found\n" file pos.pos_lnum namef t_func t_assign;
   | TypeChecking.Assignement_of_void(pos,namef) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     Printf.eprintf "File \"%s\", line %d, character %d: Incorrect assignment: the function %s does not have a return\n" file pos.pos_lnum c namef;
   | TypeChecking.No_assignement_of_the_return(pos,tf,namef) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     let t_func = string_of_type tf in
      Printf.eprintf "File \"%s\", line %d, character %d: The function %s has the type %s, it must be assigned to a variable\n" file pos.pos_lnum c namef t_func;
   | TypeChecking.Wrong_type_chan_receive (pos,ta,st) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     let t_assign= string_of_type ta in
     let t_chan = string_of_type st in
      Printf.eprintf "File \"%s\", line %d, character %d: Incorrect assignment of the channel reception: the elements passed along the channel have the type %s but the type %s was found\n" file pos.pos_lnum c t_chan t_assign;
   | TypeChecking.Wrong_type_chan_send (pos,te,st) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     let t_elt = string_of_type te in
     let t_chan = string_of_type st in
      Printf.eprintf "File \"%s\", line %d, character %d: Wrong type of element send: the elements passed along the channel have the type %s but the type %s was found\n" file pos.pos_lnum c t_chan t_elt;
   | TypeChecking.Different_type_of_return_if (pos,t1,t2) ->
     let t_then = string_of_type t1 in
     let t_else = string_of_type t2 in
     Printf.eprintf "File \"%s\", line %d: Inconsistent types: The return type %s and %s were found in the then and else branch\n" file pos.pos_lnum t_then t_else;
   | TypeChecking.Return_not_match_with_decla (pos,rt,ft,namef) ->
     let t_return = string_of_type rt in
     let t_func = string_of_type ft in
     Printf.eprintf "File \"%s\", line %d: Wrong type of function return: The function %s returns the type %s but the type %s was found\n" file pos.pos_lnum namef t_func t_return;
   | TypeChecking.Different_type_of_return_func (pos,namef) ->
     Printf.eprintf "File \"%s\", line %d: Multiple return types found in the function %s\n" file pos.pos_lnum namef;
   | TypeChecking.Illegal_type_argument(pos) ->
     Printf.eprintf "File \"%s\", line %d: Illegal type argument\n" file pos.pos_lnum;
   | TypeChecking.Different_types_in_list ->
     Printf.eprintf "File \"%s\": A list containing different types has been found, a list can contain only elements of one type\n" file;
   | TypeChecking.Unknown_error_type_checking(m) ->
     Printf.eprintf "File \"%s\": Unknown error during type checking: please check the function %s in the file TypeChecking.ml\n" file m;
   | TypeChecking.Get_tuple_too_short(pos) -> 
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     Printf.eprintf "File \"%s\", line %d, character %d: Incorrect use of get: index out of bound \n" file pos.pos_lnum c;
   | TypeChecking.Assignment_to_global_var(pos,n) ->
     let c = pos.pos_cnum - pos.pos_bol + 1 in
     Printf.eprintf "File \"%s\", line %d, character %d: You cannot assign a value to the global variable %s \n" file pos.pos_lnum c n;
   | TypeChecking.Multiple_assignments(pos) ->
     Printf.eprintf "File \"%s\", line %d: Incorrect assignment: you can assign an expression to only one variable" file pos.pos_lnum
   | ExpressionInterpretor.Run_time_error(m) -> 
     Printf.eprintf "File \"%s\": Runtime error: %s\n" file m;
   | ExpressionInterpretor.Unknown_error_expression_interpretor(m) -> 
     Printf.eprintf "File \"%s\": Unknown error during execution: please check the function %s in the file ExpressionInterpretor.ml\n" file m;
   | BetaRedInterpretor.Unknown_error_betared_interpretor(m) ->
     Printf.eprintf "File \"%s\": Unknown error during execution: please check the function %s in the file BetaRedInterpretor.ml\n" file m;
   | MainInterpretor.Unknown_error_main_interpretor(m) ->
     Printf.eprintf "File \"%s\": Unknown error during execution: please check the function %s in the file MainInterpretor.ml\n" file m;
   | Division_by_zero -> 
     Printf.eprintf "File \"%s\": Runtime error: Division by zero\n" file;
   