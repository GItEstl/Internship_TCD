open Resolve
open TypeChecking
open MainInterpretor

 (* main: string -> unit
Function compiling a file
Parameter:
  - verbosity: amount of verbosity
  - file: path of the file to compile
*)
 let main verbosity file =
  (* Opening the file *)
  let input = open_in file in
  (* Lexing *)
  let filebuf = Lexing.from_channel input in
  try
  (* Parsing *)
  let ast = Parser.main Lexer.token filebuf  in
  (* Creation of the variable and type environments *)
  let (envType,envVar,start) = create_env ast in
  (* Checking of the well-formedness *)
  let nameListType = well_formedness_check envType envVar start in
  (* Type Checking *)
  type_check_prg envVar envType nameListType ast;
  (* Initialization of the execution *)
  init_prg ast envType start verbosity 0 10000 true;
  (* Execution *)
  run_prg ()
  (* Catching of the exceptions *)
  with
  | Lexer.Error _ -> "lexical error (unexpected character)"
  | Parser.Error -> "syntax error"
  | Resolve.Multiple_declaration_type (_,name) -> "The type " ^ name ^ " is declared twice"
  | Resolve.Undeclared_type (_,name) -> "Incorrect declaration type: unbound value " ^ name
  | Resolve.Unauthorized_recursivity _ -> "Incorrect declaration type: only channel type can be recursive"
  | Resolve.Multiple_declaration_var (_,name) -> "The name " ^ name ^ " is already used by another function/variable"
  | Resolve.Multiple_declaration_param (_,name) -> "The name " ^ name ^ " is used twice in the function parameters"
  | Resolve.Type_not_found (_,name) -> "Incorrect type: unbound value " ^ name
  | Resolve.Function_not_found (_,name) -> "Incorrect function for start: unbound value " ^ name
  | Resolve.Unknow_error_in_resolve (m) -> "Unknown error during well-formness: please check the function " ^ m ^ " in the file Resolve.ml"
  | TypeChecking.Wrong_type (_,tfound,texpec) ->
    let f = string_of_type tfound in
    let e = string_of_type texpec in
    "Wrong type: The type " ^ f ^ " was found but the type " ^ e ^ " was expected"
  | TypeChecking.Inconsistent_types(_,t1found,t2found) ->
    let t_then = string_of_type t1found in
    let t_else = string_of_type t2found in
    "Inconsistent types: The return type " ^ t_then ^ " and " ^ t_else ^ " were found in the then and else branch"
  | TypeChecking.Unknown_variable(_,name) -> "Unknown variable: unbound value " ^ name;
  | TypeChecking.Unknown_function(_,namef) -> "Unknown function: unbound value " ^ namef;
  | TypeChecking.Not_type_of_the_params(_,te,tp,namef) ->
    let t_expr = string_of_type te in
    let t_decla = string_of_type tp in 
    "Wrong type of argument in the function " ^ namef ^ ": the type " ^ t_expr ^ " was found but the type " ^ t_decla ^ " was expected"
  | TypeChecking.Not_type_of_the_return(_,ta,tf,namef) ->
    let t_assign = string_of_type ta in
    let t_func = string_of_type tf in
    "Incorrect assignment of the function return: the function " ^ namef ^ " returns the type " ^ t_func ^ " but the type " ^ t_assign ^ " was found"
  | TypeChecking.Assignement_of_void(_,namef) -> "Incorrect assignment: the function " ^ namef ^ " does not have a return"
  | TypeChecking.No_assignement_of_the_return(_,tf,namef) ->
    let t_func = string_of_type tf in
    "The function " ^ namef ^ " has the type " ^ t_func ^ ", it must be assigned to a variable"
  | TypeChecking.Wrong_type_chan_receive (_,ta,st) ->
    let t_assign= string_of_type ta in
    let t_chan = string_of_type st in
    "Incorrect assignment of the channel reception: the elements passed along the channel have the type " ^ t_chan ^ " but the type " ^ t_assign ^ " was found"
  | TypeChecking.Wrong_type_chan_send (_,te,st) ->
    let t_elt = string_of_type te in
    let t_chan = string_of_type st in
    "Wrong type of element send: the elements passed along the channel have the type " ^ t_chan ^ " but the type " ^ t_elt ^ " was found"
  | TypeChecking.Different_type_of_return_if (_,t1,t2) ->
    let t_then = string_of_type t1 in
    let t_else = string_of_type t2 in
    "Inconsistent types: The return type " ^ t_then ^ " and " ^ t_else ^ " were found in the then and else branch"
  | TypeChecking.Return_not_match_with_decla (_,rt,ft,namef) ->
    let t_return = string_of_type rt in
    let t_func = string_of_type ft in
    "Wrong type of function return: The function " ^ namef ^ " returns the type " ^ t_func ^ " but the type " ^ t_return ^ " was found"
  | TypeChecking.Different_type_of_return_func (_,namef) -> "Multiple return types found in the function " ^ namef;
  | TypeChecking.Illegal_type_argument(_) -> "Illegal type argument"
  | TypeChecking.Different_types_in_list -> "A list containing different types has been found, a list can contain only elements of one type"
  | TypeChecking.Unknown_error_type_checking(m) -> "Unknown error during type checking: please check the function " ^ m ^ " in the file TypeChecking.ml"
  | TypeChecking.Get_tuple_too_short(_) -> "Incorrect use of get: index out of bound"
  | TypeChecking.Assignment_to_global_var(_,n) -> "You cannot assign a value to the global variable " ^ n
  | TypeChecking.Multiple_assignments(_) -> "Incorrect assignment: you can assign an expression to only one variable"
  | ExpressionInterpretor.Run_time_error(_) -> "Runtime error"
  | ExpressionInterpretor.Unknown_error_expression_interpretor(m) -> "Unknown error during execution: please check the function " ^ m ^ " in the file ExpressionInterpretor.ml"
  | BetaRedInterpretor.Unknown_error_betared_interpretor(m) -> "Unknown error during execution: please check the function " ^ m ^ " in the file BetaRedInterpretor.ml"
  | MainInterpretor.Unknown_error_main_interpretor(m) -> "Unknown error during execution: please check the function " ^ m ^ " in the file MainInterpretor.ml"

(* Tests *)
let%expect_test _ = print_string (main 0 "../../examples/tests/test-00.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-01.mml"); [%expect{| Unknown variable: unbound value i |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-02.mml"); [%expect{| Wrong type: The type boolean was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-03.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-04.mml"); [%expect{| Wrong type: The type boolean was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-05.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-06.mml"); [%expect{| Unknown variable: unbound value j |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-07.mml"); [%expect{| Wrong type: The type boolean was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-08.mml"); [%expect{| The name i is already used by another function/variable |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-09.mml"); [%expect{| The name i is already used by another function/variable |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-10.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-11.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-12.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-13.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-14.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-15.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-16.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-17.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-18.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-19.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-20.mml"); [%expect{| Wrong type: The type char was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-21.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-22.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-23.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-24.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-25.mml"); [%expect{| Inconsistent types: The return type boolean and integer were found in the then and else branch |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-26.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-27.mml"); [%expect{| syntax error |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-28.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-29.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-30.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-31.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-32.mml"); [%expect{| Wrong type: The type string was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-33.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-34.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-35.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-36.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-37.mml"); [%expect{| Wrong type: The type (char, boolean, integer, boolean) was found but the type (char, string, integer, boolean) was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-38.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-39.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-40.mml"); [%expect{| Incorrect use of get: index out of bound |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-41.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-42.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-43.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-44.mml"); [%expect{| Wrong type: The type (integer, integer) was found but the type (integer, boolean) was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-45.mml"); [%expect{| Incorrect assignment: you can assign an expression to only one variable |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-46.mml"); [%expect{| Wrong type: The type (integer, boolean) was found but the type (integer, integer) was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-47.mml"); [%expect{| Wrong type: The type (integer, boolean, boolean) was found but the type (integer, boolean) was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-48.mml"); [%expect{| Illegal type argument |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-49.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-50.mml"); [%expect{| Wrong type of function return: The function test50 returns the type void but the type integer was found |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-51.mml"); [%expect{| Multiple return types found in the function test51 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-52.mml"); [%expect{| Wrong type of function return: The function test52 returns the type integer but the type void was found |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-53.mml"); [%expect{| Wrong type of function return: The function test53 returns the type integer but the type boolean was found |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-54.mml"); [%expect{| [1, 2, 3, 4] |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-55.mml"); [%expect{| (1, true, 'a') |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-56.mml"); [%expect{| 12 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-57.mml"); [%expect{| -66 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-58.mml"); [%expect{| (-66, false, true) |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-59.mml"); [%expect{| Wrong type: The type list integer was found but the type list boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-60.mml"); [%expect{| A list containing different types has been found, a list can contain only elements of one type |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-61.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-62.mml"); [%expect{| [true, false, false, true, false] |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-63.mml"); [%expect{| Runtime error |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-64.mml"); [%expect{| [] |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-65.mml"); [%expect{| Wrong type: The type (string, string) was found but the type list was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-66.mml"); [%expect{| Wrong type: The type (integer, string, boolean) was found but the type list was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-67.mml"); [%expect{| -1 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-68.mml"); [%expect{| Wrong type: The type string was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-69.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-70.mml"); [%expect{| Wrong type: The type char was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-71.mml"); [%expect{| Wrong type: The type boolean was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-72.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-73.mml"); [%expect{| Wrong type: The type boolean was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-74.mml"); [%expect{| Wrong type: The type boolean was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-75.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-76.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-77.mml"); [%expect{| Wrong type: The type boolean was found but the type string was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-78.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-79.mml"); [%expect{| Wrong type: The type boolean was found but the type string was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-80.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-81.mml"); [%expect{| Wrong type: The type string was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-82.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-83.mml"); [%expect{| Wrong type: The type string was found but the type char was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-84.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-85.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-86.mml"); [%expect{| Wrong type: The type boolean was found but the type string was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-87.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-88.mml"); [%expect{| Wrong type: The type string was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-89.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-90.mml"); [%expect{| Wrong type: The type string was found but the type char was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-91.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-92.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-93.mml"); [%expect{| Wrong type: The type boolean was found but the type string was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-94.mml"); [%expect{| Wrong type: The type char was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-95.mml"); [%expect{| Wrong type: The type char was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-96.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-97.mml"); [%expect{| Wrong type: The type boolean was found but the type string was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-98.mml"); [%expect{| Wrong type: The type boolean was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-99.mml"); [%expect{| Wrong type: The type string was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-100.mml"); [%expect{| 16 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-101.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-102.mml"); [%expect{| Wrong type: The type boolean was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-103.mml"); [%expect{| Wrong type: The type string was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-104.mml"); [%expect{| -12 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-105.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-106.mml"); [%expect{| Wrong type: The type char was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-107.mml"); [%expect{| Wrong type: The type string was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-108.mml"); [%expect{| 28 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-109.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-110.mml"); [%expect{| Wrong type: The type char was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-111.mml"); [%expect{| Wrong type: The type string was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-112.mml"); [%expect{| 5 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-113.mml"); [%expect{| Wrong type: The type integer was found but the type string was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-114.mml"); [%expect{| Wrong type: The type char was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-115.mml"); [%expect{| Wrong type: The type string was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-116.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-117.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-118.mml"); [%expect{| Wrong type: The type boolean was found but the type char was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-119.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-120.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-121.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-122.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-123.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-124.mml"); [%expect{| Wrong type: The type boolean was found but the type char was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-125.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-126.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-127.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-128.mml"); [%expect{| Inconsistent types: The return type integer and boolean were found in the then and else branch |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-129.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-130.mml"); [%expect{| Multiple return types found in the function test130 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-131.mml"); [%expect{| 2 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-132.mml"); [%expect{| Multiple return types found in the function test132 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-133.mml"); [%expect{| Multiple return types found in the function test133 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-134.mml"); [%expect{| The name sameName is already used by another function/variable |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-135.mml"); [%expect{| Incorrect type: unbound value foo |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-136.mml"); [%expect{| You cannot assign a value to the global variable globalVar |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-137.mml"); [%expect{| (1, true, [1, 2, 3]) |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-138.mml"); [%expect{| Incorrect type: unbound value foo |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-139.mml"); [%expect{| Incorrect type: unbound value foo |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-140.mml"); [%expect{| The name x is used twice in the function parameters |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-141.mml"); [%expect{| The name x is used twice in the function parameters |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-142.mml"); [%expect{| The name test142 is already used by another function/variable |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-143.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-144.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-145.mml"); [%expect{| 0 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-146.mml"); [%expect{| "test" |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-147.mml"); [%expect{| Wrong type: The type integer was found but the type string was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-148.mml"); [%expect{| 13 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-149.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-150.mml"); [%expect{| Wrong type: The type integer was found but the type boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-151.mml"); [%expect{| 3 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-152.mml"); [%expect{| Incorrect assignment of the function return: the function addint returns the type integer but the type boolean was found |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-153.mml"); [%expect{| The function addint has the type integer, it must be assigned to a variable |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-154.mml"); [%expect{| Wrong type of argument in the function addint: the type boolean was found but the type (integer, integer) was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-155.mml"); [%expect{| Wrong type of argument in the function addint: the type (integer, integer, integer) was found but the type (integer, integer) was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-156.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-157.mml"); [%expect{| Incorrect assignment: the function addint does not have a return |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-158.mml"); [%expect{| Wrong type: The type (integer, char, list integer) was found but the type (integer, boolean, list integer) was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-159.mml"); [%expect{| 0 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-160.mml"); [%expect{| 21 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-161.mml"); [%expect{| 3628800 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-162.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-163.mml"); [%expect{| Inconsistent types: The return type integer and boolean were found in the then and else branch |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-164.mml"); [%expect{| 113 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-165.mml"); [%expect{| 64 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-166.mml"); [%expect{| 4 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-167.mml"); [%expect{| 3 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-168.mml"); [%expect{| Wrong type: The type list integer was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-169.mml"); [%expect{| Wrong type: The type boolean was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-170.mml"); [%expect{| Inconsistent types: The return type boolean and integer were found in the then and else branch |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-171.mml"); [%expect{| Wrong type of function return: The function test171 returns the type integer but the type boolean was found |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-172.mml"); [%expect{| The type testType is declared twice |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-173.mml"); [%expect{| Incorrect declaration type: unbound value foo |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-174.mml"); [%expect{| Incorrect declaration type: only channel type can be recursive |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-175.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-176.mml"); [%expect{| 4 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-177.mml"); [%expect{| [3, 4] |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-178.mml"); [%expect{| [3, 4] |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-179.mml"); [%expect{| ([false, true], [1, 3, 4]) |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-180.mml"); [%expect{| Wrong type: The type (integer, integer, boolean) was found but the type integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-181.mml"); [%expect{| Wrong type: The type (integer, integer, boolean) was found but the type (integer, integer) was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-182.mml"); [%expect{| (2, 4, true) |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-183.mml"); [%expect{| Wrong type of function return: The function test183 returns the type (integer, integer, boolean) but the type boolean was found |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-184.mml"); [%expect{| (1, 2, false) |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-185.mml"); [%expect{| Incorrect declaration type: unbound value t2 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-186.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-187.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-188.mml"); [%expect{| syntax error |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-189.mml"); [%expect{| Wrong type: The type boolean was found but the type channel boolean was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-190.mml"); [%expect{| You cannot assign a value to the global variable globalchan |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-191.mml"); [%expect{| unit |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-192.mml"); [%expect{| Wrong type: The type integer was found but the type channel integer was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-193.mml"); [%expect{| Chan@7 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-194.mml"); [%expect{| Chan@10 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-195.mml"); [%expect{| Chan@21 |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-196.mml"); [%expect{| The name test196 is already used by another function/variable |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-197.mml"); [%expect{| true |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-198.mml"); [%expect{| false |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-199.mml"); [%expect{| Wrong type: The type string was found but the type channel string was expected |}]
let%expect_test _ = print_string (main 0 "../../examples/tests/test-200.mml"); [%expect{| "notOK" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-201.mml"); [%expect{| Thread 0: "OK" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-202.mml"); [%expect{| Thread 0: "OK" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-203.mml"); [%expect{|
  Thread 0: "OK"
  Thread 1: 5 |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-204.mml"); [%expect{|
  Thread 2: 100
  Thread 0: "OK"
  Thread 3: 1000
  Thread 1: 10 |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-205.mml"); [%expect{|
  Thread 2: 2
  Thread 0: "OK"
  Thread 5: 5
  Thread 3: 3
  Thread 1: 1
  Thread 4: 4 |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-206.mml"); [%expect{|
  Thread 2: 'K'
  Thread 0: "OK"
  Thread 3: true
  Thread 1: 1 |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-207.mml"); [%expect{|
  Thread 2: true
  Thread 0: "OK"
  Thread 1: 'K' |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-208.mml"); [%expect{|
  Thread 2: 'K'
  Thread 0: "OK"
  Thread 1: 1 |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-209.mml"); [%expect{| Unknown variable: unbound value thread2 |}]
let%expect_test _ = print_string (main 3 "../../examples/tests/test-210.mml"); [%expect{|
  Step 0:
    Thread 0:
      Instruction: spawn thread1(1); spawn thread2('K'); return s
      State: s = "OK";
      Evaluation Context: --

  Possible steps: Beta(0);
  Chosen step: Beta(0)

  Step 1:
    Thread 0:
      Instruction: spawn thread1(1)
      State: s = "OK";
      Evaluation Context: (spawn thread2('K'); return s) :: E

  Possible steps: Spawn(0);
  Chosen step: Spawn(0)

  Step 2:
    Thread 0:
      Instruction: noop
      State: s = "OK";
      Evaluation Context: (spawn thread2('K'); return s) :: E

    Thread 1:
      Instruction: x = i; return x
      State: i = 1; x = 0;
      Evaluation Context: --

  Possible steps: Beta(0); Beta(1);
  Chosen step: Beta(0)

  Step 3:
    Thread 0:
      Instruction: spawn thread2('K')
      State: s = "OK";
      Evaluation Context: (return s) :: E

    Thread 1:
      Instruction: x = i; return x
      State: i = 1; x = 0;
      Evaluation Context: --

  Possible steps: Spawn(0); Beta(1);
  Chosen step: Spawn(0)

  Step 4:
    Thread 2:
      Instruction: x = c; y = (3 * 3); return x
      State: y = 0; c = 'K'; x = '';
      Evaluation Context: --

    Thread 0:
      Instruction: noop
      State: s = "OK";
      Evaluation Context: (return s) :: E

    Thread 1:
      Instruction: x = i; return x
      State: i = 1; x = 0;
      Evaluation Context: --

  Possible steps: Beta(2); Beta(0); Beta(1);
  Chosen step: Beta(0)

  Step 5:
    Thread 2:
      Instruction: x = c; y = (3 * 3); return x
      State: y = 0; c = 'K'; x = '';
      Evaluation Context: --

    Thread 0:
      Instruction: --
      State: s = "OK";
      Evaluation Context: --

    Thread 1:
      Instruction: x = i; return x
      State: i = 1; x = 0;
      Evaluation Context: --

  Possible steps: Beta(2); Beta(1);
  Chosen step: Beta(1)

  Step 6:
    Thread 2:
      Instruction: x = c; y = (3 * 3); return x
      State: y = 0; c = 'K'; x = '';
      Evaluation Context: --

    Thread 0:
      Instruction: --
      State: s = "OK";
      Evaluation Context: --

    Thread 1:
      Instruction: --
      State: i = 1; x = 1;
      Evaluation Context: --

  Possible steps: Beta(2);
  Chosen step: Beta(2)

  Step 7:
    Thread 2:
      Instruction: --
      State: y = 9; c = 'K'; x = 'K';
      Evaluation Context: --

    Thread 0:
      Instruction: --
      State: s = "OK";
      Evaluation Context: --

    Thread 1:
      Instruction: --
      State: i = 1; x = 1;
      Evaluation Context: --

  Possible Steps: --
  Chosen Step: --

      Thread 2: 'K'
      Thread 0: "OK"
      Thread 1: 1 |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-211.mml"); [%expect{| Thread 0: Deadlock or maximum number of step reached |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-212.mml"); [%expect{| Thread 0: Deadlock or maximum number of step reached |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-213.mml"); [%expect{| Thread 0: 3 |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-214.mml"); [%expect{|
  Thread 2: 3
  Thread 0: unit
  Thread 1: Deadlock or maximum number of step reached |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-215.mml"); [%expect{|
  Thread 2: "Sent"
  Thread 0: unit
  Thread 3: 3
  Thread 1: Deadlock or maximum number of step reached |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-216.mml"); [%expect{|
  Thread 2: (3, 5)
  Thread 0: unit
  Thread 1: "Sent" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-217.mml"); [%expect{|
  Thread 2: 3
  Thread 0: unit
  Thread 3: Deadlock or maximum number of step reached
  Thread 1: Deadlock or maximum number of step reached |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-218.mml"); [%expect{|
  Thread 2: (6, 0)
  Thread 0: unit
  Thread 1: "Sent" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-219.mml"); [%expect{|
  Thread 2: 5
  Thread 0: unit
  Thread 1: "Sent" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-220.mml"); [%expect{| Incorrect assignment of the channel reception: the elements passed along the channel have the type boolean but the type integer was found |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-221.mml"); [%expect{| Wrong type of element send: the elements passed along the channel have the type boolean but the type integer was found |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-222.mml"); [%expect{| Wrong type: The type integer was found but the type channel was expected |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-223.mml"); [%expect{|
  Thread 2: 10
  Thread 0: unit
  Thread 1: "Sent" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-224.mml"); [%expect{|
  Thread 2: "Received"
  Thread 0: unit
  Thread 3: 20
  Thread 1: Deadlock or maximum number of step reached |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-225.mml"); [%expect{|
  Thread 2: 20
  Thread 0: unit
  Thread 1: "Sent" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-226.mml"); [%expect{|
  Thread 2: 20
  Thread 0: unit
  Thread 1: "Received" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-227.mml"); [%expect{|
  Thread 0: unit
  Thread 1: 20 |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-228.mml"); [%expect{|
  Thread 2: 20
  Thread 0: unit
  Thread 1: "Sent" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-229.mml"); [%expect{|
  Thread 2: 20
  Thread 0: unit
  Thread 1: ("Received", 4) |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-230.mml"); [%expect{|
  Thread 2: Deadlock or maximum number of step reached
  Thread 0: unit
  Thread 3: 10
  Thread 1: "Sent" |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-231.mml"); [%expect{| syntax error |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-232.mml"); [%expect{| syntax error |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-233.mml"); [%expect{|
  Thread 2: "Received"
  Thread 0: "Send Chosen"
  Thread 1: Deadlock or maximum number of step reached |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-234.mml"); [%expect{|
  Thread 2: ("Sent", 6)
  Thread 7: ("Received", 1)
  Thread 0: "Communication ok"
  Thread 5: ("Sent", 15)
  Thread 6: ("Received", 0)
  Thread 9: ("Received", 3)
  Thread 3: ("Sent", 9)
  Thread 1: ("Sent", 3)
  Thread 8: ("Received", 2)
  Thread 4: ("Sent", 12) |}]
let%expect_test _ = print_string (main 1 "../../examples/tests/test-235.mml"); [%expect{| Thread 0: true |}]