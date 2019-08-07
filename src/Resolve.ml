open Ast

(* For all the function using as parameters envVar & envType:
    - TenvType: (position * string * ast * bool) list 
    - TenvVar: (position * ast * string * ast option) list
*)

exception Multiple_declaration_type of Lexing.position * string
exception Unknow_error_in_resolve of string
exception Unbound_value of Lexing.position * string
exception Undeclared_type of Lexing.position * string
exception Unauthorized_recursivity of Lexing.position
exception Multiple_declaration_var of Lexing.position * string
exception Multiple_declaration_param of Lexing.position * string
exception Type_not_found of Lexing.position * string
exception Function_not_found of Lexing.position * string
exception Start_not_found

(* find_rec: ast -> string -> bool
Function telling if a type is recursive or not
Parameters:
  - typeNode: abstract syntax tree representing the type
  - name: type name
Return: boolean meaning if the type is recursive or not
*)

let rec find_rec typeNode name =
  match typeNode with
    | NamedTypeNode(_,n) -> (String.equal n name) 
    | TypeNode (_) -> false
    | ChanTNode (_,st) -> find_rec st name
    | ListTNode (_,st) -> find_rec st name
    | TupleTNode (_,tSeq) -> find_rec tSeq name         
    | TypeSeqNode (st,None) -> find_rec st name 
    | TypeSeqNode (st1, Some(st2)) -> if (find_rec st1 name) then true else find_rec st2 name
    | _ -> raise (Unknow_error_in_resolve "find_rec")


(* create_env: TenvType * TenvVar * 
  (position * string * ast) option -> ast -> 
  TenvType * TenvVar *
  (position * string * ast) option
Function creating the type and variable environments thanks to the ast of the program
Parameters:
  - envType: list of the type declarations already found
  - envVar: list of the the variable/function declarations already found
  - start: the ast representing the start function if already found
  - tree: ast to analyze
Return: Type and variable environments
*)

let rec create_env (envType,envVar,start) tree =
    match tree with
        | ProgramNode (p1,p2) -> 
      create_env (create_env (envType,envVar,start) p1) p2
        | FunctionNode (pos,ft,name,params,_) ->
      (envType,(pos,ft,name,Some(params),true)::envVar,start)
        | GlobalVarDeclaNode (pos,t,name,_) -> 
      (envType,(pos,t,name,None,true)::envVar,start)
        | GlobalChanDeclaNode (pos,t,name) -> 
      (envType,(pos,t,name,None,true)::envVar,start)       
        | TypeDeclaNode (pos,name,t) ->
      ((pos,name,t,find_rec t name)::envType,envVar,start)
        | CallNode (pos,name,expr) ->
      (envType,envVar,Some((pos,name,expr))) 
        | _ -> 
      (envType,envVar,start)

(* well_formed_type_aux: ast -> string list -> bool
Function checking that the type declared is well-formed 
Parameters:
  - t: abstract syntax tree representing the type
  - nameList: list of names of already declared types
Return: true if the type is well-formed, false if not
*)

let rec well_formed_type_aux t nameList  =
  match t with
   | TypeNode (_,IntegerT) -> true
   | TypeNode (_,BooleanT) -> true
   | TypeNode (_,StringT) -> true
   | TypeNode (_,CharT) -> true
   | ChanTNode (_,st) -> well_formed_type_aux st nameList 
   | ListTNode (_,st) -> well_formed_type_aux st nameList 
   | TupleTNode (_,tSeq) -> well_formed_type_aux tSeq nameList 
   | NamedTypeNode (pos,name) -> if (List.mem name nameList) then true else raise (Unbound_value (pos,name))         
   | TypeSeqNode (st,None) -> well_formed_type_aux st nameList 
   | TypeSeqNode (st1, Some(st2)) -> if (well_formed_type_aux st1 nameList)
                                     then well_formed_type_aux st2 nameList
                                     else raise (Unknow_error_in_resolve "well_formed_type_aux2")
   | _ -> raise (Unknow_error_in_resolve "well_formed_type_aux")


(* well_formed_type_decla: position * string * ast * bool -> string list -> bool
Function checking that the declared type is recursive only if it is a channel type and if so check if it is well-formed
Parameters:
  - pos: the position in the file of the type declaration
  - name: the name of the type declared
  - t: abstract syntax tree representing the type
  - nameList: list of names of already declared types
Return: true if the type is completely well-formed (well-formed and recursive in the right cases), false if not
*)

let well_formed_type_decla (pos,name,t,tRec) nameList =
  try (
    match t with 
      | ChanTNode(_,t) -> (well_formed_type_aux t nameList)
      | _ -> if (tRec) then raise (Unauthorized_recursivity pos)
             else well_formed_type_aux t (List.filter (fun e -> not (String.equal e name)) nameList))
  with
    | Unbound_value (posn,n) -> raise (Undeclared_type (posn,n))


(* well_formed_envType: TenvType -> string list
Function checking that all declared types are unique and completely well-formed
Parameters:
  - envType: list of the type declarations
Return: the names of the declared types
*)

let well_formed_envType envType =
  let rec aux envType nameList = 
    match envType with
      | [] -> nameList
      | (pos,name,t,trec)::q -> if (List.mem name nameList) 
                           then raise (Multiple_declaration_type (pos,name))                   
                           else let _ = well_formed_type_decla (pos,name,t,trec) (name::nameList) in
                           (aux q (name::nameList))
  in aux envType []


(* uniqueVar: TenvVar -> string list -> string list -> string list
Function checking that all variable/function declared are unique 
Parameters:
  - envVar: list of the variable/function declarations
  - nameListVar: list of names of already declared variables/functions
  - nameListType: list of names of all the declared types
Return: the names of the declared variables/functions
*)

let rec uniqueVar envVar nameListVar nameListType = 
  match envVar with
    | [] -> nameListVar
    | (pos,_,name,None,_)::q -> if (List.mem name nameListVar) 
                         then raise (Multiple_declaration_var (pos,name))                   
                         else uniqueVar q (name::nameListVar) nameListType
    | (pos,_,name,_,_)::q -> if (List.mem name nameListVar) 
                         then raise (Multiple_declaration_var (pos,name))                   
                         else uniqueVar q (name::nameListVar) nameListType


(* well_formed_func_type: ast -> string list -> bool
Function checking that the type of the function declared is well-formed 
Parameters:
  - t: abstract syntax tree representing the function type
  - nameList: list of names of all the declared types
Return: true if the function type is well-formed, false if not
*)

let well_formed_func_type t nameList = 
  match t with
    | FuncTNode (None) -> true
    | FuncTNode (Some(st)) -> well_formed_type_aux st nameList 
    | _ -> raise (Unknow_error_in_resolve "well_formed_func_type")


(* well_formed_params: ast -> string list -> string list -> string list
Function checking that all the type of the function parameters are well-formed 
and that all the names are not used twice in the function paraneters
Parameters:
  - params: abstract syntax tree representing the parameters of the function
  - nameListType: list of names of all the declared types
  - nameListVar: list of names of already declared parameters
Return: the names of all the function parameters
*)

let rec well_formed_params params nameListType nameListVar =
  match params with 
    | ParamsNode (pos,t,name,None) -> if (List.mem name nameListVar) 
                  then raise (Multiple_declaration_param (pos,name))
                  else let _ = (well_formed_type_aux t nameListType) in name::nameListVar
    | ParamsNode (pos,t,name,Some(p)) -> if (List.mem name nameListVar) 
                  then raise (Multiple_declaration_param (pos,name))
                  else let _ = (well_formed_type_aux t nameListType) in well_formed_params p nameListType (name::nameListVar)
    | _ -> raise (Unknow_error_in_resolve "well_formed_params") 


(* well_formed_var: (position * ast * string * ast option) -> string list -> bool
Function checking that :
  - for a variable: the type of the variable declared is well-formed
  - for a function: the type of the function declared is well-formed and 
and that all the function paraneters are completely well-formed
Parameters:
  - decla: abstract syntax tree representing a variable or a function
  - nameListType: list of names of all the declared types
Return: true if the variable/function is well-formed, false if not
*)

let well_formed_var decla nameListType =
  match decla with
    | (_,t,_,None,_) -> well_formed_type_aux t nameListType 
    | (_,ft,_,Some(params),_) -> let _ = (well_formed_params params nameListType []) in (well_formed_func_type ft nameListType)


(* well_formed_envVar: TenvVar -> string list -> string list
Function checking that all the variables/functions declared at top-level are unique and well-formed
Parameters:
  - envVar: list of the the variable/function declarations
  - nameListType: list of names of all the declared types
Return: the names of all the the variables/functions declared at top-level
*)

let well_formed_envVar envVar nameListType =
    let names = uniqueVar envVar [] nameListType in
    try 
      let _ = List.for_all (fun d -> well_formed_var d nameListType) envVar in
      names
    with
      | Unbound_value (pos,n) -> raise (Type_not_found (pos,n))
      

(* well_formed_start: (position * string * ast) option -> TenvVar -> (position * ast * string * ast option)
Function checking that the function used in the start function is already declared
Parameters:
  - decla: tuple representing the start function
  - envVar: list of the the variable/function declarations
Return: the tuple representing the start function
*)

let well_formed_start decla envVar =
  match decla with 
    | Some((pos,name,_)) -> (try 
                              List.find (fun (_,_,n,p,_) -> (p != None) && (String.equal n name)) envVar 
                            with
                              | Not_found -> raise (Function_not_found (pos,name)))
    | _ -> raise (Unknow_error_in_resolve "well_formed_start")
