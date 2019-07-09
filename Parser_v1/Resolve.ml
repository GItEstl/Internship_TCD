open Ast

exception Multiple_declaration_type of Lexing.position * string
exception Unknow_error_in_type_checking
exception Unbound_value of Lexing.position * string
exception Undeclared_type of Lexing.position * string
exception Unauthorized_recursivity of Lexing.position
exception Multiple_declaration_var of Lexing.position * string
exception Multiple_declaration_param of Lexing.position * string
exception Type_not_found of Lexing.position * string
exception Function_not_found of Lexing.position * string
exception Start_not_found

let rec create_env (envType,envVar,start) tree =
    match tree with
        | ProgramNode (p1,p2) -> 
      create_env (create_env (envType,envVar,start) p1) p2
        | FunctionNode (_,_,_,_,_) ->
      (envType,tree::envVar,start)
        | VariableDeclaNode (_,_,StringNode(_,_)) -> 
      (envType,tree::envVar,start)      
        | TypeDeclaNode (_,_,_) ->
      (tree::envType,envVar,start)
        | CallNode (_,_,_) ->
      (envType,envVar,Some(tree)) 
        | _ -> 
      (envType,envVar,start)

let rec uniqueType envType nameList = 
  match envType with
    | [] -> nameList
    | TypeDeclaNode(pos,name,_)::q -> if (List.mem name nameList) 
                         then raise (Multiple_declaration_type (pos,name))                   
                         else (uniqueType q (name::nameList))
    | _ -> raise Unknow_error_in_type_checking

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
                                     else raise Unknow_error_in_type_checking
   | _ -> raise Unknow_error_in_type_checking

let well_formed_type_decla decla nameList =
  match decla with
    | (TypeDeclaNode (pos,name,t)) ->
      (match t with 
        | ChanTNode(_,t) -> 
          (try
            (well_formed_type_aux t nameList)
          with
            | Unbound_value (pos,n) -> raise (Undeclared_type (pos,n))) 
        | _ -> 
          (try
            well_formed_type_aux t (List.filter (fun e -> not (String.equal e name)) nameList)
          with
            | Unbound_value (posn,n) -> if (String.equal n name) then raise (Unauthorized_recursivity pos) else raise (Undeclared_type (posn,n))))
    | _ -> raise Unknow_error_in_type_checking

let well_formed_envType envType =
    let nameList = uniqueType envType [] in
    let _ = List.for_all (fun d -> well_formed_type_decla d nameList) envType in
    nameList

let rec uniqueVar envVar nameListVar nameListType nameListFunc = 
  match envVar with
    | [] -> (nameListVar,nameListFunc)
    | VariableDeclaNode (pos,_,StringNode(_,name))::q -> if (List.mem name nameListVar) 
                         then raise (Multiple_declaration_var (pos,name))                   
                         else uniqueVar q (name::nameListVar) nameListType nameListFunc
    | FunctionNode (pos,_,name,_,_)::q -> if (List.mem name nameListVar) 
                         then raise (Multiple_declaration_var (pos,name))                   
                         else uniqueVar q (name::nameListVar) nameListType (name::nameListFunc)
    | _ -> raise Unknow_error_in_type_checking

let well_formed_func_type t nameList = 
  match t with
    | FuncTNode (None) -> true
    | FuncTNode (Some(st)) -> well_formed_type_aux st nameList 
    | _ -> raise Unknow_error_in_type_checking

let rec well_formed_params params nameListType nameListVar =
  match params with 
    | ParamsNode (pos,t,name,None) -> if (List.mem name nameListVar) 
                  then raise (Multiple_declaration_param (pos,name))
                  else let _ = (well_formed_type_aux t nameListType) in name::nameListVar
    | ParamsNode (pos,t,name,Some(p)) -> if (List.mem name nameListVar) 
                  then raise (Multiple_declaration_param (pos,name))
                  else let _ = (well_formed_type_aux t nameListType) in well_formed_params p nameListType (name::nameListVar)
    | _ -> raise Unknow_error_in_type_checking 

let well_formed_var decla nameListType =
  match decla with
    | VariableDeclaNode (_,t,_) -> well_formed_type_aux t nameListType 
    | FunctionNode (_,ft,_,params,_) -> let _ = (well_formed_params params nameListType []) in (well_formed_func_type ft nameListType)
    | _ -> raise Unknow_error_in_type_checking

let well_formed_envVar envVar nameListType =
    let names = uniqueVar envVar [] nameListType [] in
    try 
      let _ = List.for_all (fun d -> well_formed_var d nameListType) envVar in 
      names
    with
      | Unbound_value (pos,n) -> raise (Type_not_found (pos,n))
      
let well_formed_start decla nameListFunc =
  match decla with 
    | Some(CallNode (pos,name,_)) -> if (List.mem name nameListFunc) then true else raise (Function_not_found (pos,name))
    | _ -> raise Unknow_error_in_type_checking  
