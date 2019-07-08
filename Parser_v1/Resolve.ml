open Ast

exception Multiple_declaration_type of Lexing.position * string
exception Unknow_error_in_type_checking
exception Unbound_value of Lexing.position * string
exception Undeclared_type of Lexing.position * string
exception Unauthorized_recursivity of Lexing.position
exception Multiple_declaration_var of Lexing.position * string

let rec create_env (envType,envVar) tree =
    match tree with
        | (ProgramNode (p1,p2)) -> 
      create_env (create_env (envType,envVar) p1) p2
        | (FunctionNode (_,_,_,_,_)) ->
      (envType,tree::envVar)
        | (VariableDeclaNode (_,_,StringNode(_,_))) -> 
      (envType,tree::envVar)      
        | (TypeDeclaNode (_,_,_)) ->
        (tree::envType,envVar)
        | _ -> 
      (envType,envVar)

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
    List.for_all (fun d -> well_formed_type_decla d nameList) envType

let rec uniqueVar envVar nameListVar nameListType = 
  match envVar with
    | [] -> nameListVar
    | VariableDeclaNode(pos,t,StringNode(_,name))::q -> if (List.mem name nameListVar) 
                         then raise (Multiple_declaration_var (pos,name))                   
                         else let b = (well_formed_type_aux t nameListType) in 
                         (uniqueVar q (name::nameList))
    | VariableDeclaNode(pos,name,_)::q -> if (List.mem name nameList) 
                         then raise (Multiple_declaration_var (pos,name))                   
                         else (uniqueVar q (name::nameList))
    | _ -> raise Unknow_error_in_type_checking

let well_formed_envVar envVar =
    let nameList = uniqueType envType [] in
    List.for_all (fun d -> well_formed_type d nameList) envType