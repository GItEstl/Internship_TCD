open Ast

let rec create_env (envType,envVar) tree =
    match tree with
        | (ProgramNode (p1,p2)) -> 
      create_env (create_env (envType,envVar) p1) p2
        | (FunctionNode (_,_,_,_)) ->
      (envType,tree::envVar)
        | (VariableDeclaNode (_,_)) -> 
      (envType,tree::envVar)      
        | (TypeDeclaNode (_,_)) ->
        (tree::envType,envVar)
        | _ -> 
      (envType,envVar)

let well_formed_envType envType =
  List.fold_right (fun d -> well_formed_type d) envType true

let well_formed_type decla =
  match decla with
    | 