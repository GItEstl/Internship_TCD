open Ast
open Resolve

(* For all the function using as parameters envVar & envType:
    - TenvType: (position * string * ast * bool) list 
    - TenvVar: (position * ast * string * ast option * bool) list
*)

(* Type of the epxressions *)
type typeType =
  | IntegerType
  | BooleanType
  | StringType
  | CharType
  | VoidType
  | ChannelType of typeType
  | ChannelGenType
  | ListType of typeType
  | ListGenType
  | TupleType of typeType list
  | TupleGenType
  | NamedType of string
  | ErrorType

(* Type of the instruction *)
type instrType = 
  | OK (* Type of well_typed instruction other than a return instruction *)
  | OKt of typeType (* Type of return instruction *)

exception Different_types_in_list
exception Impossible_unify_instr of typeType * typeType
exception Wrong_type of Lexing.position * typeType * typeType
exception Inconsistent_types of Lexing.position * typeType * typeType
exception Unknown_variable of Lexing.position * string
exception Unknown_function of Lexing.position * string
exception Not_type_of_the_params of Lexing.position * typeType * typeType * string
exception Not_type_of_the_return of Lexing.position * typeType * typeType * string
exception Assignement_of_void of Lexing.position * string
exception No_assignement_of_the_return of Lexing.position * typeType * string
exception Wrong_type_chan_receive of Lexing.position * typeType * typeType
exception Wrong_type_chan_send of Lexing.position * typeType * typeType
exception Different_type_of_return_if of Lexing.position * typeType * typeType
exception Return_not_match_with_decla of Lexing.position * typeType * typeType * string
exception Different_type_of_return_func of Lexing.position * string
exception Illegal_type_argument of Lexing.position
exception Get_tuple_too_short of Lexing.position
exception Assignment_to_global_var of Lexing.position * string
exception Multiple_assignments of Lexing.position
exception Unknown_error_type_checking of string

(* string_of_type: typeType -> string
Function converting a typeType into a string
Parameter:
  - t: type to convert
Return: string corresponding to the type
*)
let rec string_of_type t =
  match t with
    | IntegerType -> "integer"
    | BooleanType -> "boolean"
    | StringType -> "string"
    | CharType -> "char"
    | VoidType -> "void"
    | ChannelType(st) -> "channel " ^ (string_of_type st)
    | ChannelGenType -> "channel"
    | ListType(st) -> "list " ^ (string_of_type st)
    | ListGenType -> "list"
    | TupleType(st) -> "(" ^ (List.fold_left (fun s e ->  s ^ ", " ^ (string_of_type e)) (string_of_type (List.nth st 0)) (List.tl st)) ^ ")"
    | TupleGenType -> "tuple"
    | NamedType(s) -> s 
    | ErrorType -> "ERROR"

(* type_of_tree_aux: ast -> TenvType -> typeType
Function converting an abstract syntax tree into a typeType but if it is a recursive named type the function
does not replace it with its real type instead it just leave its name
Parameters:
  - tree: abstract syntax tree representing the type
  - envType: list of the type declarations
Return: typeType corresponding to the ast
*) 
let rec type_of_tree_aux tree envType =
  match tree with
    | TypeNode (_,IntegerT) -> IntegerType
    | TypeNode (_,BooleanT) -> BooleanType
    | TypeNode (_,StringT) -> StringType
    | TypeNode (_,CharT) -> CharType
    | ChanTNode (_,st) -> ChannelType (type_of_tree_aux st envType) 
    | ListTNode (_,st) -> ListType (type_of_tree_aux st envType)
    | TupleTNode (_,TypeSeqNode(st,None)) -> type_of_tree_aux st envType
    | TupleTNode (_,tSeq) -> TupleType (
      let rec aux t l = match t with
        | TypeSeqNode (st,None) -> List.rev ((type_of_tree_aux st envType)::l) 
        | TypeSeqNode (st1, Some(st2)) -> aux st2 ((type_of_tree_aux st1 envType)::l)
        | _ -> raise (Unknown_error_type_checking "type_of_tree")
      in aux tSeq [])
    | NamedTypeNode (_,st) -> let _,_,t,tRec = (List.find (fun (_,name,_,_) -> String.equal name st) envType) in if (tRec) then NamedType(st) else (type_of_tree_aux t envType)
    | FuncTNode (None) -> VoidType
    | FuncTNode (Some(t)) -> type_of_tree_aux t envType        
    | _ -> raise (Unknown_error_type_checking "type_of_tree")

(* type_of_tree: ast -> TenvType -> typeType
Function converting an abstract syntax tree into a typeType
Parameters:
  - tree: abstract syntax tree representing the type
  - envType: list of the type declarations
Return: typeType corresponding to the ast
*)
let type_of_tree tree envType = 
  match tree with 
    | NamedTypeNode(_,st) -> let _,_,t,_ = (List.find (fun (_,name,_,_) -> String.equal name st) envType) in (type_of_tree_aux t envType)
    | _ -> type_of_tree_aux tree envType

(* type_of_var: string -> TenvVar -> TenvType -> position -> typeType
Function finding the type of a variable thanks to its name
Parameters:
  - namevar: name of the variable
  - envVar: list of the variable/function declarations
  - envType: list of the type declarations
  - pos: the position in the file of the variable
Return: typeType corresponding to the variable
*)
let type_of_var namevar envVar envType pos =
  try 
    let _,tnode,_,_,g = List.find (fun (_,_,name,p,_) -> (String.equal name namevar) && (p == None)) envVar in
    (type_of_tree tnode envType,g)
  with
    | Not_found -> raise (Unknown_variable (pos,namevar))

(* type_of_func: string -> TenvVar -> TenvType -> position -> typeType
Function finding the type of a function thanks to its name
Parameters:
  - namef: name of the function
  - envVar: list of the variable/function declarations
  - envType: list of the type declarations
  - pos: the position in the file of the function
Return: typeType corresponding to the function
*)
let type_of_func namef envVar envType pos =
  try 
    let _,tnode,_,pnode,_ = List.find (fun (_,_,name,p,_) -> (String.equal name namef) && (p != None)) envVar in
    (type_of_tree tnode envType,pnode)
  with
    | Not_found -> raise (Unknown_variable (pos,namef))

(* compare: TenvType -> typeType -> typeType -> bool * typeType
Function comparing two types
Parameters:
  - envType: list of the type declarations
  - type1: type to be compared to type2
  - type2: type to be compared to type1
Return: a pair of:
          - a boolean which means if the two type are equals or not
          - a type : the type of the two types if they are equals, ErrorType if not
*)
let rec compare envType type1 type2 =
  match (type1,type2) with
    | (IntegerType,IntegerType) -> (true,IntegerType)
    | (BooleanType,BooleanType) -> (true,BooleanType)
    | (StringType,StringType) -> (true,StringType)
    | (CharType,CharType) -> (true,CharType)
    | (ChannelType(st1),ChannelType(st2)) -> if (fst(compare envType st1 st2)) then (true,ChannelType(st2)) else (false,ErrorType)
    | (ChannelType(st),ChannelGenType) | (ChannelGenType,ChannelType(st)) -> (true,ChannelType(st))
    | (ListType(st1),ListType(st2)) -> if (fst(compare envType st1 st2)) then (true,ListType(st2)) else (false,ErrorType)
    | (ListType(st),ListGenType) | (ListGenType,ListType(st)) -> (true,ListType(st))
    | (TupleType(st1),TupleType(st2)) -> if (List.for_all2 (fun a b -> fst(compare envType a b)) st1 st2) then (true,TupleType(st2)) else (false,ErrorType)
    | (TupleType(st),TupleGenType) | (TupleGenType,TupleType(st)) -> (true,TupleType(st))
    | (NamedType(n1),NamedType(n2)) -> if (String.equal n1 n2) then (true,NamedType(n2)) else (false,ErrorType)
    | (NamedType(st),t) | (t,NamedType(st)) -> let _,_,tnode,_ = (List.find (fun (_,name,_,_) -> String.equal name st) envType) in compare envType (type_of_tree tnode envType) t
    | (VoidType,VoidType) -> (true,VoidType)
    | _ -> (false,ErrorType)

(* unify_instr: typeType -> typeType -> TenvType -> instrType
Function trying to determine the type of the instructions sequence i1;i2
Parameters:
  - ti1: type of the instruction i1
  - ti2: type of the instruction i2
  - envType: list of the type declarations
Return: the instrType corresponding to the instructions sequence
*)
let unify_instr ti1 ti2 envType = 
  match (ti2,ti1) with
    | (OK,t) -> t
    | (t,OK) -> t
    | (OKt(t1),OKt(t2)) ->
    try
      let b,t = compare envType t1 t2 in
      if (b) then OKt(t) else raise (Impossible_unify_instr (t1,t2))
    with 
      | Invalid_argument _ -> raise (Impossible_unify_instr (t1,t2))

(* type_of_expr: ast -> TenvVar -> TenvType -> typeType
Function determining the type of an expression
Parameters:
  - expr: abstract syntax tree representing the expression
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
Return: the typeType of the expression
*)
let rec type_of_expr expr envVar envType =
  match expr with
    | BinaryNode (pos,left,op,right) -> ruleBinary op left right envVar envType pos
    | UnaryNode (pos,op,expr) -> ruleUnary op expr envVar envType pos
    | IfthenelseExprNode (pos,cond,e1,e2) -> ruleIfThenElseExpr cond e1 e2 envVar envType pos
    | ExprNode (_,e) -> type_of_expr e envVar envType
    | ExprsNode (e,None) -> type_of_expr e envVar envType
    | ExprsNode (e1,Some(e2)) -> ruleTupleExpr (ExprsNode (e1,Some(e2))) envVar envType
    | ValueNode (v) -> ruleValue v envType
    | AssignNode (pos,a) -> ruleIdentifier a envVar envType false pos
    | _ -> raise (Unknown_error_type_checking "type_of_expr")

and 

(* ruleUnary: unary -> ast -> TenvVar -> TenvType -> position -> typeType
Function determining the type of an unary expression
Parameters:
  - op: unary operator to apply
  - expr: abstract syntax tree representing the expression on which the operator is applied
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the operator (in case of error)
Return: the typeType of the unary expression
*)
ruleUnary op expr envVar envType pos = 
  try
    let texpr = (type_of_expr expr envVar envType) in
    (match op with
    | NegateInt->
      let be,_ = compare envType texpr IntegerType in
      (if (be) then IntegerType else raise (Wrong_type (pos,texpr,IntegerType)))
    | (Odd | Even)->
      let be,_ = compare envType texpr IntegerType in
      (if (be) then BooleanType else raise (Wrong_type (pos,texpr,IntegerType)))
    | NegateBool ->
      let be,_ = compare envType texpr BooleanType in
      (if (be) then BooleanType else raise (Wrong_type (pos,texpr,BooleanType)))
    | Head ->
      let _,te = compare envType texpr ListGenType in
      (match te with
        | ListType(st) -> st
        | _ -> raise (Wrong_type (pos,texpr,ListGenType))) 
    | Tail ->
      let _,te = compare envType texpr ListGenType in
      (match te with
      | ListType(_) -> te
      | _ -> raise (Wrong_type (pos,texpr,ListGenType))) 
    | Fst ->
      let _,te = compare envType texpr TupleGenType in
      (match te with
      | TupleType(st) -> List.hd st
      | _ -> raise (Wrong_type (pos,texpr,TupleGenType))) 
    | Snd ->
      let _,te = compare envType texpr TupleGenType in
      (match te with
      | TupleType(st) -> List.nth st 1
      | _ -> raise (Wrong_type (pos,texpr,TupleGenType))) 
    )
  with
    | Invalid_argument _ -> raise (Illegal_type_argument pos)

and

(* ruleBinary: binary -> ast -> ast -> TenvVar -> TenvType -> position -> typeType
Function determining the type of a binary expression
Parameters:
  - op: binary operator to apply
  - left: abstract syntax tree representing the expression to the left of the operator
  - right: abstract syntax tree representing the expression to the right of the operator
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the operator (in case of error)
Return: the typeType of the binary expression
*)
ruleBinary op left right envVar envType pos =
  try
    let tleft = (type_of_expr left envVar envType) in
    let tright = (type_of_expr right envVar envType) in
      (match op with
      | (Equal | Different) -> 
        let (_,t) = compare envType tleft tright in
          (match t with 
            | IntegerType | BooleanType | CharType | StringType | ChannelType(_) -> BooleanType
            | ErrorType -> raise (Wrong_type (pos,tright,tleft))
            | _ -> raise (Illegal_type_argument pos))
      | (Lesser | Greater) ->
        let (bl,_) = compare envType tleft IntegerType in
        let (br,_) = compare envType tright IntegerType in
          (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tright,IntegerType))) 
          else raise (Wrong_type (pos,tleft,IntegerType)))
      | (Add | Substract | Multiply | Divide) ->
        let (bl,_) = compare envType tleft IntegerType in
        let (br,_) = compare envType tright IntegerType in
          (if (bl) then (if (br) then IntegerType else raise (Wrong_type (pos,tright,IntegerType))) 
          else raise (Wrong_type (pos,tleft,IntegerType)))
      | (Or | And) ->
        let (bl,_) = compare envType tleft BooleanType in
        let (br,_) = compare envType tright BooleanType in
          (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tright,BooleanType))) 
          else raise (Wrong_type (pos,tleft,BooleanType)))
      | Get -> 
        let (b1,t1) = compare envType tleft IntegerType in
        let (b2,t2) = compare envType tright TupleGenType in
          (if (b1) then (
            if (b2) then
              match (left,t2) with
                | ValueNode(ValueNode(IntegerNode(_,i))),TupleType(t) -> List.nth t i 
                | _ -> raise (Unknown_error_type_checking "ruleBinary") 
            else raise (Wrong_type (pos,t2,TupleGenType))) 
          else raise (Wrong_type (pos,t1,IntegerType)))
      | _ -> raise (Unknown_error_type_checking "ruleBinary"))
  with 
    | Invalid_argument _ -> raise (Illegal_type_argument pos)
    | Failure _ -> raise (Get_tuple_too_short pos)

and

(* ruleIfThenElseExpr: ast -> ast -> ast -> TenvVar -> TenvType -> position -> typeType
Function determining the type of a conditional expression
Parameters:
  - cond_expr: abstract syntax tree representing the expression inside the condition
  - then_expr: abstract syntax tree representing the expression inside the then branch
  - else_expr: abstract syntax tree representing the expression inside the else branch
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the if symbol (in case of error)
Return: the typeType of the conditional expression
*)
ruleIfThenElseExpr cond_expr then_expr else_expr envVar envType pos =
  let tcond_expr = (type_of_expr cond_expr envVar envType) in
  let tthen_expr = (type_of_expr then_expr envVar envType) in
  let telse_expr = (type_of_expr else_expr envVar envType) in
  let bc,_ = compare envType tcond_expr BooleanType in
  let be,te = compare envType tthen_expr telse_expr in
    (if (bc) then (if (be) then te else raise (Inconsistent_types (pos,tthen_expr,telse_expr))) 
    else raise (Wrong_type (pos,te,BooleanType)))

and

(* ruleTupleExpr: ast -> TenvVar -> TenvType -> typeType
Function determining the type of an expression tuple
Parameters:
  - exprs: abstract syntax tree representing the tuple of expressions
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
Return: the typeType of the expression tuple
*)
ruleTupleExpr exprs envVar envType =
  TupleType (
  let rec aux exs l =
    (match exs with
      | ExprsNode (e,None) -> List.rev ((type_of_expr e envVar envType)::l)
      | ExprsNode (e1,Some(e2)) -> aux e2 ((type_of_expr e1 envVar envType)::l)
      | _ -> raise (Unknown_error_type_checking ("ruleTupleExpr")))
  in aux exprs [])

and

(* ruleValue: ast -> TenvType -> typeType
Function determining the type of a value
Parameters:
  - v: abstract syntax tree representing the value
  - envType: list of the type declarations
Return: the typeType of the value
*)
ruleValue v envType =
  match v with
    | ValueNode (IntegerNode (_,_)) -> IntegerType
    | ValueNode (CharNode (_,_)) -> CharType
    | ValueNode (StringNode (_,_)) -> StringType
    | ValueNode (TrueNode(_)) -> BooleanType
    | ValueNode (FalseNode(_)) -> BooleanType
    | ValueNode (EmptyList) -> ListGenType
    | ValueNode (ValueSeqNode (v,None)) -> ListType (ruleValue v envType)
    | ValueNode (ValueSeqNode (v,Some(vs))) -> ListType (
        let rec aux v1 vs1 =
          (match vs1 with
            | ValueSeqNode (v2,None) -> 
              let bv, tv = compare envType (ruleValue v1 envType) (ruleValue v2 envType) in
                if (bv) then tv else raise (Different_types_in_list)
            | ValueSeqNode (v2,Some(vs2)) ->
              let bv,_ = compare envType (ruleValue v1 envType) (ruleValue v2 envType) in
              if (bv) then (aux v2 vs2) else raise (Different_types_in_list)
            | _ -> raise (Unknown_error_type_checking ("ruleValue")))
        in aux v vs)
    | ValueSeqNode (v,None) -> ruleValue v envType
    | ValueSeqNode (_,Some(_)) -> TupleType (
        let rec aux vals l =
          (match vals with
            | ValueSeqNode (v1,None) -> List.rev ((ruleValue v1 envType)::l)
            | ValueSeqNode (v1,Some(v2)) -> aux v2 ((ruleValue v1 envType)::l)
            | _ -> raise (Unknown_error_type_checking ("ruleValue")))
        in aux v [])  
    | _ -> raise (Unknown_error_type_checking "ruleValue")

and

(* ruleAssignable: position -> ast -> TenvVar -> TenvType -> typeType
Function determining the type of an assignable
Parameters:
  - pos: position of the assignable (in case of error)
  - assign: abstract syntax tree representing the assignable
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
Return: the typeType of the assignable
*)
ruleAssignable pos assign envVar envType =
  match assign with
    | ExprNode (_,a) -> ruleAssignable pos a envVar envType
    | ExprsNode (a,None) -> ruleAssignable pos a envVar envType
    | ExprsNode (_,Some(_)) -> raise (Multiple_assignments pos)
    | AssignNode (pos,a) -> ruleIdentifier a envVar envType true pos
    | _ -> raise (Unknown_error_type_checking "ruleAssignable")

and

(* ruleIdentifier: ast -> TenvVar -> TenvType -> boolean -> position -> typeType
Function determining the type of an assignable and verifying that the identifier can be used
(no global variable on the left side of an assignment)
Parameters:
  - a: abstract syntax tree representing the identifier
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - checkStatus: true if the identifier corresponds to a global variable 
  - pos: position of the identifier (in case of error)
Return: the typeType of the identifier
*)
ruleIdentifier a envVar envType checkStatus pos = 
  let t,b = type_of_var a envVar envType pos in
  if (checkStatus) then (if b then raise (Assignment_to_global_var (pos,a)) else t)
  else t

(* ruleInstr: ast -> TenvVar -> TenvType -> typeType
Function determining the type of a sequence of instructions
Parameters:
  - i: abstract syntax tree representing the sequence
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
Return: the instrType of the instruction sequence
*)
let rec ruleInstr i  envVar envType =
  let rec aux i tInstr =
    match i with
      | InstrSeqNode(NoopNode,None) -> tInstr 
      | InstrSeqNode(NoopNode,Some(i)) -> aux i tInstr
      | InstrSeqNode (bi,None) -> unify_instr (type_of_binstr bi envVar envType) tInstr envType
      | InstrSeqNode (bi,Some(i)) -> aux i (unify_instr (type_of_binstr bi envVar envType) tInstr envType)
      | _ -> raise (Unknown_error_type_checking ("ruleInstr"))
  in aux i OK 
    
and

(* type_of_instr: ast -> TenvVar -> TenvType -> instrType
Function determining the type of a basic instruction
Parameters:
  - binstr: abstract syntax tree representing the basic instruction
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
Return: the instrType corresponding to the basic instruction
*)
type_of_binstr binstr envVar envType =
  match binstr with
    | BinaryNode (pos,a,Assign,CallNode (posf,namef,e)) -> ruleCallFuncWithReturn a namef e envVar envType pos posf
    | BinaryNode (pos,a,Assign,e) -> ruleAssignInstr a e envVar envType pos
    | CallNode (pos,namef,e) -> ruleCallFuncVoid namef e envVar envType pos
    | ReceiveNode (pos,a,namechan) -> ruleReceive a namechan envVar envType pos
    | SendNode (pos,namechan,e) -> ruleSend namechan e envVar envType pos
    | IfthenelseInstrNode (pos,cond,i1,i2) -> ruleIfThenElseInstr cond i1 i2 envVar envType pos
    | WhileNode (pos,e,i) -> ruleWhile e i envVar envType pos
    | ChooseNode (_,c) -> ruleChoose c envVar envType
    | SpawnNode (pos,namef,e) -> ruleSpawn namef e envVar envType pos
    | NewNode (pos,a) -> ruleNew a envVar envType pos
    | ReturnNode (_,None) -> ruleReturnVoid
    | ReturnNode (pos,Some (CallNode (_,namef,e))) -> ruleReturnFunc namef e envVar envType pos
    | ReturnNode (_,Some (e)) -> ruleReturnExpr e envVar envType
    | _ -> raise (Unknown_error_type_checking ("type_of_binstr"))

and

(* type_of_params: ast -> TenvType -> typeType
Function determining the types of the function parameters
Parameters:
  - paramNode: abstract syntax tree representing the parameters
  - envType: list of the type declarations
Return: the typeType of the parameters (in the form of a TupleType)
*)
type_of_params paramNode envType =
  match paramNode with 
    | Some(ParamsNode (_,t,_,None)) -> type_of_tree t envType
    | Some(ParamsNode (_,_,_,Some(_))) -> TupleType( 
      (let rec aux params l =
        match params with
          | Some(ParamsNode (_,t,_,None)) -> List.rev ((type_of_tree t envType)::l)
          | Some(ParamsNode (_,t,_,ps)) -> aux ps ((type_of_tree t envType)::l)
          | _ -> raise (Unknown_error_type_checking ("type_of_params"))        
      in aux paramNode []))
    | _ -> raise (Unknown_error_type_checking ("type_of_params"))

and

(* ruleAssignInstr: ast -> ast -> TenvVar -> TenvType -> position -> instrType
Function determining the type of an assignment instruction
Parameters:
  - assign: abstract syntax tree representing the assignable
  - expr: abstract syntax tree representing the assigned expression
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the assign symbol (in case of error)
Return: the instrType corresponding to the assignment instruction
*)
ruleAssignInstr assign expr envVar envType pos =
  let tassign = (ruleAssignable pos assign envVar envType) in
  let texpr = (type_of_expr expr envVar envType) in
  try
    let be,_ = compare envType tassign texpr in
    if (be) then OK else raise (Wrong_type (pos,texpr,tassign))
  with 
    | Invalid_argument _ -> raise (Wrong_type (pos,texpr,tassign))

and

(* ruleCallFuncWithReturn: ast -> string -> ast -> TenvVar -> TenvType -> position -> position -> instrType
Function determining the type of a function call with a function having return
Parameters:
  - assign: abstract syntax tree representing the assignable
  - namef: name of the called function
  - e: abstract syntax tree representing the expression used as parameter for the function
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the assign symbol (in case of error)
  - posf: position of the function name (in case of error)
Return: the instrType corresponding to the function call
*)
ruleCallFuncWithReturn a namef e envVar envType pos posf =
  let tf,pnode = type_of_func namef envVar envType posf in
  if (tf != VoidType) then
    let ta = ruleAssignable pos a envVar envType in
    try
      let br,_ = compare envType ta tf in
      if (br) then
        let tp = type_of_params pnode envType in
        let te = type_of_expr e envVar envType in
        (try
          let b,_ = compare envType tp te in
          if (b) then OK else raise (Not_type_of_the_params (pos,te,tp,namef)) 
        with 
          | Invalid_argument _ -> raise (Not_type_of_the_params (pos,te,tp,namef)))
      else raise (Not_type_of_the_return (pos,ta,tf,namef))
    with
      | Invalid_argument _ -> raise (Not_type_of_the_return (pos,ta,tf,namef))
  else raise (Assignement_of_void (pos,namef))

and

(* ruleCallFuncVoid: string -> ast -> TenvVar -> TenvType -> position -> instrType
Function determining the type of a function call with a function having void return or no return
Parameters:
  - namef: name of the called function
  - e: abstract syntax tree representing the expression used as parameter for the function
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the function name (in case of error)
Return: the instrType corresponding to the function call
*)
ruleCallFuncVoid namef e envVar envType pos =
  let tf,pnode = type_of_func namef envVar envType pos in
  let br,_ = compare envType tf VoidType in
    if (br) then
      let tp = type_of_params pnode envType in
      let te = type_of_expr e envVar envType in
      try
        let b,_ = compare envType tp te in
        if (b) then OK else raise (Not_type_of_the_params (pos,te,tp,namef))
      with
        | Invalid_argument _ -> raise (No_assignement_of_the_return (pos,tf,namef))
    else raise (No_assignement_of_the_return (pos,tf,namef))

and

(* ruleReceive: ast -> string -> TenvVar -> TenvType -> position -> instrType
Function determining the type of a receive instruction
Parameters:
  - a: abstract syntax tree representing the assignable
  - namechan: the name of the channel on which to send  
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the assign symbol (in case of error)
Return: the instrType corresponding to the receive instruction
*)
ruleReceive a namechan envVar envType pos = 
  let ta = ruleAssignable pos a envVar envType in
  let tchan,_ = type_of_var namechan envVar envType pos in
  (match tchan with
    | ChannelType(st) -> (try 
                          let b,_ = compare envType (ChannelType(ta)) tchan in 
                          if (b) then OK else raise (Wrong_type_chan_receive (pos,ta,st))
                        with
                          | Invalid_argument _ -> raise (Wrong_type_chan_receive (pos,ta,st))) 
    | _ -> raise (Wrong_type (pos,tchan,ChannelGenType)))

and

(* ruleSend: string -> ast -> TenvVar -> TenvType -> position -> instrType
Function determining the type of a receive instruction
Parameters:
  - namechan: the name of the channel on which to send
  - e: abstract syntax tree representing the expression to send  
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the send symbol (in case of error)
Return: the instrType corresponding to the send instruction
*)
ruleSend namechan e envVar envType pos = 
  let te = type_of_expr e envVar envType in
  let tchan,_ = type_of_var namechan envVar envType pos in
  (match tchan with
    | ChannelType(st) -> (try 
                          let b,_ = compare envType (ChannelType(te)) tchan in 
                          if (b) then OK else raise (Wrong_type_chan_send (pos,te,st))
                        with
                          | Invalid_argument _ -> raise (Wrong_type_chan_send (pos,te,st))) 
    | _ -> raise (Wrong_type (pos,tchan,ChannelGenType)))

and

(* ruleIfThenElseInstr: ast -> ast -> ast -> TenvVar -> TenvType -> position -> typeType
Function determining the type of a conditional instruction
Parameters:
  - cond_expr: abstract syntax tree representing the expression inside the condition
  - then_expr: abstract syntax tree representing the instruction inside the then branch
  - else_expr: abstract syntax tree representing the instruction inside the else branch
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the if symbol (in case of error)
Return: the instrType of the conditional instruction
*)
ruleIfThenElseInstr cond_expr then_instr else_instr envVar envType pos =
  let tcond_expr = (type_of_expr cond_expr envVar envType) in
  let tthen_instr = (ruleInstr then_instr envVar envType) in
  let telse_instr = (ruleInstr else_instr envVar envType) in
  try
    let be,_ = compare envType tcond_expr BooleanType in
    if (be) then unify_instr tthen_instr telse_instr envType
    else raise (Wrong_type (pos,tcond_expr,BooleanType))
  with
    | Impossible_unify_instr (t1,t2) -> raise (Different_type_of_return_if (pos,t1,t2))
    | Invalid_argument _ -> raise (Illegal_type_argument pos)

and

(* ruleWhile: ast -> ast -> TenvVar -> TenvType -> position -> typeType
Function determining the type of a conditional instruction
Parameters:
  - cond_expr: abstract syntax tree representing the expression inside the condition
  - i: abstract syntax tree representing the instruction inside the body
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the while symbol (in case of error)
Return: the instrType of the while instruction
*)
ruleWhile cond_expr i envVar envType pos =
  let tcond = (type_of_expr cond_expr envVar envType) in
  let ti = (ruleInstr i envVar envType) in
  try
    let be,_ = compare envType tcond BooleanType in 
      if (be) then ti
      else raise (Wrong_type (pos,tcond,BooleanType))
  with
    | Invalid_argument _ -> raise (Illegal_type_argument pos)

and

(* rulePrefix: ast -> TenvVar -> TenvType -> typeType
Function determining the type of a prefix
Parameters:
  - p: abstract syntax tree representing the prefix
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
Return: the instrType of the prefix
*)
rulePrefix p envVar envType = 
    match p with 
      | PrefixNode(_,_,Tau,_) -> OK
      | PrefixNode(pos,Some(e),Send,Some(namechan)) -> ruleSend namechan e envVar envType pos
      | PrefixNode(pos,Some(a),Receive,Some(namechan)) -> ruleReceive a namechan envVar envType pos
      | _ -> raise (Unknown_error_type_checking ("rulePrefix"))

and

(* rulePrefix: ast -> TenvVar -> TenvType -> typeType
Function determining the type of a choose instruction
Parameters:
  - c: abstract syntax tree representing the choices
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
Return: the instrType of the choose instruction
*)
ruleChoose c envVar envType =
  let rec aux choice typeInstr = 
    match choice with 
      | ChoicesNode(_,p,i,None) -> let _ = rulePrefix p envVar envType in unify_instr (ruleInstr i envVar envType) typeInstr envType
      | ChoicesNode(_,p,i,Some(cs)) -> let _ = rulePrefix p envVar envType in aux cs (unify_instr (ruleInstr i envVar envType) typeInstr envType) 
      | _ -> raise (Unknown_error_type_checking ("ruleChoose"))
  in aux c OK

and

(* ruleSpawn: string -> ast -> TenvVar -> TenvType -> position -> instrType
Function determining the type of a spawn instruction
Parameters:
  - namef: name of the function to use in the new thread
  - e: abstract syntax tree representing the expression used as parameter for the function
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the spawn symbol (in case of error)
Return: the instrType corresponding to the spawn instruction
*)
ruleSpawn namef e envVar envType pos = 
  let _,pnode = type_of_func namef envVar envType pos in
  let tp = type_of_params pnode envType in
  let te = type_of_expr e envVar envType in
  let b,_ = compare envType tp te in
  if (b) then OK else raise (Not_type_of_the_params (pos,te,tp,namef))

and

(* ruleNewChan: string -> ast -> TenvVar -> TenvType -> position -> instrType
Function determining the type of a newChan instruction
Parameters:
  - a: abstract syntax tree representing the assignable receiving the new channel
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - pos: position of the spawn symbol (in case of error)
Return: the instrType corresponding to the newChan instruction
*)
ruleNew a envVar envType pos = 
  let ta = ruleAssignable pos a envVar envType in
  try
    let bc,_ = compare envType ta ChannelGenType in
    if (bc) then OK else raise (Wrong_type (pos,ta,ChannelGenType))
  with
    | Invalid_argument _ -> raise (Wrong_type (pos,ta,ChannelGenType))

and

(* ruleReturnVoid: instrType
Function determining the type of a void return
Return: the instrType corresponding to a return void
*)
ruleReturnVoid = OKt(VoidType)

and

((* ruleReturnExpr: ast -> TenvVar -> TenvType -> instrType
Function determining the type of a newChan instruction
Parameters:
  - e: abstract syntax tree representing the expression to return
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
Return: the instrType corresponding to the return of the expression
*)
ruleReturnExpr e envVar envType = OKt(type_of_expr e envVar envType)

(* extend_envVar_with_params: ast -> TenvVar -> TenvVar
Function extending the variable environment with the declared parameters of a function
Parameters:
  - params: abstract syntax tree representing the parameters of a function
  - envVar: list of variable/function declarations 
Return: the envVar passed in argument extended with the parameters
Insight: The List.filter is used to remove the variable of the same name if it exists,
this is useful in the case of shadowing
*)
let rec extend_envVar_with_params params envVar =
    match params with
      | ParamsNode (pos,t,name,None) -> (pos,t,name,None,false)::(List.filter (fun (_,_,namevar,_,_) -> not (String.equal name namevar)) envVar)
      | ParamsNode (pos,t,name,Some(p)) -> extend_envVar_with_params p ((pos,t,name,None,false)::(List.filter (fun (_,_,namevar,_,_) -> not (String.equal name namevar)) envVar))
      | _ -> raise (Unknown_error_type_checking ("extend_params"))

(* extend_envVar_with_local_declas: ast -> TenvVar -> string list -> TenvVar
Function checking the well-formedness of the local declarations inside a function and extending
the variable environment with those declarations  
Parameters:
  - declas: abstract syntax tree representing the local declarations of a function
  - envVar: list of variable/function declarations
  - nameListType: list of names of declared types 
Return: the envVar passed in argument extended with the local declarations
*)
let extend_envVar_with_local_declas declas envVar nameListType =
  (* Creation of a list containing the local delcaration *)
  let addedDecla = (let rec aux d l = 
    match d with
      | VariableDeclasNode (VariableDeclaNode (pos,t,name),None) -> ((pos,t,name,None,false)::l)
      | VariableDeclasNode (VariableDeclaNode (pos,t,name),Some(vs)) -> aux vs ((pos,t,name,None,false)::l)
      | _ -> raise (Unknown_error_type_checking ("extend_ldecla"))
    in aux declas []) in
  (* Checking of the well-formedness of those declarations *)
  let _ = well_formed_envVar addedDecla nameListType in
  (* Removing of the variable already declared with a name found in the local declarations *)
  let restricted_envVar = List.filter (fun (_,_,name,_,_) -> not (List.exists (fun (_,_,lname,_,_) -> String.equal name lname) addedDecla)) envVar in
  (* Concatenation of the declarations *)
  restricted_envVar @ addedDecla

(* extend_envVar: ast -> ast -> TenvVar -> string list -> TenvVar
Function extending the variable environment with the parameters and local
declarations of a function  
Parameters:
  - params: abstract syntax tree representing the parameters of a function
  - b: abstract syntax tree representing the body of a function
  - envVar: list of variable/function declarations
  - nameListType: list of names of declared types 
Return: the envVar passed in argument extended with the local declarations and parameters
*)
let extend_envVar params b envVar nameListType = 
  let copy_envVar = envVar in
  let extended_envVar = extend_envVar_with_params params copy_envVar in
  match b with
    | BodyNode (_,None,_) -> extended_envVar
    | BodyNode (_,Some(declas),_) -> extend_envVar_with_local_declas declas extended_envVar nameListType
    | _ -> raise (Unknown_error_type_checking ("extend_envVar")) 

(* ruleFunction: ast -> TenvVar -> TenvType -> string list -> bool
Function type checking the body of a function thanks to a local variable environment
Parameters:
  - f: abstract syntax tree representing a function
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - nameListType: list of names of declared types 
Return: true if the function body is well-typed, false if not
*)
let ruleFunction f envVar envType nameListType =
  (match f with
    | FunctionNode (pos,ftnode,namef,params,b) ->
        (* Creation of the local environment *)
        let local_envVar = extend_envVar params b envVar nameListType in
        (* Determining of the type of the function body *)
        let ti = (match b with
          | BodyNode (posb,_,i) ->
            (try
              ruleInstr i local_envVar envType
            with
              | Impossible_unify_instr (_,_) -> raise (Different_type_of_return_func (posb,namef))) 
          | _ -> raise (Unknown_error_type_checking ("ruleFunction"))) in
        (* Determining the declared return type of the function *)
        let ft = type_of_tree ftnode envType in
        (* Comparing the real type of return and the declared type *)
        (match ti with
          | OK -> (try
                    if (fst(compare envType ft VoidType)) then true 
                    else raise (Return_not_match_with_decla (pos,VoidType,ft,namef))
                  with
                    | Invalid_argument _ -> raise (Return_not_match_with_decla (pos,VoidType,ft,namef)))
          | OKt(rt) -> (try
                         if (fst(compare envType ft rt)) then true 
                         else raise (Return_not_match_with_decla (pos,rt,ft,namef))
                       with
                         | Invalid_argument _ -> raise (Return_not_match_with_decla (pos,rt,ft,namef))))
    | _ -> raise (Unknown_error_type_checking ("ruleFunction")))

(* type_check_prg: TenvVar -> TenvType -> string list -> ast -> bool
Function type checking a program
Parameters:
  - envVar: list of the variable/function declarations 
  - envType: list of the type declarations
  - nameListType: list of names of declared types
  - prg: abstract syntax tree representing a program 
Return: true if the program is well-typed, false if not
*)
let type_check_prg envVar envType nameListType prg = 
  (* Creation of list containing all the functions of the file*)
  let all_func = List.rev (let rec aux l tree =
    match tree with
        (* Broswing through the abstract syntax tree *)
        | ProgramNode (p1,p2) -> aux (aux l p1) p2
        (* Adding of the function to the list *)
        | FunctionNode (_,_,_,_,_) -> (tree::l)
        (* Type checking of a global variable (except channels) *)
        | GlobalVarDeclaNode(pos,t,_,vnode) -> 
          let tdecla = type_of_tree t envType in
          let tval = ruleValue vnode envType in
          let b,_ = compare envType tdecla tval in
          if (b) then l else raise (Wrong_type (pos,tval,tdecla))
        (* Type checking of a global channel variable *)
        | GlobalChanDeclaNode(pos,t,_) ->
          let tdecla = type_of_tree t envType in
          let b,_ = compare envType tdecla ChannelGenType in
          if (b) then l else raise (Wrong_type (pos,tdecla,ChannelGenType))
        (* Type checking of the main call *)   
        | CallNode(pos,namef,expr) ->   
          (let _,pnode = type_of_func namef envVar envType pos in
          let tp = type_of_params pnode envType in
          let te = type_of_expr expr envVar envType in
          try
            let b,_ = compare envType tp te in
            if (b) then l else raise (Not_type_of_the_params (pos,te,tp,namef))
          with
            | Invalid_argument _ -> raise (Not_type_of_the_params (pos,te,tp,namef)))
        | _ -> l 
    in aux [] prg) in
  (* Type checking of all the functions *) 
  List.for_all (fun f -> ruleFunction f envVar envType nameListType) all_func