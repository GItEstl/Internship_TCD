open Ast
open Resolve

exception Wrong_type of Lexing.positiom * typeType * typeType
exception Inconsistent_types of Lexing.position * typeType * typeType
exception Different_types_in_list

type typeType =
  | IntegerType
  | BooleanType
  | StringType
  | CharType
  | Void
  | ChannelType of typeType
  | ListType of typeType
  | ListGenType
  | TupleType of typeType list
  | TupleGenType
  | NamedType of string
  | ErrorType

type instrType = OK of typeType

let rec type_of_tree tree =
  match tree with
    | TypeNode (_,IntegerT) -> IntegerType
    | TypeNode (_,BooleanT) -> BooleanType
    | TypeNode (_,StringT) -> StringType
    | TypeNode (_,CharT) -> CharType
    | ChanTNode (_,st) -> ChannelType (type_of_tree st) 
    | ListTNode (_,st) -> ListType (type_of_tree st)
    | TupleNode (_,TypeSeqNode(st,None)) -> type_of_tree st
    | TupleTNode (_,tSeq) -> TupleType (
      let rec aux t l = match t with
        | TypeSeqNode (st,None) -> List.rev ((type_of_tree st)::l) 
        | TypeSeqNode (st1, Some(st2)) -> aux st2 ((type_of_tree st1)::l)
        | _ -> raise Unknow_error_in_type_checking
      in aux tSeq [])
    | NamedTypeNode (_,name) -> NamedType (name)         
    | _ -> raise Unknow_error_in_type_checking

let rec compare type1 type2 =
  match (t1,t2) with
    | (IntegerType,IntegerType) -> (true,IntegerType)
    | (BooleanType,BooleanType) -> (true,BooleanType)
    | (StringType,StringType) -> (true,StringType)
    | (CharType,CharType) -> (true,CharType)
    | (ChannelType(st1),ChannelType(st2)) -> if (compare st1 st2) then (true,t2) else (false,ErrorType)
    | (ListType(st1),ListType(st2)) -> if (compare st1 st2) then (true,t2) else (false,ErrorType)
    | (ListType(st),ListGenType) -> (true,ListType(st))
    | (TupleType(st1),TupleType(st2)) -> if (for_all2 (fun a b -> compare a b) st1 st2) then (true,t2) else (false,ErrorType)
    | (TupleType(st),TupleGenType) -> (true,TupleType(st))
    | (NamedType(n1),NamedType(n2)) -> if (String.equal n1 n2) then (true,t2) else (false,ErrorType)
    | _ -> (false,ErrorType)

(* Expression *)

let type_of_expr expr envVar =
  match expr with
    | BinaryNode (pos,left,op,right) -> ruleBinary op left right envVar pos
    | UnaryNode (pos,op,expr)) -> ruleUnary op expr envVarVar pos
    | IfthenelseExprNode (pos,cond,e1,e2) -> ruleIfThenElseExpr cond e1 e2 envVar pos
    | ExprNode (pos,ExprsNode(e,None)) -> type_of_expr e envVar pos
    | ExprNode (pos,e) -> ruleExprs e envVar
    | ValueNode (v) -> ruleValue v envVar
    | AssignNode (pos,a) -> ruleAssignable a envVar pos

(* Rules for expressions *)

let ruleUnary op expr envVar pos = 
  let texpr = (type_of_expr expr envVar) in
  (match op with
  | (NegateInt | Odd | Even)->
    let be,te = compare texpr IntegerType in
    (if (be) then IntegerType else raise (Wrong_type (pos,texpr,IntegerType)))
  | NegateBool ->
    let be,te = compare texpr BooleanType in
    (if (be) then IntegerType else raise (Wrong_type (pos,texpr,IntegerType)))
  | Head ->
    let be,ListType(st) = compare texpr ListGenType in
    (if (be) then st else raise (Wrong_type (pos,texpr,IntegerType))) 
  | Tail ->
    let be,te = compare texpr ListGenType in
    (if (be) then te else raise (Wrong_type (pos,texpr,IntegerType)))
  | Fst ->
    let be,TupleType(st) = compare texpr TupleGenType in
    (if (be) then (List.hd st) else raise (Wrong_type (pos,texpr,IntegerType)))
  | Snd ->
    let be,TupleType(st) = compare texpr TupleGenType in
    (if (be) then (List.nth st 1) else raise (Wrong_type (pos,texpr,IntegerType)))  
  )  

let ruleBinary op left right envVar  pos =
  let tleft = (type_of_expr left envVar) in
  let tright = (type_of_expr right envVar) in
    (match op with
    | (Equal | Different | Lesser | Greater) ->
      let (bl,tl) = compare tleft IntegerType in
      let (br,tr) = compare tright IntegerType in
        (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tl,IntegerType))) 
        else raise (Wrong_type (pos,tl,IntegerType)))
    | (Add | Substract | Multiply | Divide) ->
      let (bl,tl) = compare tleft IntegerType in
      let (br,tr) = compare tright IntegerType in
        (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tl,IntegerType))) 
        else raise (Wrong_type (pos,tl,IntegerType)))
    | (Or | And) ->
      let (bl,tl) = compare tleft BooleanType in
      let (br,tr) = compare tright BooleanType in
        (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tl,IntegerType))) 
        else raise (Wrong_type (pos,tl,IntegerType)))

let ruleIfThenElseExpr cond_expr then_expr else_expr envVar pos =
  let tcond_expr = (type_of_expr cond_expr envVar) in
  let tthen_expr = (type_of_expr then_expr envVar) in
  let telse_expr = (type_of_expr else_expr envVar) in
  let bc,tc = compare tcond_expr BooleanType in
  let be,te = compare tthen_expr telse_expr in
    (if (be) then (if (br) then te else raise (Inconsistent_type (pos,tthen_expr,telse_expr))) 
    else raise (Wrong_type (pos,te,BooleanType)))

let ruleTupleExpr exprs envVar pos =
  TupleType (
  let rec aux exs l =
    match exs with
      | ExprsNode (e,None) -> List.rev ((type_of_expr e envVar)::l)
      | ExprsNode (e1,Some(e2)) -> aux e2 ((type_of_expr e1 envVar)::l)
  in aux exprs [])

let rec ruleValue v =
  match v with
    | IntegerNode (_,_) -> IntegerType
    | CharNode (_,_) -> CharType
    | StringNode (_,_) -> StringType
    | TrueNode -> BooleanType
    | FalseNode -> BooleanType
    | ValueSeqNode (v,None) -> ListType (ruleValue v)
    | ValueSeqNode (v,Some(vs)) -> ListType (
        let rec aux v1 vs1 =
          (match vs1 with
            | ValueSeqNode (v2,None) -> 
              let bv, tv = compare (ruleValue v1 envVar) (ruleValue v2) in
                if (bv) then tv else raise (Different_types_in_list)
            | ValueSeqNode (v2,Some(vs2)) -> 
              let bv,tv = compare (ruleValue v1 envVar) (ruleValue v2) in
              if (bv) then aux v2 vs2 else raise (Different_types_in_list))
        in aux v vs) 
    | _ -> raise Unknow_error_in_type_checking

let ruleAssignable a envVar pos =
  try 
    let _,t,_,_ = List.find (fun (_,_,name,p) -> (String.equal name a) && (p == None)) envVar in
    type_of_tree t
  with
    | Not_found -> raise (Unknown_variable (pos,a))
    
(* Basic Instruction *)

let type_of_binstr binstr envVar =
  match binstr with
    | BinaryNode (pos,a,Assign,e) -> ruleAssignInstr left right envVar pos
    | BinaryNode (pos,a,Assign,CallNode (_,namef,e)) -> ruleCallFuncWithReturn a namef e pos
    | CallNode (pos,namef,e) -> ruleCallFuncVoid namef e pos
    | ReceiveNode (pos,a,namechan) -> ruleReceive a namechan pos
    | SendNode (pos,namechan,e) -> ruleSend namechan a pos
    | IfthenelseInstrNode (pos,cond,i1,i2) -> ruleIfThenElseInstr cond i1 i2 pos
    | WhileNode (pos,e,i) -> ruleWhile e i pos
    | ChooseNode (pos,c) -> ruleChoose c pos
    | SpawnNode (pos,namef,e) -> ruleSpawn namef e pos
    | NewNode (pos,a) -> ruleNew a pos
    | ReturnNode (pos,None) -> ruleReturnVoid pos
    | ReturnNode (pos,Some (e)) -> ruleReturnExpr e pos
    | ReturnNode (pos,Some (CallNode (_,namef,e))) -> ruleReturnFunc namef e pos
