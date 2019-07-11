open Ast
open Resolve

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

type instrType = 
  | OK
  | OKt of typeType

exception Wrong_type of Lexing.position * typeType * typeType
exception Inconsistent_types of Lexing.position * typeType * typeType
exception Different_types_in_list
exception Unknown_variable of Lexing.position * string
exception Unknown_function of Lexing.position * string
exception Not_type_of_the_params of Lexing.position * typeType * typeType * string
exception Not_type_of_the_return of Lexing.position * typeType * typeType * string
exception Assignement_of_void of Lexing.position * string
exception No_assignement_of_the_return of Lexing.position * typeType * string
exception Wrong_type_chan_receive of Lexing.position * typeType * typeType
exception Wrong_type_chan_send of Lexing.position * typeType * typeType
exception Impossible_unify_instr of typeType * typeType
exception Different_type_of_return of Lexing.position * typeType * typeType  

let rec type_of_tree tree =
  match tree with
    | TypeNode (_,IntegerT) -> IntegerType
    | TypeNode (_,BooleanT) -> BooleanType
    | TypeNode (_,StringT) -> StringType
    | TypeNode (_,CharT) -> CharType
    | ChanTNode (_,st) -> ChannelType (type_of_tree st) 
    | ListTNode (_,st) -> ListType (type_of_tree st)
    | TupleTNode (_,TypeSeqNode(st,None)) -> type_of_tree st
    | TupleTNode (_,tSeq) -> TupleType (
      let rec aux t l = match t with
        | TypeSeqNode (st,None) -> List.rev ((type_of_tree st)::l) 
        | TypeSeqNode (st1, Some(st2)) -> aux st2 ((type_of_tree st1)::l)
        | _ -> raise Unknow_error_in_type_checking
      in aux tSeq [])
    | NamedTypeNode (_,name) -> NamedType (name)
    | FuncTNode (None) -> VoidType
    | FuncTNode (Some(t)) -> type_of_tree t         
    | _ -> raise Unknow_error_in_type_checking

let type_of_var namevar envVar pos =
  try 
    let _,tnode,_,_ = List.find (fun (_,_,name,p) -> (String.equal name namevar) && (p == None)) envVar in
    type_of_tree tnode
  with
    | Not_found -> raise (Unknown_variable (pos,namevar))

let type_of_func namef envVar pos =
  try 
    let _,tnode,_,pnode = List.find (fun (_,_,name,p) -> (String.equal name namef) && (p != None)) envVar in
    (type_of_tree tnode,pnode)
  with
    | Not_found -> raise (Unknown_variable (pos,namef))

let rec compare type1 type2 =
  match (type1,type2) with
    | (IntegerType,IntegerType) -> (true,IntegerType)
    | (BooleanType,BooleanType) -> (true,BooleanType)
    | (StringType,StringType) -> (true,StringType)
    | (CharType,CharType) -> (true,CharType)
    | (ChannelType(st1),ChannelType(st2)) -> if (fst(compare st1 st2)) then (true,ChannelType(st2)) else (false,ErrorType)
    | (ListType(st1),ListType(st2)) -> if (fst(compare st1 st2)) then (true,ListType(st2)) else (false,ErrorType)
    | (ListType(st),ListGenType) -> (true,ListType(st))
    | (TupleType(st1),TupleType(st2)) -> if (List.for_all2 (fun a b -> fst(compare a b)) st1 st2) then (true,TupleType(st2)) else (false,ErrorType)
    | (TupleType(st),TupleGenType) -> (true,TupleType(st))
    | (NamedType(n1),NamedType(n2)) -> if (String.equal n1 n2) then (true,NamedType(n2)) else (false,ErrorType)
    | _ -> (false,ErrorType)

let unify_instr ti1 ti2 = 
  match (ti2,ti1) with
    | (OK,t) -> t
    | (t,OK) -> t
    | (OKt(t1),OKt(t2)) -> let b,t = compare t1 t2 in
      if (b) then OKt(t) else raise (Impossible_unify_instr (t1,t2))

(* Expression *)

let rec type_of_expr expr envVar =
  match expr with
    | BinaryNode (pos,left,op,right) -> ruleBinary op left right envVar pos
    | UnaryNode (pos,op,expr) -> ruleUnary op expr envVar pos
    | IfthenelseExprNode (pos,cond,e1,e2) -> ruleIfThenElseExpr cond e1 e2 envVar pos
    | ExprNode (_,ExprsNode(e,None)) -> type_of_expr e envVar
    | ExprNode (_,e) -> ruleTupleExpr e envVar 
    | ValueNode (v) -> ruleValue v
    | AssignNode (pos,a) -> ruleAssignable a envVar pos
    | _ -> raise Unknow_error_in_type_checking

and

ruleUnary op expr envVar pos = 
  let texpr = (type_of_expr expr envVar) in
  (match op with
  | (NegateInt | Odd | Even)->
    let be,_ = compare texpr IntegerType in
    (if (be) then IntegerType else raise (Wrong_type (pos,texpr,IntegerType)))
  | NegateBool ->
    let be,_ = compare texpr BooleanType in
    (if (be) then IntegerType else raise (Wrong_type (pos,texpr,IntegerType)))
  | Head ->
    let _,te = compare texpr ListGenType in
    (match te with
      | ListType(st) -> st
      | _ -> raise (Wrong_type (pos,texpr,ListGenType))) 
  | Tail ->
    let _,te = compare texpr ListGenType in
    (match te with
    | ListType(_) -> te
    | _ -> raise (Wrong_type (pos,texpr,ListGenType))) 
  | Fst ->
    let _,te = compare texpr TupleGenType in
    (match te with
    | TupleType(st) -> List.hd st
    | _ -> raise (Wrong_type (pos,texpr,TupleGenType))) 
  | Snd ->
    let _,te = compare texpr TupleGenType in
    (match te with
    | TupleType(st) -> List.nth st 1
    | _ -> raise (Wrong_type (pos,texpr,TupleGenType))) 
  )
  
and 

ruleBinary op left right envVar  pos =
  let tleft = (type_of_expr left envVar) in
  let tright = (type_of_expr right envVar) in
    (match op with
    | (Equal | Different | Lesser | Greater) ->
      let (bl,tl) = compare tleft IntegerType in
      let (br,tr) = compare tright IntegerType in
        (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tr,IntegerType))) 
        else raise (Wrong_type (pos,tl,IntegerType)))
    | (Add | Substract | Multiply | Divide) ->
      let (bl,tl) = compare tleft IntegerType in
      let (br,tr) = compare tright IntegerType in
        (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tr,IntegerType))) 
        else raise (Wrong_type (pos,tl,IntegerType)))
    | (Or | And) ->
      let (bl,tl) = compare tleft BooleanType in
      let (br,tr) = compare tright BooleanType in
        (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tr,IntegerType))) 
        else raise (Wrong_type (pos,tl,IntegerType)))
    | _ -> raise Unknow_error_in_type_checking)

and

ruleIfThenElseExpr cond_expr then_expr else_expr envVar pos =
  let tcond_expr = (type_of_expr cond_expr envVar) in
  let tthen_expr = (type_of_expr then_expr envVar) in
  let telse_expr = (type_of_expr else_expr envVar) in
  let bc,_ = compare tcond_expr BooleanType in
  let be,te = compare tthen_expr telse_expr in
    (if (bc) then (if (be) then te else raise (Inconsistent_types (pos,tthen_expr,telse_expr))) 
    else raise (Wrong_type (pos,te,BooleanType)))

and

ruleTupleExpr exprs envVar =
  TupleType (
  let rec aux exs l =
    (match exs with
      | ExprsNode (e,None) -> List.rev ((type_of_expr e envVar)::l)
      | ExprsNode (e1,Some(e2)) -> aux e2 ((type_of_expr e1 envVar)::l)
      | _ -> raise Unknow_error_in_type_checking)
  in aux exprs [])

and

ruleValue v =
  match v with
    | IntegerNode (_,_) -> IntegerType
    | CharNode (_,_) -> CharType
    | StringNode (_,_) -> StringType
    | TrueNode(_) -> BooleanType
    | FalseNode(_) -> BooleanType
    | ValueSeqNode (v,None) -> ListType (ruleValue v)
    | ValueSeqNode (v,Some(vs)) -> ListType (
        let rec aux v1 vs1 =
          (match vs1 with
            | ValueSeqNode (v2,None) -> 
              let bv, tv = compare (ruleValue v1) (ruleValue v2) in
                if (bv) then tv else raise (Different_types_in_list)
            | ValueSeqNode (v2,Some(vs2)) -> 
              let bv,_ = compare (ruleValue v1) (ruleValue v2) in
              if (bv) then (aux v2 vs2) else raise (Different_types_in_list)
            | _ -> raise Unknow_error_in_type_checking)
        in aux v vs) 
    | _ -> raise Unknow_error_in_type_checking

and

ruleAssignable a envVar pos = type_of_var a envVar pos

(* Instruction *)

let type_of_instrs i  =
  let rec aux i tInstr =
    match i with
      | InstrSeqNode(NoopNode,None) -> tInstr 
      | InstrSeqNode(NoopNode,Some(i)) -> aux i enVar tInstr
      | InstrSeqNode (bi,None) -> unify bi tInstr
      | InstrSeqNode (bi,Some(i)) -> aux i (unify bi tInstr)
      | _ -> raise Unknow_error_in_type_checking
  in aux i OK 
    
(* Basic Instruction *)

let rec type_of_binstr binstr envVar =
  match binstr with
    | BinaryNode (pos,a,Assign,CallNode (posf,namef,e)) -> ruleCallFuncWithReturn a namef e envVar pos posf
    | BinaryNode (pos,a,Assign,e) -> ruleAssignInstr a e envVar pos
    | CallNode (pos,namef,e) -> ruleCallFuncVoid namef e envVar pos
    | ReceiveNode (pos,a,namechan) -> ruleReceive a namechan envVar pos
    | SendNode (pos,namechan,e) -> ruleSend namechan e envVar pos
    | IfthenelseInstrNode (pos,cond,i1,i2) -> ruleIfThenElseInstr cond i1 i2 envVar pos
    | WhileNode (pos,e,i) -> ruleWhile e i envVar pos
  (*  | ChooseNode (pos,c) -> ruleChoose c envVar pos *)
    | SpawnNode (pos,namef,e) -> ruleSpawn namef e envVar pos
    | NewNode (pos,a) -> ruleNew a envVar pos
    | ReturnNode (_,None) -> ruleReturnVoid
    | ReturnNode (pos,Some (CallNode (_,namef,e))) -> ruleReturnFunc namef e envVar pos
    | ReturnNode (_,Some (e)) -> ruleReturnExpr e envVar
    | _ -> raise Unknow_error_in_type_checking

and

type_of_params paramNode =
  match paramNode with 
    | Some(ParamsNode (_,t,_,None)) -> type_of_tree t
    | Some(ParamsNode (_,_,_,Some(_))) -> TupleType( 
      (let rec aux params l =
        match params with
          | Some(ParamsNode (_,t,_,None)) -> List.rev ((type_of_tree t)::l)
          | Some(ParamsNode (_,t,_,ps)) -> aux ps ((type_of_tree t)::l)
          | _ -> raise Unknow_error_in_type_checking         
      in aux paramNode []))
    | _ -> raise Unknow_error_in_type_checking

and

ruleAssignInstr assign expr envVar pos =
  let tassign = (type_of_expr assign envVar) in
  let texpr = (type_of_expr expr envVar) in
  let be,_ = compare tassign texpr in
  if (be) then OK else raise (Wrong_type (pos,texpr,tassign))

and

ruleCallFuncWithReturn a namef e envVar pos posf =
  let tf,pnode = type_of_func namef envVar posf in
  if (tf != VoidType) then
    let ta = type_of_expr a envVar in
    let br,_ = compare ta tf in
    if (br) then
      let tp = type_of_params pnode in
      let te = type_of_expr e envVar in
      let b,_ = compare tp te in
        if (b) then OK else raise (Not_type_of_the_params (pos,te,tp,namef)) 
    else raise (Not_type_of_the_return (pos,ta,tf,namef))
  else raise (Assignement_of_void (pos,namef))

and

ruleCallFuncVoid namef e envVar pos =
  let tf,pnode = type_of_func namef envVar pos in
  let br,_ = compare tf VoidType in
    if (br) then
      let tp = type_of_params pnode in
      let te = type_of_expr e envVar in
      let b,_ = compare tp te in
      if (b) then OK else raise (Not_type_of_the_params (pos,te,tp,namef)) 
    else raise (No_assignement_of_the_return (pos,tf,namef))

and

ruleReceive a namechan envVar pos = 
  let ta = type_of_expr a envVar in
  let tchan = type_of_var namechan envVar pos in
  match tchan with
    | ChannelType(st) -> let b,_ = compare ta st in 
                          if (b) then OK else raise (Wrong_type_chan_receive (pos,ta,st))
    | _ -> raise Unknow_error_in_type_checking

and

ruleSend namechan e envVar pos = 
  let te = type_of_expr e envVar in
  let tchan = type_of_var namechan envVar pos in
  match tchan with
    | ChannelType(st) -> let b,_ = compare te st in 
                          if (b) then OK else raise (Wrong_type_chan_send (pos,te,st))
    | _ -> raise Unknow_error_in_type_checking

and

ruleIfThenElseInstr cond_expr then_instr else_instr envVar pos =
  let tcond_expr = (type_of_expr cond_expr envVar) in
  let tthen_instr = (type_of_instr then_instr envVar) in
  let telse_instr = (type_of_instr else_instr envVar) in
  let be,te = compare tcond_expr BooleanType in
    if (be) then 
      try
        unify_instr tthen_instr telse_instr pos
      with
        | Impossible_unify_instr (t1,t2) -> raise (Different_type_of_return (pos,t1,t2))
    else raise (Wrong_type (pos,te,BooleanType))

and

ruleWhile cond_expr i envVar pos =
  let tcond = (type_of_expr cond_expr envVar) in
  let ti = (type_of_instr i envVar) in
  let be,te = compare tcond BooleanType in 
    if (be) then ti
    else raise (Wrong_type (pos,te,BooleanType))

and

ruleSpawn namef e envVar pos = ruleCallFuncVoid namef e envVar pos

and

ruleNew a envVar pos = 
  let ta = type_of_expr a envVar in
  let bc,_ = compare ta ChannelGenType in
  if (bc) then OK else raise (Wrong_type (pos,ta,ChannelGenType))

and

ruleReturnVoid = OK

and

ruleReturnExpr e envVar = OKt(type_of_expr e envVar)

and

ruleReturnFunc namef e envVar pos =
  let tf,pnode = type_of_func namef envVar pos in
  let tp = type_of_params pnode in
  let te = type_of_expr e envVar in
  let b,_ = compare tp te in
    if (b) then OKt(tf) else raise (Not_type_of_the_params (pos,te,tp,namef)) 
