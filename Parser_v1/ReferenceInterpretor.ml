open Ast

let sizelocaltbl = ref (100)

type valueType =
  | IntegerVal of int ref
  | BooleanVal of bool ref
  | StringVal of string ref
  | CharVal of string ref
  | ListVal of (valueType list) ref
  | TupleVal of (valueType list) ref
  | FuncVal of string * ast * ast
  | ErrorVal
(*
let value_of_var a state = BooleanVal(true)

let rec value_of_expr expr state =
  match expr with
    | BinaryNode (_,left,op,right) -> ruleBinary op left right state
    | UnaryNode (_,op,expr) -> ruleUnary op expr state
    | IfthenelseExprNode (_,cond,e1,e2) -> ruleIfThenElseExpr cond e1 e2 state
    | ExprNode (_,e) -> value_of_expr e state
    | ExprsNode (e,None) -> value_of_expr e state
    | ExprsNode (e1,Some(e2)) -> ruleTupleExpr (ExprsNode (e1,Some(e2))) state
    | ValueNode(ValueNode (v)) -> ruleValue v state
    | AssignNode (_,a) -> ruleIdentifier a state

and
*)
let ruleUnary op expr state v = 
  (*let v = value_of_expr expr state in*)
  (match v with
    | IntegerVal(i) -> 
      (match op with
        | NegateInt -> IntegerVal(ref (- !i))
        | Odd -> if ((!i mod 2) == 1) then BooleanVal(ref true) else BooleanVal(ref false)
        | Even -> if ((!i mod 2) == 0) then BooleanVal(ref true) else BooleanVal(ref false)
      ) 
    | BooleanVal(b) ->
      (match op with
        | NegateBool -> BooleanVal(ref(not !b))
      ) ) (*
    | ListVal(!(t::q)) -> 
      (match op with
        | Head -> ref(t)
        | Tail -> ref(ListVal(q))
      )
    | TupleVal(l) -> 
      (match op with
        | Fst -> ref (List.nth !l 0)
        | Snd -> ref (List.nth !l 1)  
      ) 
  )

and

ruleBinary op lexpr rexpr state = 
  let vl = value_of_expr lexpr state in
  let vr = value_of_expr rexpr state in  
  (match (vl,vr) with
    | (IntegerVal(il),IntegerVal(ir)) -> 
      (match op with
        | Equal -> BooleanVal(il == ir)
        | Different -> BooleanVal(il != ir)
        | Lesser -> BooleanVal(il < ir)
        | Greater -> BooleanVal(il > ir)
        | Add -> IntegerVal(il + ir)
        | Substract -> IntegerVal(il - ir)
        | Multiply -> IntegerVal(il * ir)
        | Divide -> IntegerVal(il / ir)
      )
    | (BooleanVal(bl),BooleanVal(br)) ->
      (match op with
        | Equal -> BooleanVal(bl == br)
        | Different -> BooleanVal(bl != br)
      ) 
    | (StringVal(sl),StringVal(sr)) -> 
      (match op with
        | Equal -> BooleanVal(String.equal sl sr)
        | Different -> BooleanVal(not (String.equal sl sr))
      )
  )

and

ruleIfThenElseExpr cond_expr then_expr else_expr state =
  let vcond = value_of_expr cond_expr state in
  match vcond with
    | BooleanVal(true) -> value_of_expr then_expr state
    | BooleanVal(false) -> value_of_expr else_expr state
    
and

ruleTupleExpr exprs state =
  TupleVal (
  let rec aux exs l =
    match exs with
      | ExprsNode (e,None) -> List.rev ((value_of_expr e state)::l)
      | ExprsNode (e1,Some(e2)) -> aux e2 ((value_of_expr e1 state)::l)
  in aux exprs [])

and

ruleValue v state =
  match v with
    | IntegerNode (_,i) -> IntegerVal(i)
    | CharNode (_,c) -> CharVal(c)
    | StringNode (_,s) -> StringVal(s)
    | TrueNode(_) -> BooleanVal(true)
    | FalseNode(_) -> BooleanVal(false)
    | ValueSeqNode (_,_) -> ListVal (
        let rec aux vs l =
          (match vs with
            | ValueSeqNode (ValueNode(val1),None) -> List.rev ((ruleValue val1 state)::l)
            | ValueSeqNode (ValueNode(val1),Some(val2)) -> aux val2 ((ruleValue val1 state)::l)
          ) in aux v []
      )

and

ruleIdentifier a state = value_of_var a state


let rec def_value t =
  match tree with
    | TypeNode (_,IntegerT) -> IntegerVal(0)
    | TypeNode (_,BooleanT) -> BooleanVal(true)
    | TypeNode (_,StringT) -> StringVal("")
    | TypeNode (_,CharT) -> CharVal("")
    | ListTNode (_,st) -> ListVal([])
    | TupleTNode (_,TypeSeqNode(st,None)) -> def_value st
    | TupleTNode (_,tSeq) -> TupleType (
      let rec aux t l = match t with
        | TypeSeqNode (st,None) -> List.rev ((def_value st)::l) 
        | TypeSeqNode (st1, Some(st2)) -> aux st2 ((def_value st1)::l)
      in aux tSeq [])
    | NamedTypeNode (_,st) -> let _,_,t,_ = (List.find (fun (_,name,_,_) -> String.equal name st) envType) in (def_value t)
   

let extend_state_with_params paramsnode state =
  let rec aux pnode =
    match pnode with
      | ParamsNode (pos,t,name,None) -> Hashtbl.add state name (def_value t)
      | ParamsNode (pos,t,name,Some(p)) -> Hashtbl.add state name (def_value t); aux p
  in aux paramsnode state

let extend_state_with_local_declas body state =
  match body with
    | BodyNode (_,None,_) -> unit
    | BodyNode (_,Some(v),_) -> 
      let rec aux d =
        (match d with
          | VariableDeclasNode (VariableDeclaNode (_,t,name),None) -> Hashtbl.add state name (def_value t) 
          | VariableDeclasNode (VariableDeclaNode (_,t,name),Some(vs)) -> Hashtbl.add state name (def_value t); aux vs
        )
      in aux d

let create_local_state params body =
  let local = Hashtbl.create (!sizelocaltbl) in
  extend_state_with_params params local;
  extend_state_with_local_declas body local;
  local

let rec ruleInstr i state =
  let rec aux i (v,s) =
    match (v,s) with
      | (None,s) ->
        (match i with
          | InstrSeqNode(NoopNode,None) -> v 
          | InstrSeqNode(NoopNode,Some(i)) -> aux i (v,s)
          | InstrSeqNode (bi,None) -> value_of_binstr binstr state
          | InstrSeqNode (bi,Some(i)) -> aux i (value_of_binstr binstr state)
        )
      | (Some(val),s) -> (v,s)
  in aux i (None,state) 
    
and

value_of_binstr binstr state =
  match binstr with
    | BinaryNode (pos,a,Assign,e) -> ruleAssignInstr a e state
    | BinaryNode (pos,a,Assign,CallNode (posf,namef,e)) -> ruleCallFuncWithReturn a namef e state
    | CallNode (pos,namef,e) -> ruleCallFuncVoid namef e state
    | IfthenelseInstrNode (pos,cond,i1,i2) -> ruleIfThenElseInstr cond i1 i2 state
    | WhileNode (pos,e,i) -> ruleWhile e i state
    | ReturnNode (_,None) -> ruleReturnVoid state
    | ReturnNode (pos,Some (CallNode (_,namef,e))) -> ruleReturnFunc namef e state
    | ReturnNode (_,Some (e)) -> ruleReturnExpr e state

and

ruleAssignInstr a e state =
  let ve = value_of_expr e state in
  Hashtbl.replace state a ve;
  None

and

ruleCallFuncWithReturn a namef e state = 
  let FuncVal(n,p,b) = Hashtbl.find state namef in
  let localstate = create_local_state p b in
  let ve = value_of_expr e state in
  let BodyNode (_,_,instr) = b in
  let vf = 
  
*)