open Ast

let envType = ref([(Lexing.dummy_pos,"",NoopNode,false)])

type valueType =
  | IntegerVal of int
  | BooleanVal of bool
  | StringVal of string
  | CharVal of string
  | ListVal of valueType list
  | TupleVal of valueType list
  | FuncVal of ast * ast
  | ErrorVal

exception Unknown_error_reference_interpretor of string

let string_of_val v =
  match v with
    | None -> "unit"
    | Some(vs) ->
      let rec aux v =
        match v with
        | IntegerVal(i) -> string_of_int i 
        | BooleanVal(b) -> string_of_bool b
        | StringVal(s) -> s
        | CharVal(c) -> c
        | ListVal(l) -> "[" ^ (List.fold_left (fun s e ->  s ^ " " ^ (aux e)) (aux (List.nth l 0)) (List.tl l)) ^ "]"
        | TupleVal(t) -> "(" ^ (List.fold_left (fun s e ->  s ^ ", " ^ (aux e)) (aux (List.nth t 0)) (List.tl t)) ^ ")"
        | _ -> "ERROR"
      in aux vs 

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
    | _ -> raise (Unknown_error_reference_interpretor "value_of_expr")

and

ruleUnary op expr state = 
  let v = value_of_expr expr state in
  (match v with
    | IntegerVal(i) -> 
      (match op with
        | NegateInt -> IntegerVal(- i)
        | Odd -> if ((i mod 2) == 1) then BooleanVal(true) else BooleanVal(false)
        | Even -> if ((i mod 2) == 0) then BooleanVal(true) else BooleanVal(false)
        | _ -> raise (Unknown_error_reference_interpretor "ruleUnary1")
      ) 
    | BooleanVal(b) ->
      (match op with
        | NegateBool -> BooleanVal((not b))
        | _ -> raise (Unknown_error_reference_interpretor "ruleUnary2")
      ) 
    | ListVal(t::q) -> 
      (match op with
        | Head -> t
        | Tail -> ListVal(q)
        | _ -> raise (Unknown_error_reference_interpretor "ruleUnary3")
      )
    | TupleVal(l) -> 
      (match op with
        | Fst -> List.nth l 0
        | Snd -> List.nth l 1 
        | _ -> raise (Unknown_error_reference_interpretor "ruleUnary4")
      )
    | _ -> raise (Unknown_error_reference_interpretor "ruleUnary")
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
        | _ -> raise (Unknown_error_reference_interpretor "ruleBinary1")
      )
    | (BooleanVal(bl),BooleanVal(br)) ->
      (match op with
        | Equal -> BooleanVal(bl == br)
        | Different -> BooleanVal(bl != br)
        | _ -> raise (Unknown_error_reference_interpretor "ruleBinary2")
      ) 
    | (StringVal(sl),StringVal(sr)) -> 
      (match op with
        | Equal -> BooleanVal(String.equal sl sr)
        | Different -> BooleanVal(not (String.equal sl sr))
        | _ -> raise (Unknown_error_reference_interpretor "ruleBinary3")
      )
    | (IntegerVal(i),TupleVal(t)) -> List.nth t i 
    | _ -> raise (Unknown_error_reference_interpretor "ruleBinary")
  )

and 

ruleIfThenElseExpr cond_expr then_expr else_expr state =
  let vcond = value_of_expr cond_expr state in
  match vcond with
    | BooleanVal(true) -> value_of_expr then_expr state
    | BooleanVal(false) -> value_of_expr else_expr state
    | _ -> raise (Unknown_error_reference_interpretor "ruleIfThenElseExpr")
    
and

ruleTupleExpr exprs state =
  TupleVal (
  let rec aux exs l =
    match exs with
      | ExprsNode (e,None) -> List.rev ((value_of_expr e state)::l)
      | ExprsNode (e1,Some(e2)) -> aux e2 ((value_of_expr e1 state)::l)
      | _ -> raise (Unknown_error_reference_interpretor "ruleTupleExpr")
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
            | _ -> raise (Unknown_error_reference_interpretor "ruleValue2")
          ) in aux v []
      )
    | _ -> raise (Unknown_error_reference_interpretor "ruleValue")
and

ruleIdentifier a state = !(Hashtbl.find state a)

let rec def_value t =
  match t with
    | TypeNode (_,IntegerT) -> IntegerVal(0)
    | TypeNode (_,BooleanT) -> BooleanVal(true)
    | TypeNode (_,StringT) -> StringVal("")
    | TypeNode (_,CharT) -> CharVal("")
    | ListTNode (_,_) -> ListVal([])
    | TupleTNode (_,TypeSeqNode(st,None)) -> def_value st
    | TupleTNode (_,tSeq) -> TupleVal (
      let rec aux t l = match t with
        | TypeSeqNode (st,None) -> List.rev ((def_value st)::l) 
        | TypeSeqNode (st1, Some(st2)) -> aux st2 ((def_value st1)::l)
        | _ -> raise (Unknown_error_reference_interpretor "def_value2")
      in aux tSeq [])
    | NamedTypeNode (_,st) -> let _,_,t,_ = (List.find (fun (_,name,_,_) -> String.equal name st) !envType) in (def_value t)
    | _ -> raise (Unknown_error_reference_interpretor "def_value")

let extend_state_with_params paramsnode state vparams =
  let vp = (match vparams with 
    | TupleVal(l) -> l
    | e -> [e]
  ) in
  let rec aux pnode vpa =
    match (pnode,vpa) with
      | (ParamsNode (_,_,name,None), [e])-> Hashtbl.add state name (ref e)
      | (ParamsNode (_,_,name,Some(p)), t::q) -> Hashtbl.add state name (ref t); aux p q
      | _ -> raise (Unknown_error_reference_interpretor "extend_state_with_params2")
  in aux paramsnode vp

let extend_state_with_local_declas body state =
  match body with
    | BodyNode (_,None,_) -> ()
    | BodyNode (_,Some(d),_) -> 
      let rec aux d =
        (match d with
          | VariableDeclasNode (VariableDeclaNode (_,t,name),None) -> Hashtbl.add state name (ref(def_value t)) 
          | VariableDeclasNode (VariableDeclaNode (_,t,name),Some(vs)) -> Hashtbl.add state name (ref(def_value t)); aux vs
          | _ -> raise (Unknown_error_reference_interpretor "extend_state_with_local_declas2")
        )
      in aux d
    | _ -> raise (Unknown_error_reference_interpretor "extend_state_with_local_declas")

let create_local_state params body state vparams =
  let local = Hashtbl.copy state in
  extend_state_with_params params local vparams;
  extend_state_with_local_declas body local;
  local

let rec ruleInstr i state =
  let rec aux i v =
    match v with
      | None ->
        (match i with
          | InstrSeqNode(NoopNode,None) -> v 
          | InstrSeqNode(NoopNode,Some(i)) -> aux i v
          | InstrSeqNode (bi,None) -> value_of_binstr bi state
          | InstrSeqNode (bi,Some(i)) -> aux i (value_of_binstr bi state)
          | _ -> raise (Unknown_error_reference_interpretor "ruleInstr2")
        )
      | Some(_) -> v
  in aux i None 
    
and

value_of_binstr binstr state =
  match binstr with
    | BinaryNode (_,a,Assign,CallNode (_,namef,e)) -> ruleCallFuncWithReturn a namef e state
    | BinaryNode (_,a,Assign,e) -> ruleAssignInstr a e state
    | CallNode (_,namef,e) -> ruleCallFuncVoid namef e state
    | IfthenelseInstrNode (_,cond,i1,i2) -> ruleIfThenElseInstr cond i1 i2 state
    | WhileNode (_,e,i) -> ruleWhile e i state
    | ReturnNode (_,None) -> ruleReturnVoid
    | ReturnNode (_,Some (CallNode (_,namef,e))) -> ruleReturnFunc namef e state
    | ReturnNode (_,Some (e)) -> ruleReturnExpr e state
    | _ -> raise (Unknown_error_reference_interpretor "value_of_binstr")

and

ruleAssignInstr a e state =
  match a with 
    | AssignNode (_,name) ->
      let ve = value_of_expr e state in
      let pa = Hashtbl.find state name in
      pa := ve;
      None
    | _ -> raise (Unknown_error_reference_interpretor "ruleAssignInstr") 

and

ruleCallFuncWithReturn a namef e state = 
  match (!(Hashtbl.find state namef),a) with 
    | (FuncVal(p,b),AssignNode(_,name)) ->
      let ve = value_of_expr e state in
      let localstate = create_local_state p b state ve in
      (match b with 
      | BodyNode (_,_,instr) -> 
          (match (ruleInstr instr localstate) with
            | Some(vf) ->
              let pa = Hashtbl.find state name in
              pa := vf;
              None
            | _ -> raise (Unknown_error_reference_interpretor "ruleCallFuncWithReturn3"))
      | _ -> raise (Unknown_error_reference_interpretor "ruleCallFuncWithReturn2"))
    | _ -> raise (Unknown_error_reference_interpretor "ruleCallFuncWithReturn") 
    

and

ruleCallFuncVoid namef e state = 
  match (!(Hashtbl.find state namef)) with 
    | FuncVal(p,b) ->
      let ve = value_of_expr e state in
      let localstate = create_local_state p b state ve in
      (match b with
        | BodyNode (_,_,instr) ->
          let _ = ruleInstr instr localstate in
          None
        | _ -> raise (Unknown_error_reference_interpretor "ruleCallFuncVoid2"))
    | _ -> raise (Unknown_error_reference_interpretor "ruleCallFuncVoid") 

and

ruleIfThenElseInstr cond i1 i2 state =
  let vcond = value_of_expr cond state in
  match vcond with
  | BooleanVal(true) -> ruleInstr i1 state
  | BooleanVal(false) -> ruleInstr i2 state
  | _ -> raise (Unknown_error_reference_interpretor "ruleIfThenElseInstr")

and 

ruleWhile cond i state =
  let vcond = value_of_expr cond state in
  match vcond with
  | BooleanVal(true) -> let v = ruleInstr i state in
              (match v with 
                | None -> ruleWhile cond i state
                | Some(_) -> v)
  | BooleanVal(false) -> None
  | _ -> raise (Unknown_error_reference_interpretor "ruleWhile")

and

ruleReturnVoid = None

and

ruleReturnFunc namef e state =
  match (!(Hashtbl.find state namef)) with
    | FuncVal(p,b) ->
      let ve = value_of_expr e state in
      let localstate = create_local_state p b state ve in
      (match b with
        | BodyNode (_,_,instr) -> ruleInstr instr localstate
        | _ -> raise (Unknown_error_reference_interpretor "ruleReturnFunc2"))
    | _ -> raise (Unknown_error_reference_interpretor "ruleReturnFunc")

and

ruleReturnExpr e state = Some(value_of_expr e state)

let execution_prg prg start env =
  envType := env;
  let state = Hashtbl.create 100 in 
  let rec aux tree =
    match tree with
        | ProgramNode (p1,p2) -> aux p1; aux p2
        | FunctionNode (_,_,n,p,b) -> Hashtbl.add state n (ref (FuncVal(p,b)))
        | VariableDeclaNode(_,t,n) -> Hashtbl.add state n (ref (def_value t))
        | _ -> ()
    in aux prg;
    match start with
      | Some(_,name,expr) ->
        (match (!(Hashtbl.find state name)) with 
        | FuncVal(p,b) ->
          let ve = value_of_expr expr state in
          let localstate = create_local_state p b state ve in
          (match b with 
          | BodyNode (_,_,instr) -> string_of_val (ruleInstr instr localstate)
          | _ -> raise (Unknown_error_reference_interpretor "execution_prg3"))
        | _ -> raise (Unknown_error_reference_interpretor "execution_prg2")) 
      | _ -> raise (Unknown_error_reference_interpretor "execution_prg")