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
exception Run_time_error of string


(* string_of_val: valueType option -> string
Function converting a value into a string
Parameters:
  - v: the value to convert
Return: string representing the value
*)
let string_of_val v =
  match v with
    | None -> "unit"
    | Some(vs) ->
      let rec aux v =
        match v with
        | IntegerVal(i) -> string_of_int i 
        | BooleanVal(b) -> string_of_bool b
        | StringVal(s) -> "\"" ^ s ^ "\""
        | CharVal(c) -> "\'" ^ c ^ "\'"
        | ListVal([]) -> "[]"
        | ListVal(l) -> "[" ^ (List.fold_left (fun s e ->  s ^ ", " ^ (aux e)) (aux (List.nth l 0)) (List.tl l)) ^ "]"
        | TupleVal(t) -> "(" ^ (List.fold_left (fun s e ->  s ^ ", " ^ (aux e)) (aux (List.nth t 0)) (List.tl t)) ^ ")"
        | _ -> "ERROR"
      in aux vs 


(* value_of_expr: (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an expression
Parameters:
  - expr: abstract syntax tree representing the expression
  - state: hash table representing the state of all the variables  
Return: a string corresponding to the expression
*)
let rec value_of_expr expr state =
  match expr with
    | BinaryNode (_,left,op,right) -> ruleBinary op left right state
    | UnaryNode (_,op,expr) -> ruleUnary op expr state
    | IfthenelseExprNode (_,cond,e1,e2) -> ruleIfThenElseExpr cond e1 e2 state
    | ExprNode (_,e) -> value_of_expr e state
    | ExprsNode (e,None) -> value_of_expr e state
    | ExprsNode (e1,Some(e2)) -> ruleTupleExpr (ExprsNode (e1,Some(e2))) state
    | ValueNode (v) -> ruleValue v
    | AssignNode (_,a) -> ruleIdentifier a state
    | _ -> raise (Unknown_error_reference_interpretor "value_of_expr")

and


(* ruleUnary: unary -> ast -> (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an unary operation
Parameters:
  - op: unary operator
  - expr: abstract syntax tree representing the expression
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the unary operation
*)
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
    | ListVal([]) -> 
      raise (Run_time_error "You cannot apply an operator to an empty list") 
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


(* ruleBinary: binary -> ast -> ast -> (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an binary operation
Parameters:
  - op: unary operator
  - lexpr: abstract syntax tree representing the left expression
  _ rexpr: abstract syntax tree representing the right expression
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the binary operation
*)
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
        | Or -> BooleanVal(bl || br)
        | And -> BooleanVal(bl && br)
        | _ -> raise (Unknown_error_reference_interpretor "ruleBinary2")
      ) 
    | ((StringVal(sl),StringVal(sr)) | (CharVal(sl),CharVal(sr)))-> 
      (match op with
        | Equal -> BooleanVal(String.equal sl sr)
        | Different -> BooleanVal(not (String.equal sl sr))
        | _ -> raise (Unknown_error_reference_interpretor "ruleBinary3")
      )
    | (IntegerVal(i),TupleVal(t)) -> List.nth t i 
    | _ -> raise (Unknown_error_reference_interpretor "ruleBinary")
  )

and 


(* ruleIfThenElseExpr: ast -> ast -> ast -> (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an binary operation
Parameters:
  - cond_expr: abstract syntax tree representing the expression for the condition
  - then_expr: abstract syntax tree representing the expression in the then branch
  - else_expr: abstract syntax tree representing the expression in the else branch
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the conditional expression
*)
ruleIfThenElseExpr cond_expr then_expr else_expr state =
  let vcond = value_of_expr cond_expr state in
  match vcond with
    | BooleanVal(true) -> value_of_expr then_expr state
    | BooleanVal(false) -> value_of_expr else_expr state
    | _ -> raise (Unknown_error_reference_interpretor "ruleIfThenElseExpr")
    
and


(* ruleTupleExpr: ast -> (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an expression tuple
Parameters:
  - exprs: abstract syntax tree representing the tuple of expressions
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the expression tuple
*)
ruleTupleExpr exprs state =
  TupleVal (
  let rec aux exs l =
    match exs with
      | ExprsNode (e,None) -> List.rev ((value_of_expr e state)::l)
      | ExprsNode (e1,Some(e2)) -> aux e2 ((value_of_expr e1 state)::l)
      | _ -> raise (Unknown_error_reference_interpretor "ruleTupleExpr")
  in aux exprs [])

and


(* ruleValue: ast -> (string, valueType ref) Hashtbl.t -> valueType
Function converting a value node into a valueType
Parameters:
  - v: the abstract syntax tree representing the value
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the value node
*)
ruleValue v =
  let rec aux vs l =
    (match vs with
      | ValueSeqNode (val1,None) -> List.rev ((ruleValue val1)::l)
      | ValueSeqNode (val1,Some(val2)) -> aux val2 ((ruleValue val1)::l)
      | _ -> raise (Unknown_error_reference_interpretor "ruleValue2")) in
  match v with
    | ValueNode (IntegerNode (_,i)) -> IntegerVal(i)
    | ValueNode (CharNode (_,c)) -> CharVal(c)
    | ValueNode (StringNode (_,s)) -> StringVal(s)
    | ValueNode (TrueNode(_)) -> BooleanVal(true)
    | ValueNode (FalseNode(_)) -> BooleanVal(false)
    | ValueNode (EmptyList) -> ListVal([])
    | ValueNode (ValueSeqNode (v1,v2)) -> ListVal (aux (ValueSeqNode(v1,v2)) [])
    | ValueSeqNode (_,_) -> TupleVal (aux v [])
    | _ -> raise (Unknown_error_reference_interpretor "ruleValue")
and


(* ruleIdentifier: ast -> (string, valueType ref) Hashtbl.t -> valueType
Function finding the value associated to an identifier
Parameters:
  - a: the abstract syntax tree representing the assignable
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the assignable
*)
ruleIdentifier a state = !(Hashtbl.find state a)


(* def_value: ast -> valueType
Function returning the default value for a type
Parameter:
  - t: the abstract syntax tree representing the type
Return: the default value for the type passed in parameter
*)
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


(* extend_state_with_params: ast -> (string, valueType ref) Hashtbl.t -> valueType -> unit
Function adding the parameters of a function to the state 
Parameters:
  - paramsnode: the abstract syntax tree representing the paramerers
  - state: hash table representing the state of all the variables
  - vparams: the value of the parameters
*)
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


(* extend_state_with_local_declas: ast -> (string, valueType ref) Hashtbl.t -> unit
Function adding the local variable of a function to the state 
Parameters:
  - body: the abstract syntax tree representing the function body
  - state: hash table representing the state of all the variables
*)  
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


(* create_local_state: ast -> ast -> (string, valueType ref) Hashtbl.t -> valueType -> (string, valueType ref) Hashtbl.t
Function creating a new local state containing the parameters, the global and local variables 
Parameters:
  - params: the abstract syntax tree representing the paramerers
  - body: the abstract syntax tree representing the function body
  - state: hash table representing the state of all the variables
  - vparams: the value of the parameters
Return: the new local state
*)  
let create_local_state params body state vparams =
  let local = Hashtbl.copy state in
  extend_state_with_params params local vparams;
  extend_state_with_local_declas body local;
  local


(* ruleInstr: ast -> (string, valueType ref) Hashtbl.t -> valueType option
Function executing a sequence of instructions and calculating its value
Parameters:
  - i: the abstract syntax tree representing the instructions
  - state: hash table representing the state of all the variables
Return: the value of the instructions if there is one
*)  
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


(* value_of_binstr: ast -> (string, valueType ref) Hashtbl.t -> valueType option
Function executing the basic instruction and calculating its value
Parameters:
  - binstr: the abstract syntax tree representing the basic instruction
  - state: hash table representing the state of all the variables
Return: the value of the basic instruction if there is one
*)
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


(* ruleAssignInstr: ast -> ast -> (string, valueType ref) Hashtbl.t -> valueType option
Function assigning a value to an assignable (update of the state)
Parameters:
  - a: the abstract syntax tree representing the assignable
  - e: the abstract syntax tree representing the expression to assign
  - state: hash table representing the state of all the variables
Return: None (this instruction return no value)
*)
ruleAssignInstr a e state =
  match a with 
    | AssignNode (_,name) ->
      let ve = value_of_expr e state in
      let pa = Hashtbl.find state name in
      pa := ve;
      None
    | _ -> raise (Unknown_error_reference_interpretor "ruleAssignInstr") 

and


(* ruleCallFuncWithReturn: ast -> string -> ast -> (string, valueType ref) Hashtbl.t -> valueType option
Function executing a function call and assigning its result to an assignable (update of the state)
Parameters:
  - a: the abstract syntax tree representing the assignable
  - namef: name of the function to call
  - e: the abstract syntax tree representing the expression to assign
  - state: hash table representing the state of all the variables
Return: None (this instruction return no value)
*)
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

(* ruleCallFuncVoid: string -> ast -> (string, valueType ref) Hashtbl.t -> valueType option
Function executing a function call
Parameters:
  - namef: name of the function to call
  - e: the abstract syntax tree representing the expression to assign
  - state: hash table representing the state of all the variables
Return: None (this instruction return no value)
*)
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


(* ruleIfThenElseInstr: ast -> ast -> ast -> (string, valueType ref) Hashtbl.t -> valueType option
Function executing a function call
Parameters:
  - cond: the abstract syntax tree representing the expression for the condition
  - i1: the abstract syntax tree representing the instruction in the then branch
  - i2: the abstract syntax tree representing the instruction in the else branch
  - state: hash table representing the state of all the variables
Return: the value obtained by executing the appropriate instruction (if there is one)
*)
ruleIfThenElseInstr cond i1 i2 state =
  let vcond = value_of_expr cond state in
  match vcond with
  | BooleanVal(true) -> ruleInstr i1 state
  | BooleanVal(false) -> ruleInstr i2 state
  | _ -> raise (Unknown_error_reference_interpretor "ruleIfThenElseInstr")

and 


(* ruleWhile: ast -> ast -> (string, valueType ref) Hashtbl.t -> valueType option
Function executing a function call
Parameters:
  - cond: the abstract syntax tree representing the expression for the condition
  - i: the abstract syntax tree representing the instruction in the body
  - state: hash table representing the state of all the variables
Return: the value obtained by executing the while instruction (if there is one)
*)
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


(* ruleWhile: valueType option
Function returning None (representing the absence of value)
Return: None
*)
ruleReturnVoid = None

and


(* ruleReturnFunc: string -> ast -> (string, valueType ref) Hashtbl.t -> valueType option
Function executing a function call and returning the value obtained by the execution
Parameters:
  - namef: name of the function to call
  - e: the abstract syntax tree representing the expression passed as parameter of the function
  - state: hash table representing the state of all the variables
Return: the value obtained by executing the function call (if there is one)
*)
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


(* ruleReturnExpr: ast -> (string, valueType ref) Hashtbl.t -> valueType option
Function calculating an expression and returning its value
Parameters:
  - e: the abstract syntax tree representing the expression
  - state: hash table representing the state of all the variables
Return: value of the expression
*)
ruleReturnExpr e state = Some(value_of_expr e state)


(* execution_prg: ast -> (position * string * ast) -> (position * string * ast * bool) list -> valueType option
Function executing a program and returning its result
Parameters:
  - prg: the abstract syntax tree representing the program
  - start: tuple representing the function which starts the program
  - env: the type environment created previously (cf. Resolve.ml)
Return: result of the execution of the program
*)
let execution_prg prg start env =
  (* Assignment of the type environment to the global variable envType *)
  envType := env;
  (* Creation of the hash table representing the state of all the varaibles *)
  let state = Hashtbl.create 100 in 
  (* Filling of the hash table with all the variables and functions *)
  let rec aux tree =
    match tree with
        | ProgramNode (p1,p2) -> aux p1; aux p2
        | FunctionNode (_,_,n,p,b) -> Hashtbl.add state n (ref (FuncVal(p,b)))
        | GlobalVarDeclaNode(_,_,n,vnode) -> Hashtbl.add state n (ref (ruleValue vnode))
        | _ -> ()
    in aux prg;
    (* Launch of the execution with the start function *)
    match start with
      | Some(_,name,expr) ->
        (match (!(Hashtbl.find state name)) with 
        | FuncVal(p,b) ->
          let ve = value_of_expr expr state in
          let localstate = create_local_state p b state ve in
          (match b with 
          | BodyNode (_,_,instr) -> ruleInstr instr localstate
          | _ -> raise (Unknown_error_reference_interpretor "execution_prg3"))
        | _ -> raise (Unknown_error_reference_interpretor "execution_prg2")) 
      | _ -> raise (Unknown_error_reference_interpretor "execution_prg")