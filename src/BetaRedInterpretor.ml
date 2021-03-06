open Ast
open ExpressionInterpretor
open Stack
open Hashtbl

(* Type of a state *)
type state = (string, valueType ref) Hashtbl.t

(* Type of the frames inside the evaluation stack *)
type frame = 
  | FuncCallReturnFrame of state * string
  | FuncCallVoidFrame of state
  | InstrSeqFrame of ast

exception Unknown_error_betared_interpretor of string

(* Variable representing the type environment (shared with the file MainInterpretor.ml) *)
let envType = ref([(Lexing.dummy_pos,"",NoopNode,false)])
(* Variable representing the global state (shared with the file MainInterpretor.ml) *)
let g = ref (Hashtbl.create 100)

(* def_value: ast -> valueType
Function returning the default value for a type
Global variable: 
  - envType: the type environment of the program
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
        | _ -> raise (Unknown_error_betared_interpretor "def_value2")
      in aux tSeq [])
    | NamedTypeNode (_,st) -> let _,_,t,_ = (List.find (fun (_,name,_,_) -> String.equal name st) !envType) in (def_value t)
    | ChanTNode(_,_) -> ChannelVal(Channel.create_chan_id ())
    | _ -> raise (Unknown_error_betared_interpretor "def_value")


(* extend_state_with_params: ast -> state -> valueType -> unit
Function adding the parameters of a function to the state 
Parameters:
  - paramsnode: the abstract syntax tree representing the paramerers
  - state: hash table representing the state of all the local variables
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
      | _ -> raise (Unknown_error_betared_interpretor "extend_state_with_params2")
  in aux paramsnode vp


(* extend_state_with_local_declas: ast -> state -> unit
Function adding the local variable of a function to the state 
Parameters:
  - decla: the abstract syntax tree representing the local declarations
  - state: hash table representing the state of all the local variables
*)  
let extend_state_with_local_declas decla state =
  match decla with
    | None -> ()
    | Some(d) -> 
      let rec aux d =
        (match d with
          | VariableDeclasNode (VariableDeclaNode (_,t,name),None) -> Hashtbl.add state name (ref(def_value t)) 
          | VariableDeclasNode (VariableDeclaNode (_,t,name),Some(vs)) -> Hashtbl.add state name (ref(def_value t)); aux vs
          | _ -> raise (Unknown_error_betared_interpretor "extend_state_with_local_declas2")
        )
      in aux d

(* create_state: ast -> ast -> valueType -> state
Function creating a new local state containing the parameters and local variables 
Parameters:
  - params: the abstract syntax tree representing the paramerers
  - decla: the abstract syntax tree representing the local declarations
  - vparams: the value of the parameters
Return: the new local state
*)  
let create_state params decla vparams =
  let local = Hashtbl.create 100 in
  extend_state_with_params params local vparams;
  extend_state_with_local_declas decla local;
  local

(* ruleSeqInstr: ast -> frame Stack.t ref -> ast
Function processing a sequence of instructions 
Parameters:
  - ast: the abstract syntax tree representing the sequence
  - e: stack of frames representing the evaluation contexts
Return: the next instruction to execute
*)  
let ruleSeqInstr ast e =
  match ast with 
    | InstrSeqNode(i,None) -> i
    | InstrSeqNode(i,Some(iseq)) -> 
      (match iseq with 
        | InstrSeqNode(bi,None) -> push (InstrSeqFrame(bi)) (!e); i
        | _ -> push (InstrSeqFrame(iseq)) (!e); i)
    | _ -> raise (Unknown_error_betared_interpretor "ruleSeqInstr1") 

(* ruleAssignInstr: ast -> ast -> state ref -> ast
Function processing an assign instruction
Global variable: 
  - g: the global state containing all the global variables
Parameters:
  - a: the abstract syntax tree representing the assignable
  - expr: the abstract syntax tree representing the expression to assigned
  - s: the local state containing all the local variables
Return: a noop instruction
*)  
let ruleAssignInstr a expr s =
  match a with 
    | AssignNode (_,name) ->
      let ve = value_of_expr expr (!g,!s) in
      let pa = find (!s) name in
      pa := ve;
      NoopNode
    | _ -> raise (Unknown_error_betared_interpretor "ruleAssignInstr")

(* ruleCallFuncWithReturn: ast -> string -> ast -> state ref -> frame Stack.t ref -> ast
Function processing a function call with a function having return
Global variable: 
  - g: the global state containing all the global variables
Parameters:
  - a: the abstract syntax tree representing the assignable
  - namef: name of the called function
  - expr: abstract syntax tree representing the expression used as parameter for the function
  - s: the local state containing all the local variables
  - e: stack of frames representing the evaluation contexts
Return: a noop instruction
*)  
let ruleCallFuncWithReturn a namef expr s e = 
  let f = !(find !g namef) in
  let ve = value_of_expr expr (!g,!s) in
  match (f,a) with 
    | (FuncVal(param,BodyNode(_,decla,instr)),AssignNode(_,name)) ->
      push (FuncCallReturnFrame(copy (!s),name)) (!e);
      s := create_state param decla ve;
      instr 
    | _ -> raise (Unknown_error_betared_interpretor "ruleCallFuncWithReturn") 

(* ruleCallFuncVoid: string -> ast -> state ref -> frame Stack.t ref -> ast
Function processing a function call with a function having void return or no return
Global variable: 
  - g: the global state containing all the global variables 
Parameters:
  - namef: name of the called function
  - expr: abstract syntax tree representing the expression used as parameter for the function
  - s: the local state containing all the local variables
  - e: stack of frames representing the evaluation contexts
Return: a noop instruction
*)  
let ruleCallFuncVoid namef expr s e = 
  let f = !(Hashtbl.find !g namef) in
  let ve = value_of_expr expr (!g,!s) in
  match f with 
    | FuncVal(param,BodyNode(_,decla,instr)) ->
      push (FuncCallVoidFrame(copy (!s))) (!e);
      s := create_state param decla ve;
      instr 
    | _ -> raise (Unknown_error_betared_interpretor "ruleCallFuncVoid")

(* ruleIfThenElseInstr: ast -> ast -> ast -> state ref -> ast
Function processing a conditional instruction
Global variable: 
  - g: the global state containing all the global variables 
Parameters:
  - cond: abstract syntax tree representing the expression inside the condition
  - i1: abstract syntax tree representing the instruction inside the then branch
  - i2: abstract syntax tree representing the instruction inside the else branch
  - s: the local state containing all the local variables
Return: the next instruction to execute
*)  
let ruleIfThenElseInstr cond i1 i2 s =
  let vcond = value_of_expr cond (!g,!s) in
  match vcond with
  | BooleanVal(true) -> i1
  | BooleanVal(false) -> i2
  | _ -> raise (Unknown_error_betared_interpretor "ruleIfThenElseInstr")

(* ruleWhile: position -> ast -> ast -> state ref -> ast
Function processing a while instruction
Global variable: 
  - g: the global state containing all the global variables
Parameters:
  - cond: abstract syntax tree representing the expression inside the condition
  - instr: abstract syntax tree representing the instruction inside the the body
  - s: the local state containing all the local variables
Return: the next instruction to execute
*)  
let ruleWhile pos cond instr s =
  let vcond = value_of_expr cond (!g,!s) in
  match vcond with
  | BooleanVal(true) -> InstrSeqNode(instr,Some(WhileNode(pos,cond,instr)))
  | BooleanVal(false) -> NoopNode
  | _ -> raise (Unknown_error_betared_interpretor "ruleWhile")

(* ruleReturnBreak: ast -> frame Stack.t ref -> ast
Function poping the last frame
Parameters:
  - ast: abstract syntax tree representing a return instruction
  - e: stack of frames representing the evaluation contexts
Return: the same return instruction
Insight: This function is useful when a return have to break the flow of execution
*) 
let ruleReturnBreak ast e = 
  let _ = pop (!e) in ast

(* ruleNoop: frame Stack.t ref -> ast
Function giving the next basic instruction by poping the last frame of the evaluation
stack and recovering the instruction inside the Sequence Frame
Parameter:
  - e: stack of frames representing the evaluation contexts
Return: the next instruction to execute
Insight: This function is useful when the execution of the precedent instruction is
finished (noopNode) and the top frame of the evaluation stack is a Sequence Frame 
*) 
let ruleNoop e =
  let frame = pop (!e) in
  match frame with
    | InstrSeqFrame(i) -> i
    | _ -> raise (Unknown_error_betared_interpretor "ruleNoop")

(* ruleAssignReturn: ast -> state ref -> frame Stack.t ref -> ast
Function assigning the return of a function to a variable stocked in the evaluation stack and
restauring the state before the call
Global variable: 
  - g: the global state containing all the global variables
Parameters:
  - ast: abstract syntax tree representing a return instruction
  - s: the local state containing all the local variables
  - e: stack of frames representing the evaluation contexts
Return: a noop instruction
Insight: This function is useful when a function call is finished and you want to 
assign the result and restaured the old local state
*) 
let ruleAssignReturn ast s e =
  match ast with
  | ReturnNode (_,Some(expr)) -> 
    let ve = value_of_expr expr (!g,!s) in
    let frame = pop (!e) in
    (match frame with 
      | FuncCallReturnFrame(old_s,name) -> 
        s := old_s;
        let pa = find (!s) name in
        pa := ve;
        NoopNode
      | _ -> raise (Unknown_error_betared_interpretor "ruleAssignReturn2"))
  | _ -> raise (Unknown_error_betared_interpretor "ruleAssignReturn1")

(* ruleVoid: state ref -> frame Stack.t ref -> ast
Function restauring the state before a function call with a void return
Parameters:
  - s: the local state containing all the local variables
  - e: stack of frames representing the evaluation contexts
Return: a noop instruction
Insight: This function is useful when a function call is finished and you want to 
assign the result and restaured the old local state
*) 
let ruleVoid s e =
  let frame = pop (!e) in
  match frame with 
    | FuncCallVoidFrame(old_s) -> 
      s := old_s;
      NoopNode
    | _ -> raise (Unknown_error_betared_interpretor "ruleVoid")

(* ruleNewChan: ast -> state ref -> ast
Function restauring the state before a function call with a void return
Parameters:
  - ast: abstract syntax tree representing the assignable receiving the new channel
  - s: the local state containing all the local variables
Return: a noop instruction
*) 
let ruleNewChan ast s = 
  match ast with 
  | AssignNode (_,name) ->
    let ch = ChannelVal(Channel.create_chan_id ()) in
    let pa = find (!s) name in
    pa := ch;
    NoopNode
  | _ -> raise (Unknown_error_betared_interpretor "ruleNewChan")

(* exec_beta_step: ast -> frame option -> state ref -> frame Stack.t ref -> ast
Function matching an instruction and the top frame with the right excution rule 
Parameters:
  - ast: abstract syntax tree representing the instruction to execute
  - frame: the frame at the top of the evaluation stack if there is one 
  - s: the local state containing all the local variables
  - e: stack of frames representing the evaluation contexts
Return: the next instruction to execute
*) 
let exec_beta_step ast frame s e =
  match (frame,ast) with 
    | (_, InstrSeqNode(_,_)) -> ruleSeqInstr ast e
    | (_,BinaryNode (_,a,_,CallNode (_,namef,expr))) -> ruleCallFuncWithReturn a namef expr s e
    | (_, BinaryNode (_,a,_,expr)) -> ruleAssignInstr a expr s
    | (_, CallNode (_,namef,expr)) -> ruleCallFuncVoid namef expr s e
    | (_, IfthenelseInstrNode (_,cond,i1,i2)) -> ruleIfThenElseInstr cond i1 i2 s
    | (_, WhileNode (pos,expr,i)) -> ruleWhile pos expr i s
    | (Some(InstrSeqFrame(_)), ReturnNode (_,_)) -> ruleReturnBreak ast e
    | (Some(InstrSeqFrame(_)), NoopNode) -> ruleNoop e
    | (Some(FuncCallReturnFrame(_,_)), ReturnNode (_,_)) -> ruleAssignReturn ast s e
    | (Some(FuncCallVoidFrame(_)), ReturnNode (_,_)) -> ruleVoid s e
    | (_, NewNode(_,ast)) -> ruleNewChan ast s
  (* Special cases *)
      (* End of the main function of a thread with no return *)
    | (None,NoopNode) -> ReturnNode(Lexing.dummy_pos,None)
      (* End of function call with no return *)
    | (Some(FuncCallVoidFrame(_)), NoopNode) -> ReturnNode(Lexing.dummy_pos,None)
      (* Final return of a thread *)
    | (None,ReturnNode(_,e)) -> Terminated(e)
      (* Something has gone wrong *)
    | (_,_) -> raise (Unknown_error_betared_interpretor "exec_beta_step")