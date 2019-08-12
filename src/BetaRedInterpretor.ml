open Ast
open ExpressionInterpretor
open Stack
open Hashtbl

type state = (string, valueType ref) Hashtbl.t

type frame = 
  | FuncCallReturnFrame of state * string
  | FuncCallVoidFrame of state
  | InstrSeqFrame of ast

exception Unknown_error_reference_interpretor_ of (frame option * ast)
exception Test of ast

let envType = ref([(Lexing.dummy_pos,"",NoopNode,false)])
let g = ref (Hashtbl.create 100)

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
    | ChanTNode(_,_) -> ChannelVal(Channel.create_chan_id ())
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
  - decla: the abstract syntax tree representing the local declarations
  - state: hash table representing the state of all the variables
*)  
let extend_state_with_local_declas decla state =
  match decla with
    | None -> ()
    | Some(d) -> 
      let rec aux d =
        (match d with
          | VariableDeclasNode (VariableDeclaNode (_,t,name),None) -> Hashtbl.add state name (ref(def_value t)) 
          | VariableDeclasNode (VariableDeclaNode (_,t,name),Some(vs)) -> Hashtbl.add state name (ref(def_value t)); aux vs
          | _ -> raise (Unknown_error_reference_interpretor "extend_state_with_local_declas2")
        )
      in aux d


(* create_local_state: ast -> ast -> valueType -> (string, valueType ref) Hashtbl.t
Function creating a new local state containing the parameters, the global and local variables 
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

(* Numero 8 *)
let ruleSeqInstr ast e =
  match ast with 
    | InstrSeqNode(i,None) -> i
    | InstrSeqNode(i,Some(iseq)) -> 
      (match iseq with 
        | InstrSeqNode(bi,None) -> push (InstrSeqFrame(bi)) (!e); i
        | _ -> push (InstrSeqFrame(iseq)) (!e); i)
    | _ -> raise (Unknown_error_reference_interpretor "ruleSeqInstr1") 

(* Numero 10 *)
let ruleAssignInstr a expr s =
  match a with 
    | AssignNode (_,name) ->
      let ve = value_of_expr expr (!g,!s) in
      let pa = find (!s) name in
      pa := ve;
      NoopNode
    | _ -> raise (Unknown_error_reference_interpretor "ruleAssignInstr")

(* Numero 1 *)
let ruleCallFuncWithReturn a namef expr s e = 
  let f = !(find !g namef) in
  let ve = value_of_expr expr (!g,!s) in
  match (f,a) with 
    | (FuncVal(param,BodyNode(_,decla,instr)),AssignNode(_,name)) ->
      push (FuncCallReturnFrame(copy (!s),name)) (!e);
      s := create_state param decla ve;
      instr 
    | _ -> raise (Unknown_error_reference_interpretor "ruleCallFuncWithReturn") 

(* Numero 2 *)
let ruleCallFuncVoid namef expr s e = 
  let f = !(Hashtbl.find !g namef) in
  let ve = value_of_expr expr (!g,!s) in
  match f with 
    | FuncVal(param,BodyNode(_,decla,instr)) ->
      push (FuncCallVoidFrame(copy (!s))) (!e);
      s := create_state param decla ve;
      instr 
    | _ -> raise (Unknown_error_reference_interpretor "ruleCallFuncVoid")

(* Numero / *)
let ruleIfThenElseInstr cond i1 i2 s =
  let vcond = value_of_expr cond (!g,!s) in
  match vcond with
  | BooleanVal(true) -> i1
  | BooleanVal(false) -> i2
  | _ -> raise (Unknown_error_reference_interpretor "ruleIfThenElseInstr")

(* Numero 11 et 12 *)
let ruleWhile pos cond instr s =
  let vcond = value_of_expr cond (!g,!s) in
  match vcond with
  | BooleanVal(true) -> InstrSeqNode(instr,Some(WhileNode(pos,cond,instr)))
  | BooleanVal(false) -> NoopNode
  | _ -> raise (Unknown_error_reference_interpretor "ruleWhile")

(* Numero 13 et 14 *)
let ruleReturnBreak ast e = 
  let _ = pop (!e) in ast

(* Numero 9 *)
let ruleNoop e =
  let frame = pop (!e) in
  match frame with
    | InstrSeqFrame(i) -> i
    | _ -> raise (Unknown_error_reference_interpretor "ruleNoop")

(* Numero 3 *)
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
      | _ -> raise (Unknown_error_reference_interpretor "ruleAssignReturn2"))
  | _ -> raise (Unknown_error_reference_interpretor "ruleAssignReturn1")

(* Numero 4 *)
let ruleVoid s e =
  let frame = pop (!e) in
  match frame with 
    | FuncCallVoidFrame(old_s) -> 
      s := old_s;
      NoopNode
    | _ -> raise (Unknown_error_reference_interpretor "ruleVoid")

let ruleNewChan ast s = 
  match ast with 
  | AssignNode (_,name) ->
    let ch = ChannelVal(Channel.create_chan_id ()) in
    let pa = find (!s) name in
    pa := ch;
    NoopNode
  | _ -> raise (Unknown_error_reference_interpretor "ruleNewChan")

let exec_beta_step ast frame s e =
  match (frame,ast) with 
    | (_, InstrSeqNode(_,_)) -> ruleSeqInstr ast e
    | (_,BinaryNode (_,a,_,CallNode (_,namef,expr))) -> ruleCallFuncWithReturn a namef expr s e
    | (_, BinaryNode (_,a,_,expr)) -> ruleAssignInstr a expr s
    | (_, CallNode (_,namef,expr)) -> ruleCallFuncVoid namef expr s e
    | (_, IfthenelseInstrNode (_,cond,i1,i2)) -> ruleIfThenElseInstr cond i1 i2 s
    | (_, WhileNode (pos,expr,i)) -> ruleWhile pos expr i s
    | (_, ReturnNode (_,Some (CallNode (_,_,_)))) -> raise (Unknown_error_reference_interpretor "notImplemented")
    | (Some(InstrSeqFrame(_)), ReturnNode (_,_)) -> ruleReturnBreak ast e
    | (Some(InstrSeqFrame(_)), NoopNode) -> ruleNoop e
    | (Some(FuncCallReturnFrame(_,_)), ReturnNode (_,_)) -> ruleAssignReturn ast s e
    | (Some(FuncCallVoidFrame(_)), ReturnNode (_,_)) -> ruleVoid s e
    | (_, NewNode(_,ast)) -> ruleNewChan ast s
    | (None,NoopNode) -> ReturnNode(Lexing.dummy_pos,None)
    | (Some(FuncCallVoidFrame(_)), NoopNode) -> ReturnNode(Lexing.dummy_pos,None)
    | (None,ReturnNode(_,e)) -> Terminated(e)
    | (_,_) -> raise (Unknown_error_reference_interpretor_ (frame,ast))