open Ast
open ExpressionInterpretor
open Stack
open Hashtbl

type state = (string, valueType ref) Hashtbl.t

type frame = 
  | FuncCallReturnFrame of state * string
  | FuncCallVoidFrame of state
  | InstrSeqFrame of ast

let envType = ref([(Lexing.dummy_pos,"",NoopNode,false)])
let g = ref (Hashtbl.create 100)
let s = ref (Hashtbl.create 100)
let e = ref (Stack.create ()) 

exception Unknown_error_reference_interpretor_ of (frame option * ast)

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
  - body: the abstract syntax tree representing the function body
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


(* create_local_state: ast -> ast -> (string, valueType ref) Hashtbl.t -> valueType -> (string, valueType ref) Hashtbl.t
Function creating a new local state containing the parameters, the global and local variables 
Parameters:
  - params: the abstract syntax tree representing the paramerers
  - body: the abstract syntax tree representing the function body
  - state: hash table representing the state of all the variables
  - vparams: the value of the parameters
Return: the new local state
*)  
let create_state params decla vparams =
  let local = Hashtbl.create 100 in
  extend_state_with_params params local vparams;
  extend_state_with_local_declas decla local;
  local

(* Numero 8 *)
let ruleSeqInstr ast =
  match ast with 
    | InstrSeqNode(i,None) -> i
    | InstrSeqNode(i,Some(iseq)) -> push (InstrSeqFrame(iseq)) (!e); i
    | _ -> raise (Unknown_error_reference_interpretor "ruleSeqInstr")

(* Numero 10 *)
let ruleAssignInstr a expr =
  match a with 
    | AssignNode (_,name) ->
      let ve = value_of_expr expr (!g,!s) in
      let pa = find (!s) name in
      pa := ve;
      NoopNode
    | _ -> raise (Unknown_error_reference_interpretor "ruleAssignInstr")

(* Numero 1 *)
let ruleCallFuncWithReturn a namef expr = 
  let f = !(find !g namef) in
  let ve = value_of_expr expr (!g,!s) in
  match (f,a) with 
    | (FuncVal(param,BodyNode(_,decla,instr)),AssignNode(_,name)) ->
      push (FuncCallReturnFrame(copy (!s),name)) (!e);
      s := create_state param decla ve;
      instr 
    | _ -> raise (Unknown_error_reference_interpretor "ruleCallFuncWithReturn") 

(* Numero 2 *)
let ruleCallFuncVoid namef expr = 
  let f = !(Hashtbl.find !g namef) in
  let ve = value_of_expr expr (!g,!s) in
  match f with 
    | FuncVal(param,BodyNode(_,decla,instr)) ->
      push (FuncCallVoidFrame(copy (!s))) (!e);
      s := create_state param decla ve;
      instr 
    | _ -> raise (Unknown_error_reference_interpretor "ruleCallFuncVoid")

(* Numero / *)
let ruleIfThenElseInstr cond i1 i2 =
  let vcond = value_of_expr cond (!g,!s) in
  match vcond with
  | BooleanVal(true) -> i1
  | BooleanVal(false) -> i2
  | _ -> raise (Unknown_error_reference_interpretor "ruleIfThenElseInstr")

(* Numero 11 et 12 *)
let ruleWhile pos cond instr =
  let vcond = value_of_expr cond (!g,!s) in
  match vcond with
  | BooleanVal(true) -> InstrSeqNode(instr,Some(WhileNode(pos,cond,instr)))
  | BooleanVal(false) -> NoopNode
  | _ -> raise (Unknown_error_reference_interpretor "ruleWhile")

(* Numero 13 et 14 *)
let ruleReturnBreak ast = 
  let _ = pop (!e) in ast

(* Numero 9 *)
let ruleNoop () =
  let frame = pop (!e) in
  match frame with
    | InstrSeqFrame(i) -> i
    | _ -> raise (Unknown_error_reference_interpretor "ruleNoop")

(* Numero 3 *)
let ruleAssignReturn ast =
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
let ruleVoid () =
  let frame = pop (!e) in
  match frame with 
    | FuncCallVoidFrame(old_s) -> 
      s := old_s;
      NoopNode
    | _ -> raise (Unknown_error_reference_interpretor "ruleVoid")

let ruleFinalValue ast =
  match ast with
    | ReturnNode (_, None) -> None
    | ReturnNode (_,Some(expr)) -> Some(value_of_expr expr (!g,!s))
    | _ -> raise (Unknown_error_reference_interpretor "ruleFinalValue")

let ruleNewChan ast = 
  match ast with 
  | AssignNode (_,name) ->
    let ch = ChannelVal(Channel.create_chan_id ()) in
    let pa = find (!s) name in
    pa := ch;
    NoopNode
  | _ -> raise (Unknown_error_reference_interpretor "ruleNewChan")

let exec_step ast frame =
  match (frame,ast) with 
    | (_, InstrSeqNode(_,_)) -> ruleSeqInstr ast
    | (_,BinaryNode (_,a,Assign,CallNode (_,namef,expr))) -> ruleCallFuncWithReturn a namef expr
    | (_, BinaryNode (_,a,Assign,expr)) -> ruleAssignInstr a expr
    | (_, CallNode (_,namef,expr)) -> ruleCallFuncVoid namef expr
    | (_, IfthenelseInstrNode (_,cond,i1,i2)) -> ruleIfThenElseInstr cond i1 i2
    | (_, WhileNode (pos,expr,i)) -> ruleWhile pos expr i
    | (_, ReturnNode (_,Some (CallNode (_,_,_)))) -> raise (Unknown_error_reference_interpretor "notImplemented")
    | (Some(InstrSeqFrame(_)), ReturnNode (_,_)) -> ruleReturnBreak ast
    | (Some(InstrSeqFrame(_)), NoopNode) -> ruleNoop ()
    | (Some(FuncCallReturnFrame(_,_)), ReturnNode (_,_)) -> ruleAssignReturn ast
    | (Some(FuncCallVoidFrame(_)), ReturnNode (_,_)) -> ruleVoid ()
    | (_, NewNode(_,ast)) -> ruleNewChan ast
    | (None,NoopNode) -> ReturnNode(Lexing.dummy_pos,None)
    | (Some(FuncCallVoidFrame(_)), NoopNode) -> ReturnNode(Lexing.dummy_pos,None)
    | (_,_) -> raise (Unknown_error_reference_interpretor_ (frame,ast))
    
let rec exec_prg ast =
  if (is_empty (!e)) then
    (match ast with
      | ReturnNode (_,_) -> ruleFinalValue ast
      | _ -> exec_prg (exec_step ast None))
  else exec_prg (exec_step ast (Some(Stack.top (!e))))
    
let run_prg ast env_type start = 
  (* Assignment of the type environment to the global variable envType *)
  envType := env_type;
  g := Hashtbl.create 100;
  s := Hashtbl.create 100;
  e := Stack.create (); 
  (* Filling of the hash table with all the variables and functions *)
  let rec aux tree =
    match tree with
        | ProgramNode (p1,p2) -> aux p1; aux p2
        | FunctionNode (_,_,n,p,b) -> add (!g) n (ref (FuncVal(p,b)))
        | GlobalVarDeclaNode(_,_,n,vnode) -> add (!g) n (ref (ruleValue vnode))
        | GlobalChanDeclaNode(_,_,n) -> add (!g) n (ref (ChannelVal(Channel.create_chan_id ())))
        | _ -> ()
    in aux ast;
    (* Launch of the execution with the start function *)
    match start with
      | Some(_,name,expr) -> 
        let f = !(Hashtbl.find !g name) in
        let ve = value_of_expr expr (!g,!s) in
        (match f with 
          | FuncVal(param,BodyNode(_,decla,instr)) -> 
            s := create_state param decla ve;
            exec_prg instr
          | _ -> raise (Unknown_error_reference_interpretor "run_prg2"))
      | _ -> raise (Unknown_error_reference_interpretor "run_prg1")