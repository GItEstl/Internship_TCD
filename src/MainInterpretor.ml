open Ast
open ExpressionInterpretor
open BetaRedInterpretor
open Stack
open Hashtbl
open Random

(* Type of the possible actions for a thread *)
type actions =
  | BetaAction
  | TauAction
  | SendAction of int (* the interger is the id of the channel *)
  | ReceiveAction of int (* the interger is the id of the channel *)
  | SpawnAction

(* Type of the possible steps for a set of threads *)
type step =
  | ComStep of int * int * int (* the 3 intergers represent the id of the channel, the sender and the receiver respectively *)
  | BetaStep of int (* the integer represents the id of the tread *)
  | TauStep of int (* the integer represents the id of the tread *)
  | SpawnStep of int (* the integer represents the id of the tread *)

(* Type of the execution status *)
type execStatus =
  | Running (* Meaning a steo has been chosen and we can try to run a new step *)
  | Executed (* Meaning there is no more step to run and the execution has to end *)

exception Unknown_error_main_interpretor of string

(* Variable representing the configs of the threads *)
let configs = ref (Hashtbl.create 100)

let nbThreads = ref(0)

(* Variable representing the number of executed steps *)
let nbSteps = ref(0)

(* Boolean defining how much verbose the user wants *)
let vb1 = ref(false)
let vb2 = ref(false)
let vb3 = ref(false)

(* Variable representing the maximum number of steps *)
let max = ref(10000)

(* string_of_step: step -> string
Function converting a step into a string
Parameter:
  - step: the step to convert
Return: string representing the step
*)
let string_of_step step =
  match step with
  | ComStep(ch,ids,idr) -> "Com(ch: @" ^ (string_of_int ch) ^ ", s: " ^ (string_of_int ids) ^ ", r: " ^ (string_of_int idr) ^ ")" 
  | BetaStep(id) -> "Beta(" ^ (string_of_int id) ^ ")"
  | TauStep(id) -> "Tau(" ^ (string_of_int id) ^ ")"
  | SpawnStep(id) -> "Spawn(" ^ (string_of_int id) ^ ")" 

(* print_config: step list -> step -> unit
Function printing the configuration of a thread
Parameter:
    - all_steps: list of all the possible next steps
    - step: the chosen next step
*)
let print_exec_step all_steps step =
  let s = List.fold_left (fun string_vs e -> string_vs ^ (string_of_step e) ^ "; ") "" all_steps in
  print_string("Possible steps: " ^ s ^ "\n");
  print_string("Chosen step: " ^ (string_of_step step) ^ "\n")

(* string_of_eval_context: frame Stack.t -> string
Function converting the top frame of the evaluation stack into a string
Parameter:
  - e: stack of frames representing the evaluation contexts
Return: string representing the stack
*)
let string_of_eval_context e =
  if (is_empty e) then "--"
  else match (top e) with
    | FuncCallReturnFrame(_,a) -> "(state, " ^ a ^ ") :: E" 
    | FuncCallVoidFrame(_) -> "(state) :: E"
    | InstrSeqFrame(i) -> "(" ^ (string_of_instr i) ^ ") :: E"

(* string_of_state: state -> string
Function converting a state into a string
Parameter:
  - s: local state containing all the local variables
Return: string representing the state
*)
let string_of_state s =
  let seq = to_seq s in 
  let str = Seq.fold_left (fun string_vs (name,value) -> string_vs ^ name ^ " = " ^ (string_of_val (Some(!value))) ^ "; ") "" seq in
  if (String.equal str "") then "--" else str

(* print_config: int -> (ast ref * state ref * frame Stack.t ref) -> unit
Function printing the configuration of a thread
Parameter:
  - nb: id of the thread
  - i: the instruction to execute in the tread
  - s: the local state containing all the local variables for the thread 
  - e: the stack of frames representing the evaluation contexts for the thread
*)
let print_config nb (i,s,e) =
  print_string("  Thread " ^ (string_of_int nb) ^ ": \n");
  print_string("    Instruction: " ^ (string_of_instr (!i))^ "\n");
  print_string("    State: " ^ (string_of_state (!s)) ^ "\n");
  print_string("    Evaluation Context: " ^ (string_of_eval_context (!e)) ^ "\n\n")

(* print_configs : unit
Function printing the configuration of a thread
Global variables: 
  - configs: list of configs for all the threads
  - nbSteps: number of steps executed
*)
let print_configs () =
    print_string("\nStep " ^ (string_of_int(!nbSteps)) ^ ":\n");
    iter (fun i c -> print_config i c) (!configs)

(* string_of_state: ast -> state -> string
Function converting the result of a thread into a string
Global variable: 
  - g: the global state containing all the global variables
Parameter:
  - abstract syntax tree representing the last instruction of the thread
  - s: local state containing all the local variables for the thread
Return: string representing the result of the thread
*)
let result_to_string i s = 
  match (!i) with
  | Terminated(None) -> string_of_val None
  | Terminated(Some(expr)) -> string_of_val (Some(value_of_expr expr (!g,!s)))
  | _ -> string_of_val (Some(Deadlock))

(* string_of_state: string
Function converting the result of all the threads into a string
Global variable: 
  - configs: list of configs for all the threads
Return: string representing the result of all the threads
*)
let results_to_string () =
  if (!vb1) then 
    fold (fun nb (i,s,_) str -> str ^ "    Thread " ^ (string_of_int nb) ^ ": "^ (result_to_string i s) ^ "\n") (!configs) "\n"
  else let (i,s,_) = find (!configs) 0 in (result_to_string i s) ^ "\n"


let unfold_chan ch =
  match ch with 
    | ChannelVal(i) -> i
    | _ -> raise (Unknown_error_main_interpretor "not a channel in unfold_chan")

let ruleSend ch id =
  let (i,s,_) = find (!configs) id in
  let (expr,iNew) = (
  match (!i) with
    | SendNode(_,_,exp) -> (exp,NoopNode)
    | ChooseNode(_,c) ->
      let rec aux ast = 
      (match ast with
        | ChoicesNode(_,PrefixNode(_,Some(exp),Send,_),ipref,None) -> (exp,ipref)
        | ChoicesNode(_,PrefixNode(_,Some(exp),Send,Some(n)),ipref,Some(cs)) ->
          let idch = unfold_chan (ruleIdentifier n (!g,!s)) in 
          if (idch == ch) then (exp,ipref) else aux cs
        | ChoicesNode(_,_,_,Some(cs)) -> aux cs 
        | _ -> raise (Unknown_error_main_interpretor "ruleSend"))
      in aux c
    | _ -> raise (Unknown_error_main_interpretor "ruleSend")
  ) in 
  i := iNew;
  value_of_expr expr (!g,!s)

let ruleReceive ch id v =
  let (i,s,_) = find (!configs) id in
  let (assign,iNew) = (
  match (!i) with
    | ReceiveNode(_,AssignNode(_,name),_) -> (name,NoopNode)
    | ChooseNode(_,c) ->
      let rec aux ast = 
      (match ast with
        | ChoicesNode(_,PrefixNode(_,Some(AssignNode(_,name)),Receive,Some(_)),ipref,None) -> (name,ipref)
        | ChoicesNode(_,PrefixNode(_,Some(AssignNode(_,name)),Receive,Some(n)),ipref,Some(cs)) ->
          let idch = unfold_chan (ruleIdentifier n (!g,!s)) in  
          if (idch == ch) then (name,ipref) else aux cs
        | ChoicesNode(_,_,_,Some(cs)) -> aux cs 
        | _ -> raise (Unknown_error_main_interpretor "ruleReceive"))
      in aux c
    | _ -> raise (Unknown_error_main_interpretor "ruleReceive")
  ) in 
  let pa = find (!s) assign in
  pa := v;
  i := iNew

let ruleTau id =
  let (i,_,_) = find (!configs) id in
  match (!i) with
    | ChooseNode(_,c) ->
      let rec aux ast = 
      (match ast with
        | ChoicesNode(_,PrefixNode(_,_,Tau,_),ipref,_) -> i := ipref
        | ChoicesNode(_,_,_,Some(cs)) -> aux cs 
        | _ -> raise (Unknown_error_main_interpretor "ruleTau"))
      in aux c
    | _ -> raise (Unknown_error_main_interpretor "ruleTau")

let ruleSpawn id =
  let (i,s,_) = find (!configs) id in
  match (!i) with
    | SpawnNode(_,fname,expr) -> 
      let f = !(Hashtbl.find !g fname) in
      let ve = value_of_expr expr (!g,!s) in
      (match f with 
        | FuncVal(param,BodyNode(_,decla,instr)) -> 
          i := NoopNode;
          (ref(instr),ref(create_state param decla ve),ref(Stack.create ()))
        | _ -> raise (Unknown_error_main_interpretor "ruleSpawn1"))
    | _ -> raise (Unknown_error_main_interpretor "ruleSpawn2") 
     
let prefix_to_action (_,_,action,ch) s =
  match action,ch with
    | Tau,_ -> TauAction
    | Send,Some(n) -> let id = ruleIdentifier n (!g,!s) in SendAction(unfold_chan id)
    | Receive,Some(n) -> let id = ruleIdentifier n (!g,!s) in ReceiveAction(unfold_chan id)
    | _ -> raise (Unknown_error_main_interpretor "prefix_to_actions")

let check_possible_actions id (i,s,_) =
  match (!i) with
    | SendNode(_,n,_) -> let ch = ruleIdentifier n (!g,!s) in [(id,SendAction(unfold_chan ch))]
    | ReceiveNode(_,_,n) -> let ch = ruleIdentifier n (!g,!s) in [(id,ReceiveAction(unfold_chan ch))]
    | SpawnNode(_,_,_) -> [(id,SpawnAction)]
    | ChooseNode(_,c) ->
      let rec aux ast choices = 
      (match ast with
        | ChoicesNode(_,PrefixNode(pos,arg,act,ch),_,None) -> List.rev ((pos,arg,act,ch)::choices)
        | ChoicesNode(_,PrefixNode(pos,arg,act,ch),_,Some(cs)) -> aux cs ((pos,arg,act,ch)::choices)
        | _ -> raise (Unknown_error_main_interpretor "check_possible_actions")
      ) in 
      let list_acts = aux c [] in
      List.map (fun p -> (id,prefix_to_action p s)) list_acts
    | Terminated(None) -> if (id != 0) then remove (!configs) id else (); []
    | Terminated(_) -> []
    | _ -> [(id,BetaAction)]

let find_receive chan a idsender = 
  match a with
    | (id,ReceiveAction(ch)) -> (chan == ch) && (id != idsender)
    | _ -> false

let find_send chan a idreceiver = 
  match a with
    | (id,SendAction(ch)) -> (chan == ch) && (id != idreceiver)
    | _ -> false

let find_next_steps actions =
  let rec aux list_act steps =  
    match list_act with
      | (id,BetaAction)::q -> aux q ((BetaStep(id))::steps)
      | (id,TauAction)::q -> aux q ((TauStep(id))::steps)
      | (id,SendAction(chan))::q -> aux q ((List.map (fun (idr,_) -> ComStep(chan,id,idr)) (List.find_all (fun a -> find_receive chan a id) q))@steps)  
      | (id,ReceiveAction(chan))::q -> aux q ((List.map (fun (ids,_) -> ComStep(chan,ids,id)) (List.find_all (fun a -> find_send chan a id) q))@steps)
      | (id,SpawnAction)::q -> aux q ((SpawnStep(id))::steps)
      | [] -> steps
  in aux actions []

let exec_beta_steps id =
  let (i,s,e) = find (!configs) id in
  let rec aux instr = 
    let frame = if (is_empty (!e)) then None else Some(Stack.top (!e)) in
    match instr with
      | SendNode(_,_,_) | ReceiveNode(_,_,_) | ChooseNode(_,_) | SpawnNode(_,_,_) | Terminated(_) -> i := instr
      | _ -> aux (exec_beta_step instr frame s e)
  in aux (!i) 

let exec_step ()=
  let all_actions = fold (fun id c l -> (check_possible_actions id c)@l) (!configs) [] in
  let all_steps = find_next_steps all_actions in
  let size = List.length all_steps in
  if (size > 0) then
    let n = (Random.bits ()) mod size in
    let step = List.nth all_steps n in
    (match step with
      | ComStep (ch,idsender,idreceiver) -> let v = ruleSend ch idsender in ruleReceive ch idreceiver v
      | BetaStep(id) -> exec_beta_steps id
      | TauStep(id) -> ruleTau id
      | SpawnStep(id) -> let newc = ruleSpawn id in add (!configs) (!nbThreads) newc; 
                         nbThreads := (!nbThreads) + 1);
    if (!vb2) then print_exec_step all_steps step else ();
    Running
  else
    (if (!vb2) then print_string("Possible Steps: -- \nChosen Step: -- \n") else ();
    Executed)

let init_prg ast env_type start verbosity seed maxstep =
  (* Cofiguration of the verbosity *)
  (match verbosity with
  | 0 -> vb1 := false; vb2:= false; vb3:= false
  | 1 -> vb1 := true; vb2:= false; vb3:= false
  | 2 -> vb1 := true; vb2:= true; vb3:= false
  | 3 -> vb1 := true; vb2:= true; vb3:= true
  | _ -> vb1 := false; vb2:= false; vb3:= false);
  (* Initialisation of the seed *)
  let init_seed = if (seed < 0) then (self_init (); bits ()) else seed in
  Random.init init_seed;
  if (!vb1) then
    print_string("The program will be excuted with the seed " ^ (string_of_int init_seed) ^ "\n")
  else ();
  (* Configuration of the maximum number of steps *)
  max := maxstep; 
  (* Assignment of the type environment and creation of the global state *)
  envType := env_type;
  g := Hashtbl.create 100;
  nbSteps:= 0;
  (* Filling of the hash table with all the variables and functions *)
  let rec aux tree =
    match tree with
        | ProgramNode (p1,p2) -> aux p1; aux p2
        | FunctionNode (_,_,n,p,b) -> add (!g) n (ref (FuncVal(p,b)))
        | GlobalVarDeclaNode(_,_,n,vnode) -> add (!g) n (ref (ruleValue vnode))
        | GlobalChanDeclaNode(_,_,n) -> add (!g) n (ref (ChannelVal(Channel.create_chan_id ())))
        | _ -> ()
    in aux ast;
    (* Creation of the configuration set *)
    match start with
      | Some(_,name,expr) -> 
        let f = !(Hashtbl.find !g name) in
        let ve = value_of_expr expr (!g,!g) in
        (match f with 
          | FuncVal(param,BodyNode(_,decla,instr)) -> 
            (* At the start, the configs variable only contains the main thread *)
            configs := Hashtbl.create 100;
            add (!configs) 0 (ref(instr),ref(create_state param decla ve),ref(Stack.create ()));
            nbThreads := 1 
          | _ -> raise (Unknown_error_main_interpretor "init_prg"))
      | _ -> raise (Unknown_error_main_interpretor "init_prg")

let rec run_prg () =
  if (!vb3) then print_configs () else ();
  nbSteps := (!nbSteps) + 1;
  let status = exec_step () in
  if ((status == Running) && (!nbSteps <= !max)) then
    run_prg ()
  else 
    results_to_string ()