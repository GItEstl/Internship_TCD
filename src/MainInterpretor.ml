open Ast
open ExpressionInterpretor
open BetaRedInterpretor
open Stack
open Hashtbl
open Random

(* Type of the possible actions for a thread *)
type action =
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
  - configs: hashtable matching the id of a thread to its config
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
  - configs: hashtable matching the id of a thread to its config
Return: string representing the result of all the threads
*)
let results_to_string () =
  if (!vb1) then 
    fold (fun nb (i,s,_) str -> str ^ "    Thread " ^ (string_of_int nb) ^ ": "^ (result_to_string i s) ^ "\n") (!configs) "\n"
  else let (i,s,_) = find (!configs) 0 in (result_to_string i s) ^ "\n"

(* string_of_state: valueType -> int
Function unfolding a channel valueType to recover its identifier
Parameter:
  - ch: valueType of the channel
Return: the id of the channel
*)
let unfold_chan ch =
  match ch with 
    | ChannelVal(i) -> i
    | _ -> raise (Unknown_error_main_interpretor "not a channel in unfold_chan")

(* ruleSend: int -> int -> valueType
Function executing a sending on a thread
Global variable: 
  - configs: hashtable matching the id of a thread to its config
Parameter:
  - ch: valueType of the communication channel
  - id: identifier of the thread executing the sending
Return: valueType of the sent expression
*)
let ruleSend ch id =
  let (i,s,_) = find (!configs) id in
  let (expr,iNew) = (
  match (!i) with
    (* If the sending is a send instruction *)
    | SendNode(_,_,exp) -> (exp,NoopNode)
    (* If the sending is a send prefix inside a choose instruction *)
    | ChooseNode(_,c) ->
      let rec aux ast = 
      (match ast with
        (* Last choice of the choose instruction ie necesaarily the right one *)
        | ChoicesNode(_,PrefixNode(_,Some(exp),Send,_),ipref,None) -> (exp,ipref)
        (* The prefix is a send, we check that is the right one, if not we skip to the next chocie *)
        | ChoicesNode(_,PrefixNode(_,Some(exp),Send,Some(n)),ipref,Some(cs)) ->
          let idch = unfold_chan (ruleIdentifier n (!g,!s)) in 
          if (idch == ch) then (exp,ipref) else aux cs
        (* The prefix is not a send, we skip to the next choice *)
        | ChoicesNode(_,_,_,Some(cs)) -> aux cs 
        | _ -> raise (Unknown_error_main_interpretor "ruleSend"))
      in aux c
    | _ -> raise (Unknown_error_main_interpretor "ruleSend")
  ) in 
  i := iNew;
  value_of_expr expr (!g,!s)

(* ruleReceive: int -> int -> valueType -> unit
Function executing a reception on a thread
Global variable: 
  - configs: hashtable matching the id of a thread to its config
Parameters:
  - ch: valueType of the communication channel
  - id: identifier of the thread executing the reception
*)
let ruleReceive ch id v =
  let (i,s,_) = find (!configs) id in
  let (assign,iNew) = (
  match (!i) with
    (* If the reception is an receive instruction *)
    | ReceiveNode(_,AssignNode(_,name),_) -> (name,NoopNode)
    (* If the reception is a receive prefix inside a choose instruction *)
    | ChooseNode(_,c) ->
      let rec aux ast = 
      (match ast with
        (* Last choice of the choose instruction ie necesaarily the right one *)
        | ChoicesNode(_,PrefixNode(_,Some(AssignNode(_,name)),Receive,Some(_)),ipref,None) -> (name,ipref)
        (* The prefix is a receive, we check that is the right one, if not we skip to the next chocie *)
        | ChoicesNode(_,PrefixNode(_,Some(AssignNode(_,name)),Receive,Some(n)),ipref,Some(cs)) ->
          let idch = unfold_chan (ruleIdentifier n (!g,!s)) in  
          if (idch == ch) then (name,ipref) else aux cs
        (* The prefix is not a receive, we skip to the next choice *)
        | ChoicesNode(_,_,_,Some(cs)) -> aux cs 
        | _ -> raise (Unknown_error_main_interpretor "ruleReceive"))
      in aux c
    | _ -> raise (Unknown_error_main_interpretor "ruleReceive")
  ) in 
  let pa = find (!s) assign in
  pa := v;
  i := iNew

(* ruleTau: int -> unit
Function executing a tau step on a thread
Global variable: 
  - configs: hashtable matching the id of a thread to its config
Parameter:
  - id: identifier of the thread executing the tau step
*)
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

(* ruleSpawn: int -> (ast ref * state ref * frame Stack.t ref)
Function executing a spawn instruction on a thread
Global variable: 
  - configs: hashtable matching the id of a thread to its config
Parameter:
  - id: identifier of the thread executing the tau step
Return: the configuration of the new thread 
*)
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
        | _ -> raise (Unknown_error_main_interpretor "ruleSpawn"))
    | _ -> raise (Unknown_error_main_interpretor "ruleSpawn") 

(* prefix_to_action: (position * ast * prefixAction * string) -> state ref -> action
Function converting a prefix into an action
Parameter:
  - action: the action of the prefix
  - ch: name of the channel in the thread local state
  - s: the local state containing all the local variables of the thread
Return: the action corresponding to the prefix
*)
let prefix_to_action (_,_,action,ch) s =
  match action,ch with
    | Tau,_ -> TauAction
    | Send,Some(n) -> let id = ruleIdentifier n (!g,!s) in SendAction(unfold_chan id)
    | Receive,Some(n) -> let id = ruleIdentifier n (!g,!s) in ReceiveAction(unfold_chan id)
    | _ -> raise (Unknown_error_main_interpretor "prefix_to_actions")

(* check_possible_actions: int -> (ast ref * state ref * frame Stack.t ref) -> (int * action) list
Function checking what actions are possible next for a thread
Parameter:
  - id: the identifier of the thread
  - i: the next instruction to execute for this thread
  - s: the local state containing all the local variables of the thread
Return: list of pairs containing the id of the thread and a possible action
*)
let check_possible_actions id (i,s,_) =
  match (!i) with
    | SendNode(_,n,_) -> let ch = ruleIdentifier n (!g,!s) in [(id,SendAction(unfold_chan ch))]
    | ReceiveNode(_,_,n) -> let ch = ruleIdentifier n (!g,!s) in [(id,ReceiveAction(unfold_chan ch))]
    | SpawnNode(_,_,_) -> [(id,SpawnAction)]
    (* In the case of a choose instruction, all the prefixes are possible actions *)
    | ChooseNode(_,c) ->
      let rec aux ast choices = 
      (match ast with
        | ChoicesNode(_,PrefixNode(pos,arg,act,ch),_,None) -> List.rev ((pos,arg,act,ch)::choices)
        | ChoicesNode(_,PrefixNode(pos,arg,act,ch),_,Some(cs)) -> aux cs ((pos,arg,act,ch)::choices)
        | _ -> raise (Unknown_error_main_interpretor "check_possible_actions")
      ) in 
      let list_acts = aux c [] in
      List.map (fun p -> (id,prefix_to_action p s)) list_acts
    (* In the case of terminated thread that returns nothing, we delete the thread (except for the main thread) *)
    | Terminated(None) -> if (id != 0) then remove (!configs) id else (); []
    (* In the case of terminated thread that returns nothing, we keep the thread *)
    | Terminated(_) -> []
    (* If the instruction is none of the above, it is beta reduction *)
    | _ -> [(id,BetaAction)]


(* find_com_pair: int -> int -> (int * int) list -> step list
Function checking in a list of receiving threads what communications are possible for a sending thread 
Parameter:
  - ids: the identifier of the sending thread
  - chans: the identifier of the channel on which the sender wants to communicate
  - lreceivers: list of pairs with the id of a receiving thread and id of the channel
Return: the list of possible commumication steps
*)
let find_com_pair ids chans lreceivers =
  let l = List.find_all (fun (idr,chanr) -> (ids != idr) && (chans == chanr)) lreceivers in
  List.map (fun (idr,_) -> ComStep(chans,ids,idr)) l

(* find_next_steps: (int * int) list -> step list
Function checking what next steps are possible 
Parameter:
  - actions: list of pairs containing the id of a thread and a possible action for this thread 
Return: the list of possible commumication steps
*)
let find_next_steps actions =
  (* Collection of all the steps that are not commumication steps
     and collection of the sending and receiving threads *)  
  let rec aux list_act steps senders receivers =
    match list_act with
      | (id,BetaAction)::q -> aux q ((BetaStep(id))::steps) senders receivers
      | (id,TauAction)::q -> aux q ((TauStep(id))::steps) senders receivers
      | (id,SpawnAction)::q -> aux q ((SpawnStep(id))::steps) senders receivers
      | (id,SendAction(chan))::q -> aux q steps ((id,chan)::senders) receivers 
      | (id,ReceiveAction(chan))::q -> aux q steps senders ((id,chan)::receivers)
      | [] -> (steps,senders,receivers)
  in let (steps,senders,receivers) = aux actions [] [] [] in
  (* Collection of the communication steps *)
  List.fold_left (fun l (ids,chans) -> l@(find_com_pair ids chans receivers)) steps senders

(* exec_beta_steps: int -> unit
Function executing all the beta steps possible inside a thread
Global variable: 
  - configs: hashtable matching the id of a thread to its config
Parameter:
  - id: identifier of the thread executing the beta steps
*)
let exec_beta_steps id =
  let (i,s,e) = find (!configs) id in
  (* Loop executing all the beta steps *)
  let rec aux instr = 
    let frame = if (is_empty (!e)) then None else Some(Stack.top (!e)) in
    match instr with
      (* If the instruction is not a beta reduction, the loop stops *)
      | SendNode(_,_,_) | ReceiveNode(_,_,_) | ChooseNode(_,_) | SpawnNode(_,_,_) | Terminated(_) -> i := instr
      (* Else, the beta step is executed and the function loops again *)
      | _ -> aux (exec_beta_step instr frame s e)
  in aux (!i) 

(* exec_step: execStatus
Function executing a possible step
Global variable: 
  - configs: hashtable matching the id of a thread to its config
  - nbThreads: number of created threads since the start of the execution 
  - vb2: verbose parameter
Return: the status of the execution
*)
let exec_step () =
  (* Collection of all the possible actions for each thread *)
  let all_actions = fold (fun id c l -> (check_possible_actions id c)@l) (!configs) [] in
  (* Search of all the possible steps according to the actions *)
  let all_steps = find_next_steps all_actions in
  let size = List.length all_steps in
  (* If there are available steps, the program randomly choose one and execute it *)
  if (size > 0) then
    (* Generation of a random number *)
    let n = (Random.bits ()) mod size in
    (* Extraction of the step corresponding to this number *)
    let step = List.nth all_steps n in
    (* Execution of the chosen step *)
    (match step with
      | ComStep (ch,idsender,idreceiver) -> let v = ruleSend ch idsender in ruleReceive ch idreceiver v
      | BetaStep(id) -> exec_beta_steps id
      | TauStep(id) -> ruleTau id
      | SpawnStep(id) -> let newc = ruleSpawn id in add (!configs) (!nbThreads) newc; 
                         nbThreads := (!nbThreads) + 1);
    if (!vb2) then print_exec_step all_steps step else ();
    Running
  (* If there is no available step, the execution is finished or deadlocked *)
  else
    (if (!vb2) then print_string("Possible Steps: -- \nChosen Step: -- \n") else ();
    Executed)

(* init_prg: ast -> envType -> ast -> int -> int -> int -> unit
Function intializing all the global variables necessary for the execution
Global variables:
  - vb1, vb2, vb3: verbose parameters
  - maxstep: maximum number of steps
  - envType: the type environment
  - g: the global state containing all the global variables of the execution
  - nbSteps: number of executed steps
  - configs: hashtable matching the id of a thread to its config
  - nbThreads: number of created threads since the start of the execution
Paraneters:
  - ast: abstract syntax tree representing the program
  - env_type: type environment from the resolve stage
  - start: abstract syntax tree representing the main call
  - verbosity: the amount of verbosity
  - seed: the seed to initialize the number generator
  - maxstep: the maximum number of steps 
*)
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

(* run_prg: unit
Function executing the program
Global variables:
  - vb3: verbose parameter
  - nbSteps: number of executed steps
*)
let rec run_prg () =
  if (!vb3) then print_configs () else ();
  nbSteps := (!nbSteps) + 1;
  let status = exec_step () in
  if ((status == Running) && (!nbSteps <= !max)) then
    run_prg ()
  else 
    results_to_string ()