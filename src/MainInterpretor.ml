open Ast
open ExpressionInterpretor
open BetaRedInterpretor
open Stack
open Hashtbl
open Random

type actions =
  | BetaAction
  | TauAction
  | SendAction of int
  | ReceiveAction of int
  | SpawnAction

type steps =
  | ComStep of int * int * int
  | BetaStep of int
  | TauStep of int
  | SpawnStep of int

type execStatus =
  | Running
  | Executed

let configs = ref([])

let nbSteps = ref(0)

let vb1 = ref(false)
let vb2 = ref(false)
let vb3 = ref(false)

let max = ref(10000)

let unfold_chan ch =
  match ch with 
    | ChannelVal(i) -> i
    | _ -> raise (Unknown_error_reference_interpretor "not a channel in unfold_chan")

let ruleSend ch id =
  let (i,s,_) = List.nth (!configs) id in
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
        | _ -> raise (Unknown_error_reference_interpretor "ruleSend2"))
      in aux c
    | _ -> raise (Unknown_error_reference_interpretor "ruleSend1")
  ) in 
  i := iNew;
  value_of_expr expr (!g,!s)

exception Unk of ast

let ruleReceive ch id v =
  let (i,s,_) = List.nth (!configs) id in
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
        | _ -> raise (Unknown_error_reference_interpretor "ruleReceive2"))
      in aux c
    | _ -> raise (Unk (!i))
  ) in 
  let pa = find (!s) assign in
  pa := v;
  i := iNew

let ruleTau id =
  let (i,_,_) = List.nth (!configs) id in
  match (!i) with
    | ChooseNode(_,c) ->
      let rec aux ast = 
      (match ast with
        | ChoicesNode(_,PrefixNode(_,_,Tau,_),ipref,_) -> i := ipref
        | ChoicesNode(_,_,_,Some(cs)) -> aux cs 
        | _ -> raise (Unknown_error_reference_interpretor "ruleTau2"))
      in aux c
    | _ -> raise (Unknown_error_reference_interpretor "ruleTau1")

let ruleSpawn id =
  let (i,s,_) = List.nth (!configs) id in
  match (!i) with
    | SpawnNode(_,fname,expr) -> 
      let f = !(Hashtbl.find !g fname) in
      let ve = value_of_expr expr (!g,!s) in
      (match f with 
        | FuncVal(param,BodyNode(_,decla,instr)) -> 
          i := NoopNode;
          (ref(instr),ref(create_state param decla ve),ref(Stack.create ()))
        | _ -> raise (Unknown_error_reference_interpretor "ruleSpawn2"))
    | _ -> raise (Unknown_error_reference_interpretor "ruleSpawn1") 
     
let prefix_to_action (_,_,action,ch) s =
  match action,ch with
    | Tau,_ -> TauAction
    | Send,Some(n) -> let id = ruleIdentifier n (!g,!s) in SendAction(unfold_chan id)
    | Receive,Some(n) -> let id = ruleIdentifier n (!g,!s) in ReceiveAction(unfold_chan id)
    | _ -> raise (Unknown_error_reference_interpretor "prefix_to_actions")

let check_possible_actions (i,s,_) =
  match (!i) with
    | SendNode(_,n,_) -> let ch = ruleIdentifier n (!g,!s) in [SendAction(unfold_chan ch)]
    | ReceiveNode(_,_,n) -> let ch = ruleIdentifier n (!g,!s) in [ReceiveAction(unfold_chan ch)]
    | SpawnNode(_,_,_) -> [SpawnAction]
    | ChooseNode(_,c) ->
      let rec aux ast choices = 
      (match ast with
        | ChoicesNode(_,PrefixNode(pos,arg,act,ch),_,None) -> List.rev ((pos,arg,act,ch)::choices)
        | ChoicesNode(_,PrefixNode(pos,arg,act,ch),_,Some(cs)) -> aux cs ((pos,arg,act,ch)::choices)
        | _ -> raise (Unknown_error_reference_interpretor "check_possible_actions")
      ) in 
      let list_acts = aux c [] in
      List.map (fun p -> prefix_to_action p s) list_acts
    | Terminated(_) -> []
    | _ -> [BetaAction]

let find_receive chan a idsender = 
  match a with
    | (id,ReceiveAction(ch)) -> (chan == ch) && (id != idsender)
    | _ -> false

let find_send chan a idreceiver = 
  match a with
    | (id,SendAction(ch)) -> (chan == ch) && (id != idreceiver)
    | _ -> false

let find_next_steps actions =
  let flat_list_actions =  List.flatten (List.mapi (fun i e -> List.map (fun a -> (i,a)) e ) actions) in
  let rec aux list_act steps =  
    match list_act with
      | (id,BetaAction)::q -> aux q ((BetaStep(id))::steps)
      | (id,TauAction)::q -> aux q ((TauStep(id))::steps)
      | (id,SendAction(chan))::q -> aux q ((List.map (fun (idr,_) -> ComStep(chan,id,idr)) (List.find_all (fun a -> find_receive chan a id) q))@steps)  
      | (id,ReceiveAction(chan))::q -> aux q ((List.map (fun (ids,_) -> ComStep(chan,ids,id)) (List.find_all (fun a -> find_send chan a id) q))@steps)
      | (id,SpawnAction)::q -> aux q ((SpawnStep(id))::steps)
      | [] -> steps
  in aux flat_list_actions []

let exec_beta_steps id =
  let (i,s,e) = List.nth (!configs) id in
  let rec aux instr = 
    let frame = if (is_empty (!e)) then None else Some(Stack.top (!e)) in
    match instr with
      | SendNode(_,_,_) | ReceiveNode(_,_,_) | ChooseNode(_,_) | SpawnNode(_,_,_) | Terminated(_) -> i := instr
      | _ -> aux (exec_beta_step instr frame s e)
  in aux (!i)

let string_of_step step =
  match step with
  | ComStep(ch,ids,idr) -> "Com(ch: @" ^ (string_of_int ch) ^ ", s: " ^ (string_of_int ids) ^ ", r: " ^ (string_of_int idr) ^ ")" 
  | BetaStep(id) -> "Beta(" ^ (string_of_int id) ^ ")"
  | TauStep(id) -> "Tau(" ^ (string_of_int id) ^ ")"
  | SpawnStep(id) -> "Spawn(" ^ (string_of_int id) ^ ")"  

let print_exec_step all_steps step =
  let s = List.fold_left (fun string_vs e -> string_vs ^ (string_of_step e) ^ "; ") "" all_steps in
  print_string("Possible steps: " ^ s ^ "\n");
  print_string("Chosen step: " ^ (string_of_step step) ^ "\n") 

let exec_step ()=
  let all_actions = List.map (fun c -> check_possible_actions c) !configs in
  let all_steps = find_next_steps all_actions in
  let size = List.length all_steps in
  if (size > 0) then
    let n = (Random.bits ()) mod size in
    let step = List.nth all_steps n in
    (match step with
      | ComStep (ch,idsender,idreceiver) -> let v = ruleSend ch idsender in ruleReceive ch idreceiver v
      | BetaStep(id) -> exec_beta_steps id
      | TauStep(id) -> ruleTau id
      | SpawnStep(id) -> let newc = ruleSpawn id in configs := (!configs)@[newc]);
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
            configs := [(ref(instr),ref(create_state param decla ve),ref(Stack.create ()))] 
          | _ -> raise (Unknown_error_reference_interpretor "init_prg2"))
      | _ -> raise (Unknown_error_reference_interpretor "init_prg1")

let string_of_eval_context e =
  if (is_empty e) then "--"
  else match (top e) with
    | FuncCallReturnFrame(_,a) -> "(state, " ^ a ^ ") :: E" 
    | FuncCallVoidFrame(_) -> "(state) :: E"
    | InstrSeqFrame(i) -> "(" ^ (string_of_instr i) ^ ") :: E"

let string_of_state s =
  let seq = to_seq s in 
  let str = Seq.fold_left (fun string_vs (name,value) -> string_vs ^ name ^ " = " ^ (string_of_val (Some(!value))) ^ "; ") "" seq in
  if (String.equal str "") then "--" else str

let print_config nb (i,s,e) =
  print_string("  Thread " ^ (string_of_int nb) ^ ": \n");
  print_string("    Instruction: " ^ (string_of_instr (!i))^ "\n");
  print_string("    State: " ^ (string_of_state (!s)) ^ "\n");
  print_string("    Evaluation Context: " ^ (string_of_eval_context (!e)) ^ "\n\n")

let print_configs () =
    print_string("\nStep " ^ (string_of_int(!nbSteps)) ^ ":\n");
    List.iteri (fun i c -> print_config i c) (!configs)

let result_to_string i s = 
  match (!i) with
  | Terminated(None) -> string_of_val None
  | Terminated(Some(expr)) -> string_of_val (Some(value_of_expr expr (!g,!s)))
  | _ -> string_of_val (Some(Deadlock))

let results_to_string () =
  if (!vb1) then 
    let strs = List.mapi (fun nb (i,s,_) -> "    Thread " ^ (string_of_int nb) ^ ": "^ (result_to_string i s) ^ "\n")  (!configs) in
    List.fold_left (fun str e -> str ^ e) "\n" strs
  else let (i,s,_) = List.hd (!configs) in (result_to_string i s) ^ "\n"

let rec run_prg () =
  if (!vb3) then print_configs () else ();
  nbSteps := (!nbSteps) + 1;
  let status = exec_step () in
  if ((status == Running) && (!nbSteps <= !max)) then
    run_prg ()
  else 
    results_to_string ()