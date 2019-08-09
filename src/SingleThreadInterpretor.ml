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

let ruleReceive ch id v =
  let (i,s,_) = List.nth (!configs) id in
  let (assign,iNew) = (
  match (!i) with
    | ReceiveNode(_,AssignNode(_,name),_) -> (name,NoopNode)
    | ChooseNode(_,c) ->
      let rec aux ast = 
      (match ast with
        | ChoicesNode(_,PrefixNode(_,Some(AssignNode(_,name)),Receive,Some(n)),ipref,Some(cs)) ->
          let idch = unfold_chan (ruleIdentifier n (!g,!s)) in  
          if (idch == ch) then (name,ipref) else aux cs
        | ChoicesNode(_,_,_,Some(cs)) -> aux cs 
        | _ -> raise (Unknown_error_reference_interpretor "ruleReceive2"))
      in aux c
    | _ -> raise (Unknown_error_reference_interpretor "ruleReceive1")
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

let find_next_steps actions =
  let flat_list_actions =  List.flatten (List.map (fun e -> List.mapi (fun i a -> (i,a)) e ) actions) in
  let rec aux list_act steps =  
    match list_act with
      | (id,BetaAction)::q -> aux q ((BetaStep(id))::steps)
      | (id,TauAction)::q -> aux q ((TauStep(id))::steps)
      | (id,SendAction(chan))::q -> aux q ((List.map (fun (idr,_) -> ComStep(chan,id,idr)) (List.find_all (fun (_,a) -> a == ReceiveAction(chan)) q))@steps)  
      | (id,ReceiveAction(chan))::q -> aux q ((List.map (fun (ids,_) -> ComStep(chan,ids,id)) (List.find_all (fun (_,a) -> a == SendAction(chan)) q))@steps)
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
    
let exec_step () =
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
    Running
  else
    Executed

let init_seed () =
  print_string("Do you want to initialise the execution with a seed? Y or N\n");
  let answer = read_line () in
  let seed = 
    (match answer with
      | "Y" | "y" -> print_string("Please enter the seed :"); read_int ()
      | "N" | "n" -> self_init (); bits ()
      | _ -> print_string("Wrong input! The seed has been chosen randomly \n"); self_init (); bits ()
    ) in Random.init seed;
  print_string("The program will be excuted with the seed " ^ (string_of_int seed) ^ "\n")

let init_prg ast env_type start = 
  (* Assignment of the type environment and creation of the global state *)
  envType := env_type;
  g := Hashtbl.create 100;
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

let rec run_prg () =
  let status = exec_step () in
  if (status == Running) then 
    run_prg ()
  else 
    let (i,s,_) = List.hd (!configs) in
    match (!i) with
      | Terminated(None) -> None
      | Terminated(Some(expr)) -> Some(value_of_expr expr (!g,!s))
      | _ -> Some(Deadlock)