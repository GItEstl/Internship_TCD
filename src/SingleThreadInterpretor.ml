open Ast
open ExpressionInterpretor
open BetaRedInterpretor
open Stack
open Hashtbl
open Random

let ruleFinalValue ast s =
  match ast with
    | ReturnNode (_, None) -> (NoopNode,1,None)
    | ReturnNode (_,Some(expr)) -> (NoopNode,1,Some(value_of_expr expr (!g,!s)))
    | _ -> raise (Unknown_error_reference_interpretor "ruleFinalValue")

let ruleSend ast s = 
  match ast with
    | SendNode(_,n,expr) -> 
      let ve = value_of_expr expr (!g,!s) in
      let ch = ruleIdentifier n (!g,!s) in
      let id = (match ch with 
        | ChannelVal(i) -> i
        | _ -> raise (Unknown_error_reference_interpretor "ruleReceive2")) in
      (NoopNode,id,Some(ve))
    | _ -> raise (Unknown_error_reference_interpretor "ruleSend")

let ruleReceive ast s = 
  match ast with
    | ReceiveNode(_,AssignNode(_,assign),n) ->
      let pa = find (!s) assign in  
      let ch = ruleIdentifier n (!g,!s) in
      let id = (match ch with 
        | ChannelVal(i) -> i
        | _ -> raise (Unknown_error_reference_interpretor "ruleReceive2")) in
      print_string("Communication input on channel " ^ (string_of_int id) ^ ": the value of the receiver has not changed (No implementated yet) \n");
      (NoopNode,-1,None)
    | _ -> raise (Unknown_error_reference_interpretor "ruleReceive2")

let ruleChoice ast e =
  match ast with 
    | (PrefixNode(_,None,Tau,None),i) -> push (InstrSeqFrame(i)) (!e); (NoopNode,-1,None)
    | (PrefixNode(pos,Some(expr),Send,Some(n)),i) -> push (InstrSeqFrame(i)) (!e); ((SendNode(pos,n,expr)),-1,None) 
    | (PrefixNode(pos,Some(a),Receive,Some(n)),i) -> push (InstrSeqFrame(i)) (!e); ((ReceiveNode(pos,a,n)),-1,None)
    | _ -> raise (Unknown_error_reference_interpretor "ruleChoice")

let ruleChoose ast e =
  match ast with 
    | ChooseNode(_,c) ->
      let rec aux ast choices = 
      (match ast with
        | ChoicesNode(_,p,i,None) -> List.rev ((p,i)::choices)
        | ChoicesNode(_,p,i,Some(cs)) -> aux cs ((p,i)::choices)
        | _ -> raise (Unknown_error_reference_interpretor "ruleChoose2")) in
      let choiceList = aux c [] in
      let size = List.length choiceList in
      let n = (Random.bits ()) mod size in
      let choice = List.nth choiceList n in
      ruleChoice choice e   
    | _ -> raise (Unknown_error_reference_interpretor "ruleChoose1")

let rec exec_step ast s e =
  let (i,outchan,vout) = (
    match ast with
      | SendNode(_,_,_) -> ruleSend ast s
      | ReceiveNode(_,_,_) -> ruleReceive ast s
      | ChooseNode(_,_) -> ruleChoose ast e
      | ReturnNode (_,_) -> if (is_empty (!e)) then ruleFinalValue ast s else ((exec_beta_step ast (Some(Stack.top (!e))) s e),-1,None)
      | _ -> if (is_empty (!e)) then ((exec_beta_step ast None s e),-1,None) else ((exec_beta_step ast (Some(Stack.top (!e))) s e),-1,None)  
  ) in
  match outchan with
    | 1 -> vout
    | -1 -> exec_step i s e
    | _ -> print_string("Communication output on channel " ^ (string_of_int outchan) ^ ": " ^ (string_of_val vout) ^ "\n"); exec_step i s e

let run_prg ast env_type start = 
  (* Assignment of the type environment to the global variable envType *)
  envType := env_type;
  g := Hashtbl.create 100;
  let s = ref(Hashtbl.create 100) in
  let e =  ref(Stack.create ()) in
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
            exec_step instr s e
          | _ -> raise (Unknown_error_reference_interpretor "run_prg2"))
      | _ -> raise (Unknown_error_reference_interpretor "run_prg1")


let init () =
  print_string("Do you want to initialise the execution with a seed? Y or N\n");
  let answer = read_line () in
  let seed = 
    (match answer with
      | "Y" | "y" -> print_string("Please enter the seed :"); read_int ()
      | "N" | "n" -> self_init (); bits ()
      | _ -> print_string("Wrong input! The seed has been chosen randomly \n"); self_init (); bits ()
    ) in Random.init seed;
  print_string("The program will be excuted with the seed " ^ (string_of_int seed) ^ "\n"); 