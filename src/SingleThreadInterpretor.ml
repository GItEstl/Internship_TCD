open Ast
open ExpressionInterpretor
open BetaRedInterpretor
open Stack
open Hashtbl

let ruleFinalValue ast =
  match ast with
    | ReturnNode (_, None) -> (NoopNode,1,None)
    | ReturnNode (_,Some(expr)) -> (NoopNode,1,Some(value_of_expr expr (!g,!s)))
    | _ -> raise (Unknown_error_reference_interpretor "ruleFinalValue")

let ruleSend ast = 
  match ast with
    | SendNode(_,n,expr) -> 
      let ve = value_of_expr expr (!g,!s) in
      let ch = ruleIdentifier n (!g,!s) in
      let id = (match ch with 
        | ChannelVal(i) -> i
        | _ -> raise (Unknown_error_reference_interpretor "ruleReceive2")) in
      (NoopNode,id,Some(ve))
    | _ -> raise (Unknown_error_reference_interpretor "ruleSend")

let ruleReceive ast = 
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

let ruleChoice ast =
  match ast with 
    | (PrefixNode(pos,None,Tau,None),i) -> (i,-1,None)
    | (PrefixNode(pos,Some(expr),Send,Some(n)),i) -> push (InstrSeqFrame(i)) (!e); ((SendNode(pos,n,expr)),-1,None) 
    | (PrefixNode(pos,Some(a),Receive,Some(n)),i) -> push (InstrSeqFrame(i)) (!e); ((ReceiveNode(pos,a,n)),-1,None)
    | _ -> raise (Unknown_error_reference_interpretor "ruleChoice")

let ruleChoose ast =
  match ast with 
    | ChooseNode(pos,c) ->
      let rec aux ast choices = 
      (match ast with
        | ChoicesNode(_,p,i,None) -> List.rev ((p,i)::choices)
        | ChoicesNode(_,p,i,Some(cs)) -> aux cs ((p,i)::choices)
        | _ -> raise (Unknown_error_reference_interpretor "ruleChoose2")) in
      let choiceList = aux c [] in
      let size = List.length choiceList in
      print_string ("Choose instruction line " ^ (string_of_int pos.pos_lnum) ^ ": Please select a number between 0 and " ^ (string_of_int (size - 1)) ^"! \n");
      let n = read_int () in
      let choice = List.nth choiceList n in
      ruleChoice choice   
    | _ -> raise (Unknown_error_reference_interpretor "ruleChoose1")

let rec exec_step ast =
  let (i,outchan,vout) = (
    match ast with
      | SendNode(_,_,_) -> ruleSend ast
      | ReceiveNode(_,_,_) -> ruleReceive ast
      | ChooseNode(_,_) -> ruleChoose ast
      | ReturnNode (_,_) -> if (is_empty (!e)) then ruleFinalValue ast else ((exec_beta_step ast (Some(Stack.top (!e)))),-1,None)
      | _ -> if (is_empty (!e)) then ((exec_beta_step ast None),-1,None) else ((exec_beta_step ast (Some(Stack.top (!e)))),-1,None)  
  ) in
  match outchan with
    | 1 -> vout
    | -1 -> exec_step i
    | _ -> print_string("Communication output on channel " ^ (string_of_int outchan) ^ ": " ^ (string_of_val vout) ^ "\n"); exec_step i

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
            exec_step instr
          | _ -> raise (Unknown_error_reference_interpretor "run_prg2"))
      | _ -> raise (Unknown_error_reference_interpretor "run_prg1")