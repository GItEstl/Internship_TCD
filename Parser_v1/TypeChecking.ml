open Ast
open Resolve

type typeType =
  | IntegerType
  | BooleanType
  | StringType
  | CharType
  | VoidType
  | ChannelType of typeType
  | ChannelGenType
  | ListType of typeType
  | ListGenType
  | TupleType of typeType list
  | TupleGenType
  | NamedType of string
  | ErrorType

type instrType = 
  | OK
  | OKt of typeType

exception Different_types_in_list
exception Impossible_unify_instr of typeType * typeType

exception Wrong_type of Lexing.position * typeType * typeType
exception Inconsistent_types of Lexing.position * typeType * typeType
exception Unknown_variable of Lexing.position * string
exception Unknown_function of Lexing.position * string
exception Not_type_of_the_params of Lexing.position * typeType * typeType * string
exception Not_type_of_the_return of Lexing.position * typeType * typeType * string
exception Assignement_of_void of Lexing.position * string
exception No_assignement_of_the_return of Lexing.position * typeType * string
exception Wrong_type_chan_receive of Lexing.position * typeType * typeType
exception Wrong_type_chan_send of Lexing.position * typeType * typeType
exception Different_type_of_return_if of Lexing.position * typeType * typeType
exception Return_not_match_with_decla of Lexing.position * typeType * typeType * string
exception Different_type_of_return_func of Lexing.position * string
exception Illegal_type_argument of Lexing.position
exception Unknown_error_type_checking of string

let rec string_of_type t =
  match t with
    | IntegerType -> "integer"
    | BooleanType -> "boolean"
    | StringType -> "string"
    | CharType -> "char"
    | VoidType -> "void"
    | ChannelType(st) -> "channel " ^ (string_of_type st)
    | ChannelGenType -> "channel"
    | ListType(st) -> "list" ^ (string_of_type st)
    | ListGenType -> "list"
    | TupleType(st) -> "(" ^ (let rec aux l = match l with 
                                      | [] -> ""
                                      | t::q -> (string_of_type t) ^ ", " ^ (aux q)
                        in aux st) ^ ")"  
    | TupleGenType -> "tuple"
    | NamedType(s) -> s 
    | ErrorType -> "ERROR"

let rec type_of_tree_aux tree envType =
  match tree with
    | TypeNode (_,IntegerT) -> IntegerType
    | TypeNode (_,BooleanT) -> BooleanType
    | TypeNode (_,StringT) -> StringType
    | TypeNode (_,CharT) -> CharType
    | ChanTNode (_,st) -> ChannelType (type_of_tree_aux st envType) 
    | ListTNode (_,st) -> ListType (type_of_tree_aux st envType)
    | TupleTNode (_,TypeSeqNode(st,None)) -> type_of_tree_aux st envType
    | TupleTNode (_,tSeq) -> TupleType (
      let rec aux t l = match t with
        | TypeSeqNode (st,None) -> List.rev ((type_of_tree_aux st envType)::l) 
        | TypeSeqNode (st1, Some(st2)) -> aux st2 ((type_of_tree_aux st1 envType)::l)
        | _ -> raise (Unknown_error_type_checking "type_of_tree")
      in aux tSeq [])
    | NamedTypeNode (_,st) -> let _,_,t,tRec = (List.find (fun (_,name,_,_) -> String.equal name st) envType) in if (tRec) then NamedType(st) else (type_of_tree_aux t envType)
    | FuncTNode (None) -> VoidType
    | FuncTNode (Some(t)) -> type_of_tree_aux t envType        
    | _ -> raise (Unknown_error_type_checking "type_of_tree")

let type_of_tree tree envType = 
  match tree with 
    | NamedTypeNode(_,st) -> let _,_,t,_ = (List.find (fun (_,name,_,_) -> String.equal name st) envType) in (type_of_tree_aux t envType)
    | _ -> type_of_tree_aux tree envType

let type_of_var namevar envVar envType pos =
  try 
    let _,tnode,_,_ = List.find (fun (_,_,name,p) -> (String.equal name namevar) && (p == None)) envVar in
    type_of_tree tnode envType
  with
    | Not_found -> raise (Unknown_variable (pos,namevar))

let type_of_func namef envVar envType pos =
  try 
    let _,tnode,_,pnode = List.find (fun (_,_,name,p) -> (String.equal name namef) && (p != None)) envVar in
    (type_of_tree tnode envType,pnode)
  with
    | Not_found -> raise (Unknown_variable (pos,namef))

let rec compare envType type1 type2 =
  match (type1,type2) with
    | (IntegerType,IntegerType) -> (true,IntegerType)
    | (BooleanType,BooleanType) -> (true,BooleanType)
    | (StringType,StringType) -> (true,StringType)
    | (CharType,CharType) -> (true,CharType)
    | (ChannelType(st1),ChannelType(st2)) -> if (fst(compare envType st1 st2)) then (true,ChannelType(st2)) else (false,ErrorType)
    | (ChannelType(st),ChannelGenType) -> (true,ChannelType(st))
    | (ListType(st1),ListType(st2)) -> if (fst(compare envType st1 st2)) then (true,ListType(st2)) else (false,ErrorType)
    | (ListType(st),ListGenType) -> (true,ListType(st))
    | (TupleType(st1),TupleType(st2)) -> if (List.for_all2 (fun a b -> fst(compare envType a b)) st1 st2) then (true,TupleType(st2)) else (false,ErrorType)
    | (TupleType(st),TupleGenType) -> (true,TupleType(st))
    | (NamedType(n1),NamedType(n2)) -> if (String.equal n1 n2) then (true,NamedType(n2)) else (false,ErrorType)
    | (NamedType(st),t) | (t,NamedType(st)) -> let _,_,tnode,_ = (List.find (fun (_,name,_,_) -> String.equal name st) envType) in compare envType (type_of_tree tnode envType) t
    | (VoidType,VoidType) -> (true,VoidType)
    | _ -> (false,ErrorType)

let unify_instr ti1 ti2 envType = 
  match (ti2,ti1) with
    | (OK,t) -> t
    | (t,OK) -> t
    | (OKt(t1),OKt(t2)) ->
    try
      let b,t = compare envType t1 t2 in
      if (b) then OKt(t) else raise (Impossible_unify_instr (t1,t2))
    with 
      | Invalid_argument _ -> raise (Impossible_unify_instr (t1,t2))

(* Expression *)

let rec type_of_expr expr envVar envType =
  match expr with
    | BinaryNode (pos,left,op,right) -> ruleBinary op left right envVar envType pos
    | UnaryNode (pos,op,expr) -> ruleUnary op expr envVar envType pos
    | IfthenelseExprNode (pos,cond,e1,e2) -> ruleIfThenElseExpr cond e1 e2 envVar envType pos
    | ExprNode (_,e) -> type_of_expr e envVar envType
    | ExprsNode (e,None) -> type_of_expr e envVar envType
    | ExprsNode (e1,Some(e2)) -> ruleTupleExpr (ExprsNode (e1,Some(e2))) envVar envType
    | ValueNode(ValueNode (v)) -> ruleValue v envType
    | AssignNode (pos,a) -> ruleAssignable a envVar envType pos
    | _ -> raise (Unknown_error_type_checking "type_of_expr")

and

ruleUnary op expr envVar envType pos = 
  try
    let texpr = (type_of_expr expr envVar envType) in
    (match op with
    | (NegateInt | Odd | Even)->
      let be,_ = compare envType texpr IntegerType in
      (if (be) then IntegerType else raise (Wrong_type (pos,texpr,IntegerType)))
    | NegateBool ->
      let be,_ = compare envType texpr BooleanType in
      (if (be) then IntegerType else raise (Wrong_type (pos,texpr,IntegerType)))
    | Head ->
      let _,te = compare envType texpr ListGenType in
      (match te with
        | ListType(st) -> st
        | _ -> raise (Wrong_type (pos,texpr,ListGenType))) 
    | Tail ->
      let _,te = compare envType texpr ListGenType in
      (match te with
      | ListType(_) -> te
      | _ -> raise (Wrong_type (pos,texpr,ListGenType))) 
    | Fst ->
      let _,te = compare envType texpr TupleGenType in
      (match te with
      | TupleType(st) -> List.hd st
      | _ -> raise (Wrong_type (pos,texpr,TupleGenType))) 
    | Snd ->
      let _,te = compare envType texpr TupleGenType in
      (match te with
      | TupleType(st) -> List.nth st 1
      | _ -> raise (Wrong_type (pos,texpr,TupleGenType))) 
    )
  with
    | Invalid_argument _ -> raise (Illegal_type_argument pos)
and 

ruleBinary op left right envVar envType pos =
  try
    let tleft = (type_of_expr left envVar envType) in
    let tright = (type_of_expr right envVar envType) in
      (match op with
      | (Equal | Different) -> 
        let (b,t) = compare envType tleft tright in
          (match t with 
            | IntegerType | BooleanType | CharType | StringType -> BooleanType
            | ErrorType -> raise (Wrong_type (pos,tright,tleft))
            | _ -> raise (Illegal_type_argument pos))
      | (Lesser | Greater) ->
        let (bl,tl) = compare envType tleft IntegerType in
        let (br,tr) = compare envType tright IntegerType in
          (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tr,IntegerType))) 
          else raise (Wrong_type (pos,tl,IntegerType)))
      | (Add | Substract | Multiply | Divide) ->
        let (bl,tl) = compare envType tleft IntegerType in
        let (br,tr) = compare envType tright IntegerType in
          (if (bl) then (if (br) then IntegerType else raise (Wrong_type (pos,tr,IntegerType))) 
          else raise (Wrong_type (pos,tl,IntegerType)))
      | (Or | And) ->
        let (bl,tl) = compare envType tleft BooleanType in
        let (br,tr) = compare envType tright BooleanType in
          (if (bl) then (if (br) then BooleanType else raise (Wrong_type (pos,tr,IntegerType))) 
          else raise (Wrong_type (pos,tl,IntegerType)))
      | _ -> raise (Unknown_error_type_checking "ruleBinary"))
  with 
    | Invalid_argument _ -> raise (Illegal_type_argument pos)

and

ruleIfThenElseExpr cond_expr then_expr else_expr envVar envType pos =
  let tcond_expr = (type_of_expr cond_expr envVar envType) in
  let tthen_expr = (type_of_expr then_expr envVar envType) in
  let telse_expr = (type_of_expr else_expr envVar envType) in
  let bc,_ = compare envType tcond_expr BooleanType in
  let be,te = compare envType tthen_expr telse_expr in
    (if (bc) then (if (be) then te else raise (Inconsistent_types (pos,tthen_expr,telse_expr))) 
    else raise (Wrong_type (pos,te,BooleanType)))

and

ruleTupleExpr exprs envVar envType =
  TupleType (
  let rec aux exs l =
    (match exs with
      | ExprsNode (e,None) -> List.rev ((type_of_expr e envVar envType)::l)
      | ExprsNode (e1,Some(e2)) -> aux e2 ((type_of_expr e1 envVar envType)::l)
      | _ -> raise (Unknown_error_type_checking ("ruleTupleExpr")))
  in aux exprs [])

and

ruleValue v envType =
  match v with
    | IntegerNode (_,_) -> IntegerType
    | CharNode (_,_) -> CharType
    | StringNode (_,_) -> StringType
    | TrueNode(_) -> BooleanType
    | FalseNode(_) -> BooleanType
    | ValueSeqNode (v,None) -> ListType (ruleValue v envType)
    | ValueSeqNode (v,Some(vs)) -> ListType (
        let rec aux v1 vs1 =
          (match vs1 with
            | ValueSeqNode (v2,None) -> 
              let bv, tv = compare envType (ruleValue v1 envType) (ruleValue v2 envType) in
                if (bv) then tv else raise (Different_types_in_list)
            | ValueSeqNode (v2,Some(vs2)) -> 
              let bv,_ = compare envType (ruleValue v1 envType) (ruleValue v2 envType) in
              if (bv) then (aux v2 vs2) else raise (Different_types_in_list)
            | _ -> raise (Unknown_error_type_checking ("ruleValue1")))
        in aux v vs) 
    | _ -> raise (Unknown_error_type_checking "ruleValue")

and

ruleAssignable a envVar envType pos = type_of_var a envVar envType pos

(* Instruction *)

let rec ruleInstr i  envVar envType =
  let rec aux i tInstr =
    match i with
      | InstrSeqNode(NoopNode,None) -> tInstr 
      | InstrSeqNode(NoopNode,Some(i)) -> aux i tInstr
      | InstrSeqNode (bi,None) -> unify_instr (type_of_binstr bi envVar envType) tInstr envType
      | InstrSeqNode (bi,Some(i)) -> aux i (unify_instr (type_of_binstr bi envVar envType) tInstr envType)
      | _ -> raise (Unknown_error_type_checking ("ruleInstr"))
  in aux i OK 
    
and

type_of_binstr binstr envVar envType =
  match binstr with
    | BinaryNode (pos,a,Assign,CallNode (posf,namef,e)) -> ruleCallFuncWithReturn a namef e envVar envType pos posf
    | BinaryNode (pos,a,Assign,e) -> ruleAssignInstr a e envVar envType pos
    | CallNode (pos,namef,e) -> ruleCallFuncVoid namef e envVar envType pos
    | ReceiveNode (pos,a,namechan) -> ruleReceive a namechan envVar envType pos
    | SendNode (pos,namechan,e) -> ruleSend namechan e envVar envType pos
    | IfthenelseInstrNode (pos,cond,i1,i2) -> ruleIfThenElseInstr cond i1 i2 envVar envType pos
    | WhileNode (pos,e,i) -> ruleWhile e i envVar envType pos
    | ChooseNode (_,c) -> ruleChoose c envVar envType
    | SpawnNode (pos,namef,e) -> ruleSpawn namef e envVar envType pos
    | NewNode (pos,a) -> ruleNew a envVar envType pos
    | ReturnNode (_,None) -> ruleReturnVoid
    | ReturnNode (pos,Some (CallNode (_,namef,e))) -> ruleReturnFunc namef e envVar envType pos
    | ReturnNode (_,Some (e)) -> ruleReturnExpr e envVar envType
    | _ -> raise (Unknown_error_type_checking ("type_of_binstr"))

and

type_of_params paramNode envType =
  match paramNode with 
    | Some(ParamsNode (_,t,_,None)) -> type_of_tree t envType
    | Some(ParamsNode (_,_,_,Some(_))) -> TupleType( 
      (let rec aux params l =
        match params with
          | Some(ParamsNode (_,t,_,None)) -> List.rev ((type_of_tree t envType)::l)
          | Some(ParamsNode (_,t,_,ps)) -> aux ps ((type_of_tree t envType)::l)
          | _ -> raise (Unknown_error_type_checking ("type_of_params"))        
      in aux paramNode []))
    | _ -> raise (Unknown_error_type_checking ("type_of_params"))

and

ruleAssignInstr assign expr envVar envType pos =
  let tassign = (type_of_expr assign envVar envType) in
  let texpr = (type_of_expr expr envVar envType) in
  try
    let be,_ = compare envType tassign texpr in
    if (be) then OK else raise (Wrong_type (pos,texpr,tassign))
  with 
    | Invalid_argument _ -> raise (Wrong_type (pos,texpr,tassign))

and

ruleCallFuncWithReturn a namef e envVar envType pos posf =
  let tf,pnode = type_of_func namef envVar envType posf in
  if (tf != VoidType) then
    let ta = type_of_expr a envVar envType in
    try
      let br,_ = compare envType ta tf in
      if (br) then
        let tp = type_of_params pnode envType in
        let te = type_of_expr e envVar envType in
        (try
          let b,_ = compare envType tp te in
          if (b) then OK else raise (Not_type_of_the_params (pos,te,tp,namef)) 
        with 
          | Invalid_argument _ -> raise (Not_type_of_the_params (pos,te,tp,namef)))
      else raise (Not_type_of_the_return (pos,ta,tf,namef))
    with
      | Invalid_argument _ -> raise (Not_type_of_the_return (pos,ta,tf,namef))
  else raise (Assignement_of_void (pos,namef))

and

ruleCallFuncVoid namef e envVar envType pos =
  let tf,pnode = type_of_func namef envVar envType pos in
  let br,_ = compare envType tf VoidType in
    if (br) then
      let tp = type_of_params pnode envType in
      let te = type_of_expr e envVar envType in
      try
        let b,_ = compare envType tp te in
        if (b) then OK else raise (Not_type_of_the_params (pos,te,tp,namef))
      with
        | Invalid_argument _ -> raise (No_assignement_of_the_return (pos,tf,namef))
    else raise (No_assignement_of_the_return (pos,tf,namef))

and

ruleReceive a namechan envVar envType pos = 
  let ta = type_of_expr a envVar envType in
  let tchan = type_of_var namechan envVar envType pos in
  (match tchan with
    | ChannelType(st) -> (try 
                          let b,_ = compare envType (ChannelType(ta)) tchan in 
                          if (b) then OK else raise (Wrong_type_chan_receive (pos,ta,st))
                        with
                          | Invalid_argument _ -> raise (Wrong_type_chan_receive (pos,ta,st))) 
    | _ -> raise (Wrong_type (pos,tchan,ChannelGenType)))

and

ruleSend namechan e envVar envType pos = 
  let te = type_of_expr e envVar envType in
  let tchan = type_of_var namechan envVar envType pos in
  (match tchan with
    | ChannelType(st) -> (try 
                          let b,_ = compare envType (ChannelType(te)) tchan in 
                          if (b) then OK else raise (Wrong_type_chan_send (pos,te,st))
                        with
                          | Invalid_argument _ -> raise (Wrong_type_chan_send (pos,te,st))) 
    | _ -> raise (Wrong_type (pos,tchan,ChannelGenType)))

and

ruleIfThenElseInstr cond_expr then_instr else_instr envVar envType pos =
  let tcond_expr = (type_of_expr cond_expr envVar envType) in
  let tthen_instr = (ruleInstr then_instr envVar envType) in
  let telse_instr = (ruleInstr else_instr envVar envType) in
  try
    let be,te = compare envType tcond_expr BooleanType in
    if (be) then unify_instr tthen_instr telse_instr envType
    else raise (Wrong_type (pos,te,BooleanType))
  with
    | Impossible_unify_instr (t1,t2) -> raise (Different_type_of_return_if (pos,t1,t2))
    | Invalid_argument _ -> raise (Illegal_type_argument pos)

and

ruleWhile cond_expr i envVar envType pos =
  let tcond = (type_of_expr cond_expr envVar envType) in
  let ti = (ruleInstr i envVar envType) in
  try
    let be,te = compare envType tcond BooleanType in 
      if (be) then ti
      else raise (Wrong_type (pos,te,BooleanType))
  with
    | Invalid_argument _ -> raise (Illegal_type_argument pos)

and

rulePrefix p envVar envType = 
    match p with 
      | PrefixNode(_,_,Tau,_,_) -> OK
      | PrefixNode(pos,_,Send,Some(namechan),Some(e)) -> ruleSend namechan e envVar envType pos
      | PrefixNode(pos,Some(a),Receive,Some(namechan),_) -> ruleReceive a namechan envVar envType pos
      | PrefixNode(pos,Some(a),New,_,_) -> ruleNew a envVar envType pos
      | PrefixNode(pos,_,Spawn,Some(namef),Some(e)) -> ruleSpawn namef e envVar envType pos
      | _ -> raise (Unknown_error_type_checking ("rulePrefix"))

and

ruleChoose c envVar envType =
  let rec aux choice typeInstr = 
    match choice with 
      | ChoicesNode(_,p,i,None) -> let _ = rulePrefix p envVar envType in unify_instr (ruleInstr i envVar envType) typeInstr envType
      | ChoicesNode(_,p,i,Some(cs)) -> let _ = rulePrefix p envVar envType in aux cs (unify_instr (ruleInstr i envVar envType) typeInstr envType) 
      | _ -> raise (Unknown_error_type_checking ("ruleChoose"))
  in aux c OK

and

ruleSpawn namef e envVar envType pos = ruleCallFuncVoid namef e envVar envType pos

and

ruleNew a envVar envType pos = 
  let ta = type_of_expr a envVar envType in
  try
    let bc,_ = compare envType ta ChannelGenType in
    if (bc) then OK else raise (Wrong_type (pos,ta,ChannelGenType))
  with
    | Invalid_argument _ -> raise (Wrong_type (pos,ta,ChannelGenType))

and

ruleReturnVoid = OK

and

ruleReturnExpr e envVar envType = OKt(type_of_expr e envVar envType)

and

ruleReturnFunc namef e envVar envType pos =
  let tf,pnode = type_of_func namef envVar envType pos in
  let tp = type_of_params pnode envType in
  let te = type_of_expr e envVar envType in
  try
    let b,_ = compare envType tp te in
    if (b) then OKt(tf) else raise (Not_type_of_the_params (pos,te,tp,namef))
  with
    | Invalid_argument _ -> raise (Not_type_of_the_params (pos,te,tp,namef))

(* Functions *)

let rec extend_envVar_with_params params envVar =
    match params with
      | ParamsNode (pos,t,name,None) -> (pos,t,name,None)::(List.filter (fun (_,_,namevar,_) -> not (String.equal name namevar)) envVar)
      | ParamsNode (pos,t,name,Some(p)) -> extend_envVar_with_params p ((pos,t,name,None)::(List.filter (fun (_,_,namevar,_) -> not (String.equal name namevar)) envVar))
      | _ -> raise (Unknown_error_type_checking ("extend_params"))

let extended_envVar_with_local_declas declas envVar nameListType =
  let addedDecla = (let rec aux d l = 
    match d with
      | VariableDeclasNode (VariableDeclaNode (pos,t,name),None) -> ((pos,t,name,None)::l)
      | VariableDeclasNode (VariableDeclaNode (pos,t,name),Some(vs)) -> aux vs ((pos,t,name,None)::l)
      | _ -> raise (Unknown_error_type_checking ("extend_ldecla"))
    in aux declas []) in
  let _ = well_formed_envVar addedDecla nameListType in
  let restricted_envVar = List.filter (fun (_,_,name,_) -> not (List.exists (fun (_,_,lname,_) -> String.equal name lname) addedDecla)) envVar in
  restricted_envVar @ addedDecla

let extend_envVar params b envVar nameListType = 
  let copy_envVar = envVar in
  let extended_envVar = extend_envVar_with_params params copy_envVar in
  match b with
    | BodyNode (_,None,_) -> extended_envVar
    | BodyNode (_,Some(declas),_) -> extended_envVar_with_local_declas declas extended_envVar nameListType
    | _ -> raise (Unknown_error_type_checking ("extend_envVar")) 

let ruleFunction f envVar envType nameListType =
  (match f with
    | FunctionNode (pos,ftnode,namef,params,b) ->
        let local_envVar = extend_envVar params b envVar nameListType in
        let ti = (match b with
          | BodyNode (posb,_,i) ->
            (try
              ruleInstr i local_envVar envType
            with
              | Impossible_unify_instr (_,_) -> raise (Different_type_of_return_func (posb,namef))) 
          | _ -> raise (Unknown_error_type_checking ("ruleFunction"))) in
        let ft = type_of_tree ftnode envType in
        (match ti with
          | OK -> (try
                    if (fst(compare envType ft VoidType)) then true 
                    else raise (Return_not_match_with_decla (pos,VoidType,ft,namef))
                  with
                    | Invalid_argument _ -> raise (Return_not_match_with_decla (pos,VoidType,ft,namef)))
          | OKt(rt) -> (try
                         if (fst(compare envType ft rt)) then true 
                         else raise (Return_not_match_with_decla (pos,rt,ft,namef))
                       with
                         | Invalid_argument _ -> raise (Return_not_match_with_decla (pos,rt,ft,namef))))
    | _ -> raise (Unknown_error_type_checking ("ruleFunction")))

(* Program *)

let type_check_prg envVar envType nameListType prg = 
  let all_func = List.rev (let rec aux l tree =
    match tree with
        | ProgramNode (p1,p2) -> aux (aux l p1) p2
        | FunctionNode (_,_,_,_,_) -> (tree::l)
        | _ -> l 
    in aux [] prg) in 
  List.for_all (fun f -> ruleFunction f envVar envType nameListType) all_func