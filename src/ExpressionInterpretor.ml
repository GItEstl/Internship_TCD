open Ast
open Channel

type valueType =
  | IntegerVal of int
  | BooleanVal of bool
  | StringVal of string
  | CharVal of string
  | ListVal of valueType list
  | TupleVal of valueType list
  | FuncVal of ast * ast
  | ChannelVal of channel
  | ErrorVal
  | Deadlock

exception Unknown_error_reference_interpretor of string
exception Run_time_error of string

(* string_of_val: valueType option -> string
Function converting a value into a string
Parameters:
  - v: the value to convert
Return: string representing the value
*)
let string_of_val v =
  match v with
    | None -> "unit"
    | Some(vs) ->
      let rec aux v =
        match v with
        | IntegerVal(i) -> string_of_int i 
        | BooleanVal(b) -> string_of_bool b
        | StringVal(s) -> "\"" ^ s ^ "\""
        | CharVal(c) -> "\'" ^ c ^ "\'"
        | ListVal([]) -> "[]"
        | ListVal(l) -> "[" ^ (List.fold_left (fun s e ->  s ^ ", " ^ (aux e)) (aux (List.nth l 0)) (List.tl l)) ^ "]"
        | TupleVal(t) -> "(" ^ (List.fold_left (fun s e ->  s ^ ", " ^ (aux e)) (aux (List.nth t 0)) (List.tl t)) ^ ")"
        | ChannelVal(id) -> "Chan@" ^ (string_of_int id)
        | Deadlock -> "Deadlock or maximum number of step reached"
        | _ -> "ERROR"
      in aux vs 

(* value_of_expr: (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an expression
Parameters:
  - expr: abstract syntax tree representing the expression
  - state: hash table representing the state of all the variables  
Return: a string corresponding to the expression
*)
let rec value_of_expr expr state =
  match expr with
    | BinaryNode (_,left,op,right) -> ruleBinary op left right state
    | UnaryNode (_,op,expr) -> ruleUnary op expr state
    | IfthenelseExprNode (_,cond,e1,e2) -> ruleIfThenElseExpr cond e1 e2 state
    | ExprNode (_,e) -> value_of_expr e state
    | ExprsNode (e,None) -> value_of_expr e state
    | ExprsNode (e1,Some(e2)) -> ruleTupleExpr (ExprsNode (e1,Some(e2))) state
    | ValueNode (v) -> ruleValue v
    | AssignNode (_,a) -> ruleIdentifier a state
    | _ -> raise (Unknown_error_reference_interpretor "value_of_expr")

and


(* ruleUnary: unary -> ast -> (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an unary operation
Parameters:
  - op: unary operator
  - expr: abstract syntax tree representing the expression
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the unary operation
*)
ruleUnary op expr state = 
  let v = value_of_expr expr state in
  (match v with
    | IntegerVal(i) -> 
      (match op with
        | NegateInt -> IntegerVal(- i)
        | Odd -> if ((i mod 2) == 1) then BooleanVal(true) else BooleanVal(false)
        | Even -> if ((i mod 2) == 0) then BooleanVal(true) else BooleanVal(false)
        | _ -> raise (Unknown_error_reference_interpretor "ruleUnary1")
      ) 
    | BooleanVal(b) ->
      (match op with
        | NegateBool -> BooleanVal((not b))
        | _ -> raise (Unknown_error_reference_interpretor "ruleUnary2")
      )
    | ListVal([]) -> 
      raise (Run_time_error "You cannot apply an operator to an empty list") 
    | ListVal(t::q) -> 
      (match op with
        | Head -> t
        | Tail -> ListVal(q)
        | _ -> raise (Unknown_error_reference_interpretor "ruleUnary3")
      )
    | TupleVal(l) -> 
      (match op with
        | Fst -> List.nth l 0
        | Snd -> List.nth l 1 
        | _ -> raise (Unknown_error_reference_interpretor "ruleUnary4")
      )
    | _ -> raise (Unknown_error_reference_interpretor "ruleUnary")
  )

and


(* ruleBinary: binary -> ast -> ast -> (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an binary operation
Parameters:
  - op: unary operator
  - lexpr: abstract syntax tree representing the left expression
  _ rexpr: abstract syntax tree representing the right expression
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the binary operation
*)
ruleBinary op lexpr rexpr state = 
  let vl = value_of_expr lexpr state in
  let vr = value_of_expr rexpr state in  
  (match (vl,vr) with
    | (IntegerVal(il),IntegerVal(ir)) -> 
      (match op with
        | Equal -> BooleanVal(il == ir)
        | Different -> BooleanVal(il != ir)
        | Lesser -> BooleanVal(il < ir)
        | Greater -> BooleanVal(il > ir)
        | Add -> IntegerVal(il + ir)
        | Substract -> IntegerVal(il - ir)
        | Multiply -> IntegerVal(il * ir)
        | Divide -> IntegerVal(il / ir)
        | _ -> raise (Unknown_error_reference_interpretor "ruleBinary1")
      )
    | (BooleanVal(bl),BooleanVal(br)) ->
      (match op with
        | Equal -> BooleanVal(bl == br)
        | Different -> BooleanVal(bl != br)
        | Or -> BooleanVal(bl || br)
        | And -> BooleanVal(bl && br)
        | _ -> raise (Unknown_error_reference_interpretor "ruleBinary2")
      ) 
    | ((StringVal(sl),StringVal(sr)) | (CharVal(sl),CharVal(sr)))-> 
      (match op with
        | Equal -> BooleanVal(String.equal sl sr)
        | Different -> BooleanVal(not (String.equal sl sr))
        | _ -> raise (Unknown_error_reference_interpretor "ruleBinary3")
      )
    | (IntegerVal(i),TupleVal(t)) -> List.nth t i
    | (ChannelVal(ch1),ChannelVal(ch2)) -> BooleanVal(Channel.equal ch1 ch2) 
    | _ -> raise (Unknown_error_reference_interpretor "ruleBinary")
  )

and 


(* ruleIfThenElseExpr: ast -> ast -> ast -> (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an binary operation
Parameters:
  - cond_expr: abstract syntax tree representing the expression for the condition
  - then_expr: abstract syntax tree representing the expression in the then branch
  - else_expr: abstract syntax tree representing the expression in the else branch
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the conditional expression
*)
ruleIfThenElseExpr cond_expr then_expr else_expr state =
  let vcond = value_of_expr cond_expr state in
  match vcond with
    | BooleanVal(true) -> value_of_expr then_expr state
    | BooleanVal(false) -> value_of_expr else_expr state
    | _ -> raise (Unknown_error_reference_interpretor "ruleIfThenElseExpr")
    
and


(* ruleTupleExpr: ast -> (string, valueType ref) Hashtbl.t -> valueType
Function calculating the value of an expression tuple
Parameters:
  - exprs: abstract syntax tree representing the tuple of expressions
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the expression tuple
*)
ruleTupleExpr exprs state =
  TupleVal (
  let rec aux exs l =
    match exs with
      | ExprsNode (e,None) -> List.rev ((value_of_expr e state)::l)
      | ExprsNode (e1,Some(e2)) -> aux e2 ((value_of_expr e1 state)::l)
      | _ -> raise (Unknown_error_reference_interpretor "ruleTupleExpr")
  in aux exprs [])

and


(* ruleValue: ast -> (string, valueType ref) Hashtbl.t -> valueType
Function converting a value node into a valueType
Parameters:
  - v: the abstract syntax tree representing the value
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the value node
*)
ruleValue v =
  let rec aux vs l =
    (match vs with
      | ValueSeqNode (val1,None) -> List.rev ((ruleValue val1)::l)
      | ValueSeqNode (val1,Some(val2)) -> aux val2 ((ruleValue val1)::l)
      | _ -> raise (Unknown_error_reference_interpretor "ruleValue2")) in
  match v with
    | ValueNode (IntegerNode (_,i)) -> IntegerVal(i)
    | ValueNode (CharNode (_,c)) -> CharVal(c)
    | ValueNode (StringNode (_,s)) -> StringVal(s)
    | ValueNode (TrueNode(_)) -> BooleanVal(true)
    | ValueNode (FalseNode(_)) -> BooleanVal(false)
    | ValueNode (EmptyList) -> ListVal([])
    | ValueNode (ValueSeqNode (v1,v2)) -> ListVal (aux (ValueSeqNode(v1,v2)) [])
    | ValueSeqNode (_,_) -> TupleVal (aux v [])
    | _ -> raise (Unknown_error_reference_interpretor "ruleValue")
and


(* ruleIdentifier: ast -> (string, valueType ref) Hashtbl.t -> valueType
Function finding the value associated to an identifier
Parameters:
  - a: the abstract syntax tree representing the assignable
  - state: hash table representing the state of all the variables  
Return: the result corresponding to the assignable
*)
ruleIdentifier a (global,local) = 
  let n = Hashtbl.find_opt local a in
  match n with
    | None -> !(Hashtbl.find global a)
    | Some(name) -> !name