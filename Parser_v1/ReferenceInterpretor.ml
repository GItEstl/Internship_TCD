open Ast

type valueType =
  | IntegerVal of int
  | BooleanVal of bool
  | StringVal of string
  | CharVal of char
  | VoidVal
  | ListVal of valueType list
  | TupleVal of valueType list
  | ErrorVal

(*
let rec value_of_expr expr =
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
*)

(* Rule for unary operators *)

let ruleUnary op expr envVar envType v = 
  (*let v = value_of_expr expr in *)
  (match v with
    | IntegerVal(i) -> 
      (match op with
        | NegateInt -> IntegerVal(-i)
        | Odd -> if ((i mod 2) == 1) then BooleanVal(true) else BooleanVal(false)
        | Even -> if ((i mod 2) == 0) then BooleanVal(true) else BooleanVal(false)
      )
    | BooleanVal(b) ->
      (match op with
        | NegateBool -> BooleanVal(not b)
      ) 
    | ListVal(t::q) -> 
      (match op with
        | Head -> t
        | Tail -> ListVal(q)
      )
    | TupleVal(l) -> 
      (match op with
        | Fst -> List.nth l 0
        | Snd -> List.nth l 1  
      ) 
  )
