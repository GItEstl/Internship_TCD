(* Binary operator Type *)
type binary = 
  | Add 
  | Substract 
  | Or 
  | Multiply 
  | Divide 
  | And 
  | Equal 
  | Different 
  | Lesser 
  | Greater
  | Assign
;;

(* Unary operator Type *)
type unary = 
  | Negate
  | Head
  | Tail
  | Odd
  | Even
;;

(* Basic type Type *)
type typ =   
  | IntegerT
  | BooleanT
  | CharT
  | StringT
;;

(* prefix Type *)
type prefixAction =
  | Tau
  | Send
  | Receive
  | New
  | Spawn
;;

(* Type for the Construction of the Abstract Syntaxic Tree  *)
type ast =
  | ProgramNode of ast * ast
  | AssignNode of string
  | InstrNode of ast
  | InstrSeqNode of ast * ast option
  | BInstrNode of ast
  | ExprNode of ast
  | ExprsNode of ast * ast option
  | FunctionNode of ast * string * ast * ast
  | ParamsNode of ast * string * ast option
  | VariableDeclasNode of ast * ast option
  | VariableDeclaNode of ast * ast
  | TupleDeclaNode of string * ast option
  | BodyNode of ast option * ast
  | WhileNode of ast * ast
  | ReceiveNode of ast * string
  | SendNode of string * ast
  | NewNode of ast
  | SpawnNode of string * ast
  | CallNode of string * ast
  | IfthenelseExprNode of ast * ast * ast
  | IfthenelseInstrNode of ast * ast * ast
  | ChooseNode of ast
  | ChoicesNode of ast * ast * ast option
  | PrefixNode of ast option * prefixAction * string option * ast option
  | IntegerNode of int
  | CharNode of string
  | StringNode of string
  | ChannelNode of ast 
  | ListNode of ast * ast
  | BinaryNode of ast * binary * ast
  | UnaryNode of unary * ast
  | ValueNode of ast
  | ValueSeqNode of ast * ast option
  | TrueNode
  | FalseNode
  | TypeNode of typ
  | ChanTNode of ast
  | ListTNode of ast
  | TupleTNode of ast
  | TypeSeqNode of ast * ast option
  | FuncTNode of ast option
  | NoopNode
  | ReturnNode of ast option
;;

(* Convert an AST into a string *)
let rec string_of_ast tree =
match tree with
    | (ProgramNode (p1,p2)) -> 
  (string_of_ast p1) ^ "\n" ^ (string_of_ast p2)
    | (FunctionNode (ft,n,params,b)) ->
  "func " ^ (string_of_ast ft) ^ " " ^ n ^ "(" ^ (string_of_ast params) ^ ") " ^ (string_of_ast b)
    | (ParamsNode (t,n,None)) -> 
  (string_of_ast t) ^ " " ^ n
    | (ParamsNode (t,n,Some(params))) -> 
  (string_of_ast t) ^ " " ^ n ^ ", " ^ (string_of_ast params)
    | (BodyNode (Some(v),i)) ->
  "{\ndef \n" ^ (string_of_ast v) ^ "in \n" ^ (string_of_ast i) ^ "}\n"    
    | (BodyNode (None,i)) ->
  "{\n" ^ (string_of_ast i) ^ "}\n"
    | (VariableDeclaNode (t,StringNode(n))) -> 
  (string_of_ast t) ^ " " ^ n
    | (VariableDeclaNode (t,idents)) -> 
  (string_of_ast t) ^ " (" ^ (string_of_ast idents) ^ ")"
    | (TupleDeclaNode (n,None)) ->
  n
    | (TupleDeclaNode (n,Some(idents))) ->
  n ^ ", " ^ (string_of_ast idents)
    | (VariableDeclasNode (v,None)) -> 
  (string_of_ast v) ^ "\n"
    | (VariableDeclasNode (v,Some(vs))) -> 
  (string_of_ast v) ^ ";\n" ^ (string_of_ast vs)
    | (InstrSeqNode (NoopNode,Some(i))) ->
  ";\n" ^ (string_of_ast i)
    | (InstrSeqNode (NoopNode,None)) ->
  ""
    | (InstrSeqNode (bi,None)) ->
  (string_of_ast bi) ^ "\n"
    | (InstrSeqNode (bi,Some(i))) ->
  (string_of_ast bi) ^ ";\n" ^ (string_of_ast i)
    | (BinaryNode (a,Assign,e)) -> 
  (string_of_ast a) ^ " = " ^ (string_of_ast e)
    | (CallNode (f,e)) ->
  f ^ "(" ^ (string_of_ast e) ^ ")" 
    | (ReceiveNode (a,n)) ->
  (string_of_ast a) ^ " = receive(" ^ n ^ ")"
    | (SendNode (n,e)) ->
  "send(" ^ n ^ ", " ^ (string_of_ast e) ^ ")"
    | (IfthenelseInstrNode (cond,i1,i2)) ->
  "if (" ^ (string_of_ast cond) ^ ") {\n" ^ (string_of_ast i1) ^ "} else {\n" ^ (string_of_ast i2) ^ "}"
    | (WhileNode (e,i)) ->
  "while (" ^ (string_of_ast e) ^ ") {\n" ^ (string_of_ast i) ^ "}"
    | (ChooseNode (c)) ->
  "choose {\n" ^ (string_of_ast c) ^ "}"
    | (SpawnNode (f,e)) ->
  "spawn " ^ f ^ "(" ^ (string_of_ast e) ^ ")" 
    | (NewNode (a)) ->
  (string_of_ast a) ^ " = newChan()" 
    | (ReturnNode (Some (e))) ->
  "return " ^ (string_of_ast e)
    | (ReturnNode (None)) ->
  "return "   
    | (ChoicesNode(p,i,Some(cs))) -> 
  (string_of_ast p) ^ " -> {" ^ (string_of_ast i) ^ "} \n" ^ (string_of_ast cs)
    | (ChoicesNode(p,i,None)) -> 
  (string_of_ast p) ^ " -> {" ^ (string_of_ast i) ^ "}"
    | (PrefixNode(None,Tau,None,None)) ->
  " | tau"
    | (PrefixNode(None,Send,Some(n),Some(e))) -> 
  " |send(" ^ n ^ ", " ^ (string_of_ast e) ^ ")"
    | (PrefixNode(Some(a),Receive,Some(n),None)) ->
  " |" ^ (string_of_ast a) ^ " = receive(" ^ n ^ ")"
    | (PrefixNode(Some(a),New,None,None)) ->
  (string_of_ast a) ^ " = newChan()"  
    | (PrefixNode(None,Spawn,Some(f),Some(e))) ->
  " | spawn " ^ f ^ "(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (Negate,e)) ->
  "(-" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (Head,e)) ->
  "head(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (Tail,e)) ->
  "tail(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (Odd,e)) ->
  "odd(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (Even,e)) ->
  "even(" ^ (string_of_ast e) ^ ")"
    | (BinaryNode (e1,Add,e2)) ->
  "(" ^ (string_of_ast e1) ^ " + " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (e1,Substract,e2)) ->
  "(" ^ (string_of_ast e1) ^ " - " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (e1,Or,e2)) ->
  "(" ^ (string_of_ast e1) ^ " || " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (e1,Multiply,e2)) ->
  "(" ^ (string_of_ast e1) ^ " * " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (e1,Divide,e2)) ->
  "(" ^ (string_of_ast e1) ^ " / " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (e1,And,e2)) ->
  "(" ^ (string_of_ast e1) ^ " && " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (e1,Equal,e2)) ->
  "(" ^ (string_of_ast e1) ^ " == " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (e1,Different,e2)) ->
  "(" ^ (string_of_ast e1) ^ " != " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (e1,Lesser,e2)) ->
  "(" ^ (string_of_ast e1) ^ " < " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (e1,Greater,e2)) ->
  "(" ^ (string_of_ast e1) ^ " > " ^ (string_of_ast e2) ^ ")"
    | (IfthenelseExprNode (cond,e1,e2)) ->
  "if (" ^ (string_of_ast cond) ^ ") {\n" ^ (string_of_ast e1) ^ "} else {\n" ^ (string_of_ast e2) ^ "}"
    | (ExprNode (e)) ->
  "(" ^ (string_of_ast e) ^ ")"
    | (ExprsNode (e1,Some (e2))) ->
  (string_of_ast e1) ^ ", " ^ (string_of_ast e2)
    | (ExprsNode (e1,None)) ->
  (string_of_ast e1)
    | (ValueNode (v)) ->
  (string_of_ast v)
    | (IntegerNode (c)) ->
  (string_of_int c)
    | (CharNode (c)) ->
  "'" ^ c ^ "'"
    | (StringNode (c)) ->
  "\"" ^ c ^ "\""
    | (TrueNode) ->
  "true"
    | (FalseNode) ->
  "false"
    | (ValueNode (ValueSeqNode (v,None))) ->
  "[" ^ (string_of_ast v) ^ "]"
    | (ValueNode(ValueSeqNode(v,Some(vs)))) ->
  "[" ^ (string_of_ast v) ^ ", " ^ (string_of_ast vs) ^ "]"
    | (ValueSeqNode (v,None)) ->
  (string_of_ast v)
    | (ValueSeqNode (v,Some(vs))) ->
  (string_of_ast v) ^ ", " ^ (string_of_ast vs)
    | (AssignNode (n)) ->
  n
    | (TypeNode (IntegerT)) ->
  "int"
    | (TypeNode (BooleanT)) ->
  "boolean"
    | (TypeNode (StringT)) ->
  "string"
    | (TypeNode (CharT)) ->
  "char" 
    | (ChanTNode (t)) ->
  "channel " ^ (string_of_ast t) 
    | (ListTNode (t)) ->
  "list[" ^ (string_of_ast t) ^ "]" 
    | (TupleTNode (t)) ->
  "(" ^ (string_of_ast t) ^ ")" 
    | (TypeSeqNode (t,None)) ->
  (string_of_ast t) 
    | (TypeSeqNode (t1,Some(t2))) ->
  (string_of_ast t1) ^ ", " ^ (string_of_ast t2)
    | (FuncTNode (None)) ->
  "void"
    | (FuncTNode (Some (t))) ->
  (string_of_ast t)
    | _ -> 
  "unknownPrinting"

