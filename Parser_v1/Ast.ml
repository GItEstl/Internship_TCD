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
  | Fst
  | Snd
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
  | AssignNode of Lexing.position * string
  | InstrNode of Lexing.position * ast
  | InstrSeqNode of ast * ast option
  | BInstrNode of Lexing.position * ast
  | ExprNode of Lexing.position * ast
  | ExprsNode of ast * ast option
  | FunctionNode of Lexing.position * ast * string * ast * ast
  | ParamsNode of Lexing.position * ast * string * ast option
  | VariableDeclasNode of ast * ast option
  | VariableDeclaNode of Lexing.position *  ast * ast
  | TupleDeclaNode of Lexing.position * string * ast option
  | BodyNode of Lexing.position * ast option * ast
  | WhileNode of Lexing.position * ast * ast
  | ReceiveNode of Lexing.position * ast * string
  | SendNode of Lexing.position * string * ast
  | NewNode of Lexing.position * ast
  | SpawnNode of Lexing.position * string * ast
  | CallNode of Lexing.position * string * ast
  | IfthenelseExprNode of Lexing.position * ast * ast * ast
  | IfthenelseInstrNode of Lexing.position * ast * ast * ast
  | ChooseNode of Lexing.position * ast
  | ChoicesNode of Lexing.position * ast * ast * ast option
  | PrefixNode of Lexing.position * ast option * prefixAction * string option * ast option
  | IntegerNode of Lexing.position * int
  | CharNode of Lexing.position * string
  | StringNode of Lexing.position * string
  | ChannelNode of Lexing.position * ast 
  | ListNode of Lexing.position * ast * ast
  | BinaryNode of Lexing.position * ast * binary * ast
  | UnaryNode of Lexing.position * unary * ast
  | ValueNode of ast
  | ValueSeqNode of ast * ast option
  | TrueNode of Lexing.position
  | FalseNode of Lexing.position
  | TypeNode of Lexing.position * typ
  | ChanTNode of Lexing.position * ast
  | ListTNode of Lexing.position * ast
  | TupleTNode of Lexing.position * ast
  | TypeSeqNode of ast * ast option
  | FuncTNode of ast option
  | NoopNode
  | ReturnNode of Lexing.position * ast option
  | NamedTypeNode of Lexing.position * string
  | TypeDeclaNode of Lexing.position * string * ast
;;

(* Convert an AST into a string *)
let rec string_of_ast tree =
match tree with
    | (ProgramNode (p1,p2)) -> 
  (string_of_ast p1) ^ "\n" ^ (string_of_ast p2)
    | (FunctionNode (_,ft,n,params,b)) ->
  "func " ^ (string_of_ast ft) ^ " " ^ n ^ "(" ^ (string_of_ast params) ^ ") " ^ (string_of_ast b)
    | (ParamsNode (_,t,n,None)) -> 
  (string_of_ast t) ^ " " ^ n
    | (ParamsNode (_,t,n,Some(params))) -> 
  (string_of_ast t) ^ " " ^ n ^ ", " ^ (string_of_ast params)
    | (BodyNode (_,Some(v),i)) ->
  "{\ndef\n" ^ (string_of_ast v) ^ "in \n" ^ (string_of_ast i) ^ "}\n"    
    | (BodyNode (_,None,i)) ->
  "{\n" ^ (string_of_ast i) ^ "}\n"
    | (VariableDeclaNode (_,t,StringNode(_,n))) -> 
  (string_of_ast t) ^ " " ^ n
    | (VariableDeclaNode (_,t,idents)) -> 
  (string_of_ast t) ^ " (" ^ (string_of_ast idents) ^ ")"
    | (TupleDeclaNode (_,n,None)) ->
  n
    | (TupleDeclaNode (_,n,Some(idents))) ->
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
    | (BinaryNode (_,a,Assign,e)) -> 
  (string_of_ast a) ^ " = " ^ (string_of_ast e)
    | (CallNode (_,f,e)) ->
  "call " ^ f ^ "(" ^ (string_of_ast e) ^ ")" 
    | (ReceiveNode (_,a,n)) ->
  (string_of_ast a) ^ " = receive(" ^ n ^ ")"
    | (SendNode (_,n,e)) ->
  "send(" ^ n ^ ", " ^ (string_of_ast e) ^ ")"
    | (IfthenelseInstrNode (_,cond,i1,i2)) ->
  "if (" ^ (string_of_ast cond) ^ ") {\n" ^ (string_of_ast i1) ^ "} else {\n" ^ (string_of_ast i2) ^ "}"
    | (WhileNode (_,e,i)) ->
  "while (" ^ (string_of_ast e) ^ ") {\n" ^ (string_of_ast i) ^ "}"
    | (ChooseNode (_,c)) ->
  "choose {\n" ^ (string_of_ast c) ^ "}"
    | (SpawnNode (_,f,e)) ->
  "spawn " ^ f ^ "(" ^ (string_of_ast e) ^ ")" 
    | (NewNode (_,a)) ->
  (string_of_ast a) ^ " = newChan()" 
    | (ReturnNode (_,Some (e))) ->
  "return " ^ (string_of_ast e)
    | (ReturnNode (_,None)) ->
  "return "   
    | (ChoicesNode(_,p,i,Some(cs))) -> 
  (string_of_ast p) ^ " -> {" ^ (string_of_ast i) ^ "} \n" ^ (string_of_ast cs)
    | (ChoicesNode(_,p,i,None)) -> 
  (string_of_ast p) ^ " -> {" ^ (string_of_ast i) ^ "}"
    | (PrefixNode(_,None,Tau,None,None)) ->
  " | tau"
    | (PrefixNode(_,None,Send,Some(n),Some(e))) -> 
  " |send(" ^ n ^ ", " ^ (string_of_ast e) ^ ")"
    | (PrefixNode(_,Some(a),Receive,Some(n),None)) ->
  " |" ^ (string_of_ast a) ^ " = receive(" ^ n ^ ")"
    | (PrefixNode(_,Some(a),New,None,None)) ->
  (string_of_ast a) ^ " = newChan()"  
    | (PrefixNode(_,None,Spawn,Some(f),Some(e))) ->
  " | spawn " ^ f ^ "(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (_,Negate,e)) ->
  "(-" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (_,Head,e)) ->
  "head(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (_,Tail,e)) ->
  "tail(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (_,Odd,e)) ->
  "odd(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (_,Even,e)) ->
  "even(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (_,Fst,e)) ->
  "fst(" ^ (string_of_ast e) ^ ")"
    | (UnaryNode (_,Snd,e)) ->
  "snd(" ^ (string_of_ast e) ^ ")"
    | (BinaryNode (_,e1,Add,e2)) ->
  "(" ^ (string_of_ast e1) ^ " + " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (_,e1,Substract,e2)) ->
  "(" ^ (string_of_ast e1) ^ " - " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (_,e1,Or,e2)) ->
  "(" ^ (string_of_ast e1) ^ " || " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (_,e1,Multiply,e2)) ->
  "(" ^ (string_of_ast e1) ^ " * " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (_,e1,Divide,e2)) ->
  "(" ^ (string_of_ast e1) ^ " / " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (_,e1,And,e2)) ->
  "(" ^ (string_of_ast e1) ^ " && " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (_,e1,Equal,e2)) ->
  "(" ^ (string_of_ast e1) ^ " == " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (_,e1,Different,e2)) ->
  "(" ^ (string_of_ast e1) ^ " != " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (_,e1,Lesser,e2)) ->
  "(" ^ (string_of_ast e1) ^ " < " ^ (string_of_ast e2) ^ ")"
    | (BinaryNode (_,e1,Greater,e2)) ->
  "(" ^ (string_of_ast e1) ^ " > " ^ (string_of_ast e2) ^ ")"
    | (IfthenelseExprNode (_,cond,e1,e2)) ->
  "if (" ^ (string_of_ast cond) ^ ") {\n" ^ (string_of_ast e1) ^ "} else {\n" ^ (string_of_ast e2) ^ "}"
    | (ExprNode (_,e)) ->
  "(" ^ (string_of_ast e) ^ ")"
    | (ExprsNode (e1,Some (e2))) ->
  (string_of_ast e1) ^ ", " ^ (string_of_ast e2)
    | (ExprsNode (e1,None)) ->
  (string_of_ast e1)
    | (ValueNode (v)) ->
  (string_of_ast v)
    | (IntegerNode (_,c)) ->
  (string_of_int c)
    | (CharNode (_,c)) ->
  "'" ^ c ^ "'"
    | (StringNode (_,c)) ->
  "\"" ^ c ^ "\""
    | (TrueNode (_)) ->
  "true"
    | (FalseNode (_)) ->
  "false"
    | (ValueNode (ValueSeqNode (v,None))) ->
  "[" ^ (string_of_ast v) ^ "]"
    | (ValueNode(ValueSeqNode(v,Some(vs)))) ->
  "[" ^ (string_of_ast v) ^ ", " ^ (string_of_ast vs) ^ "]"
    | (ValueSeqNode (v,None)) ->
  (string_of_ast v)
    | (ValueSeqNode (v,Some(vs))) ->
  (string_of_ast v) ^ ", " ^ (string_of_ast vs)
    | (AssignNode (_,n)) ->
  n
    | (TypeNode (_,IntegerT)) ->
  "int"
    | (TypeNode (_,BooleanT)) ->
  "boolean"
    | (TypeNode (_,StringT)) ->
  "string"
    | (TypeNode (_,CharT)) ->
  "char" 
    | (ChanTNode (_,t)) ->
  "channel " ^ (string_of_ast t) 
    | (ListTNode (_,t)) ->
  "list[" ^ (string_of_ast t) ^ "]" 
    | (TupleTNode (_,t)) ->
  "(" ^ (string_of_ast t) ^ ")" 
    | (TypeSeqNode (t,None)) ->
  (string_of_ast t) 
    | (TypeSeqNode (t1,Some(t2))) ->
  (string_of_ast t1) ^ ", " ^ (string_of_ast t2)
    | (FuncTNode (None)) ->
  "void"
    | (FuncTNode (Some (t))) ->
  (string_of_ast t)
    | (NamedTypeNode (_,t)) ->
  t
    | (TypeDeclaNode (_,n,t)) ->
  "type " ^ n ^ " = " ^ (string_of_ast t) 
    | _ -> 
  "unknownPrinting"

