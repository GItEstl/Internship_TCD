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

type typ =   
  | IntegerT
  | BooleanT
  | CharT
  | StringT
  | ChannT

type prefixAction =
  | Tau
  | Send
  | Receive
  | New
  | Spawn

(* Type for the Construction of the Abstract Syntaxic Tree  *)
type ast =
  | ProgramNode of ast * ast
  | AssignNode of ast
  | AssignSeqNode of ast * ast option
  | InstrNode of ast
  | InstrSeqNode of ast * ast option
  | BInstrNode of ast
  | ExprsNode of ast * ast option
  | FunctionNode of ast * string * ast * ast
  | ParamsNode of ast * string * ast option
  | VariableDeclasNode of ast * ast option
  | VariableDeclaNode of ast * string * ast option
  | BodyNode of ast * ast
  | WhileNode of ast * ast
  | LetInNode of ast * ast * ast
  | ReceiveNode of ast * ast
  | SendNode of ast
  | NewNode of ast * ast
  | SpawnNode of string * ast
  | CallNode of ast option * string * ast
  | IfthenelseNode of ast * ast * ast
  | ChooseNode of ast
  | ChoicesNode of ast * ast option
  | PrefixNode of prefixAction * string option * ast option
  | IntegerNode of int
  | CharNode of char
  | StringNode of string
  | ChannelNode of ast 
  | ListNode of ast * ast
  | BinaryNode of ast * ast * ast
  | UnaryNode of ast * ast
  | ValueNode of ast
  | ValueSeqNode of ast * ast option
  | TrueNode
  | FalseNode
  | OpUnaryNode of unary
  | OpBinaryNode of binary
  | TypeNode of typ
  | ListTNode of ast
  | ChannTNode of ast
  | TupleTNode of ast * ast option
  | FuncTNode of ast option
;;

(* string_of_ast : ast -> string *)
(* Convertit un ast en une chaine de caractères en vue de son affichage 
let rec string_of_ast tree =
  match tree with
    | (FunctionNode (par,body)) -> 
	("(fun " ^ par ^ " -> " ^ (string_of_ast body) ^ ")")
    | (CallNode (func,par)) -> 
	("(" ^ (string_of_ast func) ^ " " ^ (string_of_ast par) ^ ")") 
    | (IfthenelseNode (cond,bthen,belse)) -> 
	("(if " ^ (string_of_ast cond) ^ " then " ^ (string_of_ast bthen) ^ " else " ^ (string_of_ast belse) ^ ")")
    | (LetNode (id,blet,bin)) -> 
	("(let " ^ id ^ " = " ^ (string_of_ast blet) ^ " in " ^ (string_of_ast bin) ^ ")")
    | (LetrecNode (id,blet,bin)) -> 
	("(let rec " ^ id ^ " = " ^ (string_of_ast blet) ^ " in " ^ (string_of_ast bin) ^ ")")
    | (RefNode exp) -> ("(ref " ^ (string_of_ast exp) ^ ")")
    | (ReadNode exp) -> ("(! " ^ (string_of_ast exp) ^ ")")
    | (WriteNode (id,exp)) -> 
	("(" ^ (string_of_ast id) ^ " := " ^ (string_of_ast exp) ^ ")")
    | (SequenceNode (exp,exp2)) -> ("( " ^ (string_of_ast exp) ^ "; " ^ (string_of_ast exp2) ^ ")")
    | (AccessNode name) -> name
    | (IntegerNode value) -> (string_of_int value) 
    | (BinaryNode (op,left,right)) -> 
	( "(" ^ (string_of_ast left) ^
	    (match op with
	       | Equal -> " = "
	       | Different -> " != "
	       | Lesser -> " < "
	       | Greater -> " > "
	       | LesserEqual -> " <= "
	       | GreaterEqual -> " => "
	       | Add -> " + "
	       | Substract -> " - "
	       | Or -> " || "
	       | Multiply -> " * "
	       | Divide -> " / "
	       | And -> " && "
	    ) ^ (string_of_ast right) ^ ")" ) 
    | (UnaryNode (op,expr)) -> 
	( "(" ^ 
	    (match op with
	       | Negate -> "- "
            ) ^ (string_of_ast expr) ^ ")" )
    | TrueNode  -> ( "true")
    | FalseNode  -> ( "false")
    | UnitNode  -> ( "()")
;;    *)

