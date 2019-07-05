%{
open Ast
%}


%token <Lexing.position * int> NumberToken
%token <Lexing.position * string> IdentToken
%token <Lexing.position * string> CharValueToken
%token <Lexing.position> ArrowToken
%token <Lexing.position> IfToken
%token <Lexing.position> ElseToken
%token <Lexing.position> WhileToken
%token <Lexing.position> DefToken, InToken
%token <Lexing.position> TrueToken
%token <Lexing.position> FalseToken
%token <Lexing.position> LeftParenthesisToken
%token <Lexing.position> RightParenthesisToken
%token <Lexing.position> LeftBracketToken
%token <Lexing.position>RightBracketToken
%token <Lexing.position> LeftSqBracketToken
%token <Lexing.position> RightSqBracketToken
%token <Lexing.position> SequenceToken, AssignToken, OrToken, AndToken, DifferentToken, EqualToken
%token <Lexing.position>  LesserToken, GreaterToken
%token <Lexing.position> AddToken, SubToken
%token <Lexing.position> MulToken, DivToken
%token <Lexing.position> ComaToken
%token <Lexing.position> DoubleQuoteToken
%token <Lexing.position> BoolToken, StringToken, IntegerToken, CharToken, ChannelToken, ListToken
%token <Lexing.position> VoidToken
%token <Lexing.position> FunctionToken
%token <Lexing.position> ReceiveToken, SendToken, NewToken, SpawnToken, TauToken
%token <Lexing.position> ChooseToken, ChoiceToken
%token <Lexing.position> HeadToken, TailToken, OddToken, EvenToken, FstToken, SndToken
%token <Lexing.position> StartToken, ReturnToken
%token <Lexing.position> TypeToken
%token EOF

%left OrToken
%left AndToken
%left DifferentToken, EqualToken
%nonassoc LesserToken, GreaterToken
%left AddToken, SubToken
%left MulToken, DivToken

(* Type de l'attribut synthétisé des non-terminaux *)
%type <Ast.ast> program

(* Type et définition de l'axiome *)
%start <Ast.ast> main

%%

main : a = program EOF   {a}

program :
 | t = typeDecla p = program                                                                                {ProgramNode (t,p)} 
 | f = funcDecla p = program                                                                                 {ProgramNode (f,p)}
 | v = variableDecla p = program                                                                             {ProgramNode (v,p)}
 | f = funcDecla c = callMain                                                                                    {ProgramNode (f,c)}
 | v = variableDecla c = callMain                                                                                {ProgramNode (v,c)}

funcDecla :
 | FunctionToken ft = funcType n = IdentToken LeftParenthesisToken params = parameters RightParenthesisToken b = body       {FunctionNode (ft,snd(n),params,b)}

 typeDecla :
 | TypeToken tname = IdentToken AssignToken t = typ                                                          {TypeDeclaNode (snd(tname),t)}

parameters : 
 | t = typ n = IdentToken                                                                                   {ParamsNode (t,snd(n),None)} 
 | t = typ n = IdentToken ComaToken params = parameters                                                     {ParamsNode (t,snd(n),Some(params))} 

body :
 | LeftBracketToken i = instruction RightBracketToken                                                           {BodyNode (None,i)}
 | LeftBracketToken DefToken v = variableDeclas InToken i = instruction RightBracketToken                       {BodyNode (Some(v),i)}

variableDecla :
 | t = typ n = IdentToken                                                                                   {VariableDeclaNode (t,StringNode(snd(n)))}
 | t = typ LeftParenthesisToken idents = tupleDecla RightParenthesisToken                                   {VariableDeclaNode (t,idents)}

tupleDecla : 
 | n = IdentToken                                                                                           {TupleDeclaNode (snd(n),None)} 
 | n = IdentToken ComaToken idents = tupleDecla                                                             {TupleDeclaNode (snd(n),Some(idents))} 

variableDeclas :
 | v = variableDecla                                                                                         {VariableDeclasNode (v,None)}
 | v = variableDecla SequenceToken                                                                           {VariableDeclasNode (v,None)}
 | v = variableDecla SequenceToken vs = variableDeclas                                                       {VariableDeclasNode (v,Some(vs))}  

instruction :
 |                                                                                                         {InstrSeqNode(NoopNode,None)}
 | SequenceToken i = instruction                                                                           {InstrSeqNode(NoopNode,Some(i))}
 | bi = binstruction                                                                                       {InstrSeqNode (bi,None)}
 | bi = binstruction SequenceToken i = instruction                                                         {InstrSeqNode (bi,Some(i))}

binstruction :
 | a = expr AssignToken e = expr                                                                                  {BinaryNode (a,Assign,e)}
 | a = expr AssignToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken              {BinaryNode (a,Assign,CallNode (snd(f),e))} 
 | f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                              {CallNode (snd(f),e)}
 | a = expr AssignToken ReceiveToken LeftParenthesisToken n = IdentToken RightParenthesisToken                  {ReceiveNode (a,snd(n))}
 | SendToken LeftParenthesisToken n = IdentToken ComaToken e = expr RightParenthesisToken                            {SendNode (snd(n),e)}
 | IfToken LeftParenthesisToken cond = expr RightParenthesisToken 
      LeftBracketToken i1 = instruction RightBracketToken 
      ElseToken LeftBracketToken i2 = instruction RightBracketToken                                             {IfthenelseInstrNode (cond,i1,i2)}
 | WhileToken LeftParenthesisToken e = expr RightParenthesisToken LeftBracketToken i = instruction RightBracketToken          {WhileNode (e,i)}
 | ChooseToken LeftBracketToken c = choices RightBracketToken                                                     {ChooseNode (c)}
 | SpawnToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                         {SpawnNode (snd(f),e)}
 | a = expr AssignToken NewToken LeftParenthesisToken RightParenthesisToken                                         {NewNode (a)}
 | ReturnToken                                                                                                          {ReturnNode (None)}
 | ReturnToken e = expr                                                                                                 {ReturnNode (Some (e))}
 | ReturnToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                      {ReturnNode (Some (CallNode (snd(f),e)))}

choices : 
 | p = prefix ArrowToken LeftBracketToken i = instruction RightBracketToken cs = choices                                  {ChoicesNode(p,i,Some(cs))}
 | p = prefix ArrowToken LeftBracketToken i = instruction RightBracketToken                                            {ChoicesNode(p,i,None)}

prefix :
 | ChoiceToken TauToken                                                                                                                {PrefixNode(None,Tau,None,None)}
 | ChoiceToken SendToken LeftParenthesisToken n = IdentToken ComaToken e = expr RightParenthesisToken                                {PrefixNode(None,Send,Some(snd(n)),Some(e))}
 | ChoiceToken a = expr AssignToken ReceiveToken LeftParenthesisToken n = IdentToken RightParenthesisToken                         {PrefixNode(Some(a),Receive,Some(snd(n)),None)}
 | ChoiceToken a = expr AssignToken NewToken LeftParenthesisToken RightParenthesisToken                                             {PrefixNode(Some(a),New,None,None)}
 | ChoiceToken SpawnToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                  {PrefixNode(None,Spawn,Some(snd(f)),Some(e))}
 
expr :
 | SubToken e = expr                                             {UnaryNode (Negate,e)}
 | HeadToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Head,e)}                                                                                                                     
 | TailToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Tail,e)}                                                                                                                    
 | OddToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Odd,e)}                                                                                                                      
 | EvenToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Even,e)}
 | FstToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Fst,e)}                                                                                                                      
 | SndToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Snd,e)}  
 | e1 = expr AddToken e2 = expr                                                                                                       {BinaryNode (e1,Add,e2)}
 | e1 = expr SubToken e2 = expr                                                                                                       {BinaryNode (e1,Substract,e2)}
 | e1 = expr OrToken e2 = expr                                                                                                        {BinaryNode (e1,Or,e2)}
 | e1 = expr MulToken e2 = expr                                                                                                       {BinaryNode (e1,Multiply,e2)}
 | e1 = expr DivToken e2 = expr                                                                                                       {BinaryNode (e1,Divide,e2)}
 | e1 = expr AndToken e2 = expr                                                                                                       {BinaryNode (e1,And,e2)}
 | e1 = expr EqualToken e2 = expr                                                                                                     {BinaryNode (e1,Equal,e2)}
 | e1 = expr DifferentToken e2 = expr                                                                                                 {BinaryNode (e1,Different,e2)}
 | e1 = expr LesserToken e2 = expr                                                                                                    {BinaryNode (e1,Lesser,e2)}
 | e1 = expr GreaterToken e2 = expr                                                                                                   {BinaryNode (e1,Greater,e2)}
 | IfToken LeftParenthesisToken cond = expr RightParenthesisToken 
      LeftBracketToken e1 = expr RightBracketToken ElseToken LeftBracketToken e2 = expr RightBracketToken                         {IfthenelseExprNode (cond,e1,e2)}
 | LeftParenthesisToken e = exprs RightParenthesisToken                                                                           {ExprNode (e)}
 | v = value                                                                                                                      {ValueNode (v)} 
 | n = IdentToken                                                                                                                 {AssignNode (snd(n))}

exprs :
 | e = expr                                                                                                                        {ExprsNode (e,None)}
 | e1 = expr ComaToken e2 = exprs                                                                                                  {ExprsNode (e1,Some(e2))}

cst : 
 | c = NumberToken                                                                                                        {IntegerNode(snd(c))}
 | c = CharValueToken                                                                                                    {CharNode (snd(c))}
 | DoubleQuoteToken c = IdentToken DoubleQuoteToken                                                                       {StringNode (snd(c))}
 | TrueToken                                                                                                              {TrueNode}
 | FalseToken                                                                                                             {FalseNode}

value :
 | c = cst                                                                                                              {ValueNode (c)}              
 | LeftSqBracketToken vs = valueSeq RightSqBracketToken                                                                 {ValueNode (vs)}

valueSeq :
 | v = value                                                                                                            {ValueSeqNode (v,None)}
 | v = value ComaToken vs = valueSeq                                                                                    {ValueSeqNode (v,Some(vs))}

typ :
 | IntegerToken                                                                                                            {TypeNode (IntegerT)}
 | BoolToken                                                                                                              {TypeNode (BooleanT)}    
 | StringToken                                                                                                            {TypeNode (StringT)}
 | CharToken                                                                                                              {TypeNode (CharT)}
 | ChannelToken t = typ                                                                                                   {ChanTNode (t)}
 | ListToken LeftSqBracketToken t = typ RightSqBracketToken                                                               {ListTNode (t)}
 | LeftParenthesisToken tSeq = types RightParenthesisToken                                                                {TupleTNode (tSeq)}
 | t = IdentToken                                                                                                         {NamedTypeNode (snd(t))}        

types :
 | t = typ                                                                                                            {TypeSeqNode (t,None)}
 | t1 = typ ComaToken t2 = types                                                                                      {TypeSeqNode (t1, Some(t2))}

funcType :
 | VoidToken                                                                                                          {FuncTNode (None)}
 | t = typ                                                                                                            {FuncTNode (Some (t))}

callMain :
 | StartToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                       {CallNode (snd(f),e)} 