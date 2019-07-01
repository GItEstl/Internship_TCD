%{
open Ast
%}


%token <int> NumberToken
%token <string> IdentToken
%token <string> CharValueToken
%token ArrowToken
%token IfToken
%token ElseToken
%token WhileToken
%token DefToken, InToken
%token TrueToken
%token FalseToken
%token LeftParenthesisToken
%token RightParenthesisToken
%token LeftBracketToken
%token RightBracketToken
%token LeftSqBracketToken
%token RightSqBracketToken
%token SequenceToken, AssignToken, OrToken, AndToken, DifferentToken, EqualToken
%token LesserToken, GreaterToken
%token AddToken, SubToken
%token MulToken, DivToken
%token ComaToken
%token DoubleQuoteToken
%token BoolToken, StringToken, IntegerToken, CharToken, ChannelToken, ListToken
%token VoidToken
%token FunctionToken
%token ReceiveToken, SendToken, NewToken, SpawnToken, TauToken
%token ChooseToken, ChoiceToken
%token HeadToken, TailToken, OddToken, EvenToken
%token StartToken, ReturnToken
%token TypeToken
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
 | FunctionToken ft = funcType n = IdentToken LeftParenthesisToken params = parameters RightParenthesisToken b = body       {FunctionNode (ft,n,params,b)}

 typeDecla :
 | TypeToken tname = IdentToken AssignToken t = typ                                                          {TypeDeclaNode (tname,t)}

parameters : 
 | t = typ n = IdentToken                                                                                   {ParamsNode (t,n,None)} 
 | t = typ n = IdentToken ComaToken params = parameters                                                     {ParamsNode (t,n,Some(params))} 

body :
 | LeftBracketToken i = instruction RightBracketToken                                                           {BodyNode (None,i)}
 | LeftBracketToken DefToken v = variableDeclas InToken i = instruction RightBracketToken                       {BodyNode (Some(v),i)}

variableDecla :
 | t = typ n = IdentToken                                                                                   {VariableDeclaNode (t,StringNode(n))}
 | t = typ LeftParenthesisToken idents = tupleDecla RightParenthesisToken                                   {VariableDeclaNode (t,idents)}

tupleDecla : 
 | n = IdentToken                                                                                           {TupleDeclaNode (n,None)} 
 | n = IdentToken ComaToken idents = tupleDecla                                                             {TupleDeclaNode (n,Some(idents))} 

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
 | a = expr AssignToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken              {BinaryNode (a,Assign,CallNode (f,e))} 
 | f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                              {CallNode (f,e)}
 | a = expr AssignToken ReceiveToken LeftParenthesisToken n = IdentToken RightParenthesisToken                  {ReceiveNode (a,n)}
 | SendToken LeftParenthesisToken n = IdentToken ComaToken e = expr RightParenthesisToken                            {SendNode (n,e)}
 | IfToken LeftParenthesisToken cond = expr RightParenthesisToken 
      LeftBracketToken i1 = instruction RightBracketToken 
      ElseToken LeftBracketToken i2 = instruction RightBracketToken                                             {IfthenelseInstrNode (cond,i1,i2)}
 | WhileToken LeftParenthesisToken e = expr RightParenthesisToken LeftBracketToken i = instruction RightBracketToken          {WhileNode (e,i)}
 | ChooseToken LeftBracketToken c = choices RightBracketToken                                                     {ChooseNode (c)}
 | SpawnToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                         {SpawnNode (f,e)}
 | a = expr AssignToken NewToken LeftParenthesisToken RightParenthesisToken                                         {NewNode (a)}
 | ReturnToken                                                                                                          {ReturnNode (None)}
 | ReturnToken e = expr                                                                                                 {ReturnNode (Some (e))}
 | ReturnToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                      {ReturnNode (Some (CallNode (f,e)))}

choices : 
 | p = prefix ArrowToken LeftBracketToken i = instruction RightBracketToken cs = choices                                  {ChoicesNode(p,i,Some(cs))}
 | p = prefix ArrowToken LeftBracketToken i = instruction RightBracketToken                                            {ChoicesNode(p,i,None)}

prefix :
 | ChoiceToken TauToken                                                                                                                {PrefixNode(None,Tau,None,None)}
 | ChoiceToken SendToken LeftParenthesisToken n = IdentToken ComaToken e = expr RightParenthesisToken                                {PrefixNode(None,Send,Some(n),Some(e))}
 | ChoiceToken a = expr AssignToken ReceiveToken LeftParenthesisToken n = IdentToken RightParenthesisToken                         {PrefixNode(Some(a),Receive,Some(n),None)}
 | ChoiceToken a = expr AssignToken NewToken LeftParenthesisToken RightParenthesisToken                                             {PrefixNode(Some(a),New,None,None)}
 | ChoiceToken SpawnToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                  {PrefixNode(None,Spawn,Some(f),Some(e))}
 
expr :
 | SubToken e = expr                                             {UnaryNode (Negate,e)}
 | HeadToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Head,e)}                                                                                                                     
 | TailToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Tail,e)}                                                                                                                    
 | OddToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Odd,e)}                                                                                                                      
 | EvenToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (Even,e)} 
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
 | n = IdentToken                                                                                                                 {AssignNode (n)}

exprs :
 | e = expr                                                                                                                        {ExprsNode (e,None)}
 | e1 = expr ComaToken e2 = exprs                                                                                                  {ExprsNode (e1,Some(e2))}

cst : 
 | c = NumberToken                                                                                                        {IntegerNode(c)}
 | c = CharValueToken                                                                                                    {CharNode (c)}
 | DoubleQuoteToken c = IdentToken DoubleQuoteToken                                                                       {StringNode (c)}
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
 | t = IdentToken                                                                                                         {NamedTypeNode (t)}        

types :
 | t = typ                                                                                                            {TypeSeqNode (t,None)}
 | t1 = typ ComaToken t2 = types                                                                                      {TypeSeqNode (t1, Some(t2))}

funcType :
 | VoidToken                                                                                                          {FuncTNode (None)}
 | t = typ                                                                                                            {FuncTNode (Some (t))}

callMain :
 | StartToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                       {CallNode (f,e)} 