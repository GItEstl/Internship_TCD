%{
open Ast
%}


%token <int> NumberToken
%token <string> IdentToken
%token <char> CharValueToken
%token ArrowToken
%token IfToken
%token ElseToken
%token WhileToken
%token LetToken, InToken
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
%token SimpleQuoteToken, DoubleQuoteToken
%token BoolToken, StringToken, IntegerToken, CharToken, ChannelToken, ListToken
%token VoidToken
%token FunctionToken
%token ReceiveToken, SendToken, NewToken, SpawnToken, TauToken
%token ChooseToken
%token HeadToken, TailToken, OddToken, EvenToken
%token EOF

(* Type de l'attribut synthétisé des non-terminaux *)
%type <Ast.ast> program

(* Type et définition de l'axiome *)
%start <Ast.ast> main

%%

main : a = program EOF   {a}

program :
 | f = funcDecla p = program                                                                                 {ProgramNode (f,p)}
 | v = variableDecla p = program                                                                             {ProgramNode (v,p)}
 | f = funcDecla c = callMain                                                                                    {ProgramNode (f,c)}
 | v = variableDecla c = callMain                                                                                {ProgramNode (v,c)}

funcDecla :
 | FunctionToken ft = funcType n = IdentToken LeftParenthesisToken params = parameters RightParenthesisToken b = body       {FunctionNode (ft,n,params,b)}

parameters : 
 | t = typ n = IdentToken                                                                                   {ParamsNode (t,n,None)} 
 | t = typ n = IdentToken ComaToken params = parameters                                                     {ParamsNode (t,n,Some(params))} 

body :
 | LeftBracketToken v = variableDeclas SequenceToken i = instruction RightBracketToken                       {BodyNode (v,i)}

variableDecla :
 | t = typ n = IdentToken                                                                                   {VariableDeclaNode (t,n, None)}
 | t = typ n = IdentToken EqualToken e = expr                                                               {VariableDeclaNode (t,n,Some(e))}

variableDeclas :
 | v = variableDecla vs = variableDeclas                                                                     {VariableDeclasNode (v,Some(vs))} 
 | v = variableDecla                                                                                         {VariableDeclasNode (v,None)} 

instruction :
 | bi = binstruction                                                                                        {InstrNode (bi)}
 | iSeq = instrSeq                                                                                          {InstrNode (iSeq)}

instrSeq :
 | bi = binstruction                                                                                         {InstrSeqNode (bi,None)}                         
 | bi = binstruction SequenceToken iSeq = instrSeq                                                           {InstrSeqNode (bi,Some(iSeq))}
 
binstruction :
 | a = assignable AssignToken e = expr                                                                        {BinaryNode (a,OpBinaryNode(Assign),e)}
 | a = assignable AssignToken f = IdentToken LeftParenthesisToken e = expr RightParenthesisToken                  {CallNode (Some(a),f,e)}
 | f = IdentToken LeftParenthesisToken e = expr RightParenthesisToken                                             {CallNode (None,f,e)}
 | a = assignable AssignToken ReceiveToken LeftParenthesisToken e = expr RightParenthesisToken                  {ReceiveNode (a,e)}
 | SendToken LeftParenthesisToken e = expr RightParenthesisToken                                                {SendNode (e)}
 | IfToken LeftParenthesisToken cond = expr RightParenthesisToken 
      LeftBracketToken i1 = instruction RightBracketToken 
      ElseToken LeftBracketToken i2 = instruction RightBracketToken                                             {IfthenelseNode (cond,i1,i2)}
 | LetToken a = assignable AssignToken e = expr InToken LeftBracketToken i = instruction RightBracketToken     {LetInNode (a,e,i)}
 | WhileToken LeftParenthesisToken e = expr RightParenthesisToken LeftBracketToken i = instruction RightBracketToken          {WhileNode (e,i)}
 | ChooseToken LeftBracketToken c = choices RightBracketToken                                                     {ChooseNode (c)}
 | SpawnToken LeftParenthesisToken f = IdentToken ComaToken e = expr RightParenthesisToken                                {SpawnNode (f,e)}
 | a = assignable NewToken LeftParenthesisToken e = expr RightParenthesisToken                                    {NewNode (a,e)}

choices : 
 | prefix ArrowToken i = instruction cs = choices                                                                     {ChoicesNode(i,Some(cs))}
 | prefix ArrowToken i = instruction                                                                                  {ChoicesNode(i,None)}

prefix :
 | TauToken                                                                                                                {PrefixNode(Tau,None,None)}
 | SendToken LeftParenthesisToken e = expr RightParenthesisToken                                                             {PrefixNode(Send,None,Some(e))}
 | ReceiveToken LeftParenthesisToken e = expr RightParenthesisToken                                                          {PrefixNode(Receive,None,Some(e))}
 | NewToken LeftParenthesisToken e = expr RightParenthesisToken                                                              {PrefixNode(New,None,Some(e))}
 | SpawnToken LeftParenthesisToken f = IdentToken ComaToken e = expr RightParenthesisToken                                  {PrefixNode(Spawn,Some(f),Some(e))}
 
expr :
 | op = opUnary e = expr                                             {UnaryNode (op,e)}
 | e1 = expr op = opBinary e2 = expr                                      {BinaryNode (e1,op,e2)}
 | LetToken a = assignable AssignToken e1 = expr InToken LeftParenthesisToken e2 = expr RightParenthesisToken                    {LetInNode (a,e1,e2)}
 | IfToken LeftParenthesisToken cond = expr RightParenthesisToken 
      LeftBracketToken e1 = expr RightBracketToken ElseToken LeftBracketToken e2 = expr RightBracketToken                         {IfthenelseNode (cond,e1,e2)}
 | LeftParenthesisToken e1 = expr ComaToken e2 = exprs RightParenthesisToken                                                      {ExprsNode (e1,Some (e2))}
 | v = value                                                                                                                      {ValueNode (v)} 

exprs :
 | e = expr                                                                                                                        {ExprsNode (e,None)}
 | e1 = expr ComaToken e2 = exprs                                                                                                  {ExprsNode (e1,Some(e2))}

opUnary :
 | SubToken                                                                                                                       {OpUnaryNode (Negate)}
 | HeadToken                                                                                                                      {OpUnaryNode (Head)}
 | TailToken                                                                                                                      {OpUnaryNode (Tail)}
 | OddToken                                                                                                                       {OpUnaryNode (Odd)}
 | EvenToken                                                                                                                     {OpUnaryNode (Even)}

opBinary :
 | AddToken                                                                                                                    {OpBinaryNode (Add)}
 | SubToken                                                                                                                    {OpBinaryNode (Substract)}
 | OrToken                                                                                                                    {OpBinaryNode (Or)}
 | MulToken                                                                                                                 {OpBinaryNode (Multiply)}
 | DivToken                                                                                                                    {OpBinaryNode (Divide)}
 | AndToken                                                                                                                    {OpBinaryNode (And)}
 | EqualToken                                                                                                                  {OpBinaryNode (Equal)}
 | DifferentToken                                                                                                               {OpBinaryNode (Different)}
 | LesserToken                                                                                                                   {OpBinaryNode (Lesser)}
 | GreaterToken                                                                                                                  {OpBinaryNode (Greater)}
 | AssignToken                                                                                                                    {OpBinaryNode (Assign)}

cst : 
 | c = NumberToken                                                                                                            {IntegerNode(c)}
 | SimpleQuoteToken c = CharValueToken SimpleQuoteToken                                                                   {CharNode (c)}
 | DoubleQuoteToken c = IdentToken DoubleQuoteToken                                                                       {StringNode (c)}
 | TrueToken                                                                                                              {TrueNode}
 | FalseToken                                                                                                             {FalseNode}
(* Channel *)

value :
 | c = cst                                                                                                              {ValueNode (c)}              
 | LeftParenthesisToken vs = valueSeq RightParenthesisToken                                                             {ValueNode (vs)}
 | LeftSqBracketToken vs = valueSeq RightSqBracketToken                                                                 {ValueNode (vs)}

valueSeq :
 | v = value                                                                                                            {ValueSeqNode (v,None)}
 | v = value ComaToken vs = valueSeq                                                                                    {ValueSeqNode (v,Some(vs))}

assignable :
 | n = IdentToken                                                                                                         {AssignNode (StringNode(n))} 
 | LeftParenthesisToken ns = assignableSeq RightParenthesisToken                                                          {AssignNode (ns)} 

assignableSeq :
 | n = IdentToken                                                                                                          {AssignSeqNode (StringNode(n),None)}
 | n = IdentToken ComaToken ns = assignableSeq                                                                             {AssignSeqNode (StringNode(n),Some(ns))}

typ :
 | IntegerToken                                                                                                            {TypeNode (IntegerT)}
 | BoolToken                                                                                                              {TypeNode (BooleanT)}    
 | StringToken                                                                                                            {TypeNode (StringT)}
 | CharToken                                                                                                              {TypeNode (CharT)}
 | ChannelToken t = typ                                                                                                  {ChannTNode (t)}
 | ListToken LeftSqBracketToken t = typ RightSqBracketToken                                                                      {ListTNode (t)}
 | LeftParenthesisToken t1 = typ ComaToken t2 = types RightParenthesisToken                                              {TupleTNode (t1,Some(t2))}

types :
 | t = typ                                                                                                            {TupleTNode (t,None)}
 | t1 = typ ComaToken t2 = types                                                                                      {TupleTNode (t1, Some(t2))}

funcType :
 | VoidToken                                                                                                          {FuncTNode (None)}
 | t = typ                                                                                                            {FuncTNode (Some (t))}

callMain :
 | f = IdentToken LeftParenthesisToken e = expr RightParenthesisToken                                             {CallNode (None,f,e)} 