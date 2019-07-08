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
 | pos = FunctionToken ft = funcType n = IdentToken LeftParenthesisToken params = parameters RightParenthesisToken b = body       {FunctionNode (pos,ft,snd(n),params,b)}

 typeDecla :
 | pos = TypeToken tname = IdentToken AssignToken t = typ                                                          {TypeDeclaNode (pos,snd(tname),t)}

parameters : 
 | t = typ n = IdentToken                                                                                   {ParamsNode (fst(n),t,snd(n),None)} 
 | t = typ n = IdentToken ComaToken params = parameters                                                     {ParamsNode (fst(n),t,snd(n),Some(params))} 

body :
 | pos = LeftBracketToken i = instruction RightBracketToken                                                     {BodyNode (pos,None,i)}
 | pos = LeftBracketToken DefToken v = variableDeclas InToken i = instruction RightBracketToken                 {BodyNode (pos,Some(v),i)}

variableDecla :
 | t = typ n = IdentToken                                                                                   {VariableDeclaNode (fst(n),t,StringNode(fst(n),snd(n)))}
 | t = typ pos = LeftParenthesisToken idents = tupleDecla RightParenthesisToken                              {VariableDeclaNode (pos,t,idents)}

tupleDecla : 
 | n = IdentToken                                                                                           {TupleDeclaNode (fst(n),snd(n),None)} 
 | n = IdentToken ComaToken idents = tupleDecla                                                             {TupleDeclaNode (fst(n),snd(n),Some(idents))} 

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
 | a = expr pos = AssignToken e = expr                                                                                  {BinaryNode (pos,a,Assign,e)}
 | a = expr pos = AssignToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken              {BinaryNode (pos,a,Assign,CallNode (fst(f),snd(f),e))} 
 | f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                              {CallNode (fst(f),snd(f),e)}
 | a = expr pos = AssignToken ReceiveToken LeftParenthesisToken n = IdentToken RightParenthesisToken                  {ReceiveNode (pos,a,snd(n))}
 | pos = SendToken LeftParenthesisToken n = IdentToken ComaToken e = expr RightParenthesisToken                            {SendNode (pos,snd(n),e)}
 | pos = IfToken LeftParenthesisToken cond = expr RightParenthesisToken 
      LeftBracketToken i1 = instruction RightBracketToken 
      ElseToken LeftBracketToken i2 = instruction RightBracketToken                                             {IfthenelseInstrNode (pos,cond,i1,i2)}
 | pos = WhileToken LeftParenthesisToken e = expr RightParenthesisToken LeftBracketToken i = instruction RightBracketToken          {WhileNode (pos,e,i)}
 | pos = ChooseToken LeftBracketToken c = choices RightBracketToken                                                     {ChooseNode (pos,c)}
 | pos = SpawnToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                         {SpawnNode (pos,snd(f),e)}
 | a = expr pos = AssignToken NewToken LeftParenthesisToken RightParenthesisToken                                         {NewNode (pos,a)}
 | pos = ReturnToken                                                                                                          {ReturnNode (pos,None)}
 | pos = ReturnToken e = expr                                                                                                 {ReturnNode (pos,Some (e))}
 | pos = ReturnToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                      {ReturnNode (pos,Some (CallNode (fst(f),snd(f),e)))}

choices : 
 | p = prefix pos = ArrowToken LeftBracketToken i = instruction RightBracketToken cs = choices                                  {ChoicesNode(pos,p,i,Some(cs))}
 | p = prefix pos = ArrowToken LeftBracketToken i = instruction RightBracketToken                                            {ChoicesNode(pos,p,i,None)}

prefix :
 | pos = ChoiceToken TauToken                                                                                                                {PrefixNode(pos,None,Tau,None,None)}
 | pos = ChoiceToken SendToken LeftParenthesisToken n = IdentToken ComaToken e = expr RightParenthesisToken                                {PrefixNode(pos,None,Send,Some(snd(n)),Some(e))}
 | pos = ChoiceToken a = expr AssignToken ReceiveToken LeftParenthesisToken n = IdentToken RightParenthesisToken                         {PrefixNode(pos,Some(a),Receive,Some(snd(n)),None)}
 | pos = ChoiceToken a = expr AssignToken NewToken LeftParenthesisToken RightParenthesisToken                                             {PrefixNode(pos,Some(a),New,None,None)}
 | pos = ChoiceToken SpawnToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                  {PrefixNode(pos,None,Spawn,Some(snd(f)),Some(e))}
 
expr :
 | pos = SubToken e = expr                                                                                   {UnaryNode (pos,Negate,e)}
 | pos = HeadToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (pos,Head,e)}                                                                                                                     
 | pos = TailToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (pos,Tail,e)}                                                                                                                    
 | pos = OddToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (pos,Odd,e)}                                                                                                                      
 | pos = EvenToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (pos,Even,e)}
 | pos = FstToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (pos,Fst,e)}                                                                                                                      
 | pos = SndToken LeftParenthesisToken e = expr RightParenthesisToken                                             {UnaryNode (pos,Snd,e)}  
 | e1 = expr pos = AddToken e2 = expr                                                                                                       {BinaryNode (pos,e1,Add,e2)}
 | e1 = expr pos = SubToken e2 = expr                                                                                                       {BinaryNode (pos,e1,Substract,e2)}
 | e1 = expr pos = OrToken e2 = expr                                                                                                        {BinaryNode (pos,e1,Or,e2)}
 | e1 = expr pos = MulToken e2 = expr                                                                                                       {BinaryNode (pos,e1,Multiply,e2)}
 | e1 = expr pos = DivToken e2 = expr                                                                                                       {BinaryNode (pos,e1,Divide,e2)}
 | e1 = expr pos = AndToken e2 = expr                                                                                                       {BinaryNode (pos,e1,And,e2)}
 | e1 = expr pos = EqualToken e2 = expr                                                                                                     {BinaryNode (pos,e1,Equal,e2)}
 | e1 = expr pos = DifferentToken e2 = expr                                                                                                 {BinaryNode (pos,e1,Different,e2)}
 | e1 = expr pos = LesserToken e2 = expr                                                                                                    {BinaryNode (pos,e1,Lesser,e2)}
 | e1 = expr pos = GreaterToken e2 = expr                                                                                                   {BinaryNode (pos,e1,Greater,e2)}
 | pos = IfToken LeftParenthesisToken cond = expr RightParenthesisToken 
      LeftBracketToken e1 = expr RightBracketToken ElseToken LeftBracketToken e2 = expr RightBracketToken                         {IfthenelseExprNode (pos,cond,e1,e2)}
 | pos = LeftParenthesisToken e = exprs RightParenthesisToken                                                                           {ExprNode (pos,e)}
 | v = value                                                                                                                      {ValueNode (v)} 
 | n = IdentToken                                                                                                                 {AssignNode (fst(n),snd(n))}

exprs :
 | e = expr                                                                                                                        {ExprsNode (e,None)}
 | e1 = expr ComaToken e2 = exprs                                                                                                  {ExprsNode (e1,Some(e2))}

cst : 
 | c = NumberToken                                                                                                        {IntegerNode(fst(c),snd(c))}
 | c = CharValueToken                                                                                                    {CharNode (fst(c),snd(c))}
 | DoubleQuoteToken c = IdentToken DoubleQuoteToken                                                                       {StringNode (fst(c),snd(c))}
 | pos = TrueToken                                                                                                              {TrueNode (pos)}
 | pos = FalseToken                                                                                                             {FalseNode (pos)}

value :
 | c = cst                                                                                                              {ValueNode (c)}              
 | LeftSqBracketToken vs = valueSeq RightSqBracketToken                                                                 {ValueNode (vs)}

valueSeq :
 | v = value                                                                                                            {ValueSeqNode (v,None)}
 | v = value ComaToken vs = valueSeq                                                                                    {ValueSeqNode (v,Some(vs))}

typ :
 | pos = IntegerToken                                                                                                            {TypeNode (pos,IntegerT)}
 | pos = BoolToken                                                                                                              {TypeNode (pos,BooleanT)}    
 | pos = StringToken                                                                                                            {TypeNode (pos,StringT)}
 | pos = CharToken                                                                                                              {TypeNode (pos,CharT)}
 | pos = ChannelToken t = typ                                                                                                   {ChanTNode (pos,t)}
 | pos = ListToken LeftSqBracketToken t = typ RightSqBracketToken                                                               {ListTNode (pos,t)}
 | pos = LeftParenthesisToken tSeq = types RightParenthesisToken                                                                {TupleTNode (pos,tSeq)}
 | t = IdentToken                                                                                                         {NamedTypeNode (fst(t),snd(t))}        

types :
 | t = typ                                                                                                            {TypeSeqNode (t,None)}
 | t1 = typ ComaToken t2 = types                                                                                      {TypeSeqNode (t1, Some(t2))}

funcType :
 | VoidToken                                                                                                          {FuncTNode (None)}
 | t = typ                                                                                                            {FuncTNode (Some (t))}

callMain :
 | StartToken f = IdentToken LeftParenthesisToken e = exprs RightParenthesisToken                                       {CallNode (fst(f),snd(f),e)} 