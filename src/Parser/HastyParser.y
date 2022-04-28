{
module Parser.HastyParser(parseFile,parse) where
import Parser.HastyLexer as HastyLexer
import IR.Hasty as Hasty
}

%name parse Program
%tokentype { Token }
%error { parseError }

%token
 BOOL       { BOOL }
 ELSE       { ELSE }
 FUNC       { FUNC }
 IF         { IF }
 INT        { INT }
 NIL        { NIL }
 PRINT      { PRINT }
 RETURN     { RETURN }
 STRING     { STRING }
 WHILE      { WHILE }
 PLUS       { PLUS }
 MINUS      { MINUS }
 STAR       { STAR }
 SLASH      { SLASH }
 LPAREN     { LPAREN }
 RPAREN     { RPAREN }
 LBRACE     { LBRACE }
 RBRACE     { RBRACE }
 LBRACKET   { LBRACKET }
 RBRACKET   { RBRACKET }
 SEMI       { SEMI }
 ASSIGN     { ASSIGN }
 COMMA      { COMMA }
 QUERY      { QUERY }
 COLON      { COLON }
 BANG       { BANG }
 BCONST     { BCONST $$ }
 ICONST     { ICONST $$ }
 SCONST     { SCONST $$ }
 IDENT      { IDENT $$ }
 EQEQ       { EQEQ }
 NEQEQ      { NEQEQ }
 LESS       { LESS }
 GREATER    { GREATER }
 LESSEQ     { LESSEQ }
 GREATEREQ  { GREATEREQ }
 ANDAND     { ANDAND }
 OROR       { OROR }
 QUERYQUERY { QUERYQUERY }
 VAR        { VAR }
 ARROW      { ARROW }
 VOID       { VOID }
 DOT        { DOT }
 CAST       { CAST }
 CLASSNAME  { CLASSNAME $$ }
 CLASS      { CLASS }
 INIT       { INIT }
 STATIC     { STATIC }
 SUPER      { SUPER }
 OVERRIDE   { OVERRIDE }

%right QUERY COLON
%left OROR
%left ANDAND
%nonassoc EQEQ NEQEQ LESS GREATER LESSEQ GREATEREQ
%right QUERYQUERY
%left PLUS MINUS
%left STAR SLASH
%right BANG UNARY_MINUS
%left DOT

%%

Program :: { Program }
Program: Declarations { $1 }

Statement :: { Stmt }
Statement: IF Expression Block                          { SIf $2 $3 (SBlock []) }
         | IF Expression Block ELSE Statement           { SIf $2 $3 $5 }
         | Block                                        { $1 }
         | WHILE Expression Block                       { SWhile $2 $3 }
         | RETURN SEMI                                  { SReturn Nothing }
         | RETURN Expression SEMI                       { SReturn (Just $2) }
         | PRINT LPAREN Expression RPAREN SEMI          { SPrint $3 }
         | Expression ASSIGN Expression SEMI            { SAssign $1 $3 }
         | VAR IDENT COLON Type ASSIGN Expression SEMI  { SVarDecl $2 $4 $6 }
         | Expression SEMI                              { SExpr $1 }

Statements :: { [Stmt] }
Statements :                      { [] }
           | Statement Statements { $1 : $2 }

Block :: { Stmt }
Block : LBRACE Statements RBRACE  { SBlock $2 }

Expression :: { Expr }
Expression: ICONST                                         { EConstI $1 }
          | BCONST                                         { EConstB $1 }
          | SCONST                                         { EConstS $1 }
          | IDENT                                          { EVar $1 }
          
          | MINUS Expression %prec UNARY_MINUS             { EUop NegOp $2 }
          | BANG Expression                                { EUop NotOp $2 }
          
          | Expression PLUS Expression                     { EBop $1 PlusOp $3 }
          | Expression MINUS Expression                    { EBop $1 MinusOp $3 }
          | Expression STAR Expression                     { EBop $1 TimesOp $3 }
          | Expression SLASH Expression                    { EBop $1 DivOp $3 }
          
          | Expression EQEQ Expression                     { EBop $1 EqOp $3 }
          | Expression NEQEQ Expression                    { EBop $1 NeOp $3 }
          | Expression LESS Expression                     { EBop $1 LtOp $3 }
          | Expression GREATER Expression                  { EBop $1 GtOp $3 }
          | Expression LESSEQ Expression                   { EBop $1 LeOp $3 }
          | Expression GREATEREQ Expression                { EBop $1 GeOp $3 }
          
          | Expression ANDAND Expression                   { EBop $1 AndOp $3 }
          | Expression OROR Expression                     { EBop $1 OrOp $3 }

          | Expression DOT IDENT                           { EProj $1 $3 }

          | CAST Type LPAREN Expression RPAREN             { EConvert $2 $4 }

          | NIL                                            { ENil }
          | Expression QUERYQUERY Expression               { ECoalesce $1 $3 }

          | Expression QUERY Expression COLON Expression   { ETernary $1 $3 $5 }

          | IDENT LPAREN Expressionz RPAREN                { ECall $1 $3 }

          | LPAREN Expression RPAREN                       { $2 }

          | CLASSNAME DOT IDENT LPAREN Expressionz RPAREN  { EStaticCall $1 $3 $5 }
          | Expression DOT IDENT LPAREN Expressionz RPAREN { EInvoke $1 $3 $5 }
          | CLASSNAME LPAREN Expressionz RPAREN            { ENew $1 $3 }

          | LBRACKET Recordz RBRACKET                      { ERecord $2 }


Expressionz:   { [] }
           |   Expressions { $1 }

Expressions: Expression { [$1] }
           | Expression COMMA Expressions { $1 : $3 }

Type :: { Type }
Type : INT                      { IntTy }
     | BOOL                     { BoolTy }
     | STRING                   { StringTy }
     | Type QUERY               { OptionalTy $1 }
     | VOID                     { VoidTy }
     | CLASSNAME                { ClassTy $1 }
     | LBRACKET Parameterz RBRACKET { RecordTy $2 }

Declaration :: { Declaration }
Declaration: FUNC IDENT LPAREN Parameterz RPAREN ARROW Type Block { DeclFunc $2 $4 $7 $8 }
           | CLASS CLASSNAME Superclassz LBRACE Fieldz Constructor Methodz RBRACE 
               { DeclClass $2 $3 $5 $6 $7 }

Declarations: { [] }
            | Declaration Declarations { $1 : $2 }

Parameterz:: { [(Ident, Type)] }
Parameterz : { [] }
          | Parameters { $1 }

Parameters : Parameter { [$1] }
          | Parameter COMMA Parameters { $1 : $3 }

Parameter: IDENT COLON Type { ($1, $3) }

Recordz:: { [(Ident, Expr)] }
Recordz : { [] }
          | Records { $1 }

Records : Record { [$1] }
        | Record COMMA Records { $1 : $3 }

Record: IDENT ASSIGN Expression { ($1, $3) }

Superclassz :: { [ClassName] }
Superclassz :                    { [] }
            | COLON Superclasses { $2 }
 
Superclasses : CLASSNAME                    { [$1] }
             | CLASSNAME COMMA Superclasses { $1 : $3 }

Field: VAR IDENT COLON Type SEMI   { ($2, $4) }

Fieldz:: { [(Ident, Type)] }
Fieldz: { [] }
      | Field Fieldz { $1: $2 }

Constructor :: { Constructor }
Constructor: INIT LPAREN Parameterz RPAREN SuperInitz Block { ($3, $5, $6) }

SuperInitz :: { [SuperInit] }
SuperInitz :                  { [] }
           | COLON SUPER LPAREN Expressionz RPAREN { [(Nothing, $4)] }
           | COLON NamedSuperInits { $2 }

NamedSuperInits :: { [SuperInit] }
NamedSuperInits : NamedSuperInit { [$1] }
                | NamedSuperInit COMMA NamedSuperInits { $1 : $3 }

NamedSuperInit :: { SuperInit }
NamedSuperInit : CLASSNAME LPAREN Expressionz RPAREN { (Just $1, $3) }

Method :: { Method }
Method: MethodKind FUNC IDENT LPAREN Parameterz RPAREN ARROW Type Block         
         { ($1, $3, $5, $8, $9) }

Methodz:: { [Method] }
Methodz: { [] }
       | Method Methodz { $1: $2 }

MethodKind :: { MethodKind }         
MethodKind:          { Virtual }
          | OVERRIDE { Override }
          | STATIC   { Static }
{

parseError :: [Token] -> a
parseError toks =
    error ("Parse error" ++ lcn ++ "\n")
      where  lcn = case toks of
                    []   -> " at end of file"
                    toks -> ", before tokens " ++ show (take 10 toks)

parseFile :: String -> IO Program
parseFile filename =
    do sourceCode <- readFile filename
       let tokenList = alexScanTokens sourceCode
       let ast = parse tokenList
       return ast

}
