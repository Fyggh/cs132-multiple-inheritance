{
module Parser.HastyLexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

tokens :-

  [\ \t\n]+  ;
  "//".*\n   ;

  ";"       { \_ -> SEMI }
  "+"       { \_ -> PLUS }
  "-"       { \_ -> MINUS }
  "*"       { \_ -> STAR }
  "/"       { \_ -> SLASH }
  "("       { \_ -> LPAREN }
  ")"       { \_ -> RPAREN }
  "="       { \_ -> ASSIGN }
  "{"       { \_ -> LBRACE }
  "}"       { \_ -> RBRACE }
  "["       { \_ -> LBRACKET }
  "]"       { \_ -> RBRACKET }
  "?"       { \_ -> QUERY }
  "!"       { \_ -> BANG }
  ","       { \_ -> COMMA }
  ":"       { \_ -> COLON }
  "."       { \_ -> DOT }
  "->"      { \_ -> ARROW }

  "else"     { \_ -> ELSE }
  "func"     { \_ -> FUNC }
  "if"       { \_ -> IF }
  "nil"      { \_ -> NIL }
  "print"    { \_ -> PRINT }
  "return"   { \_ -> RETURN }
  "while"    { \_ -> WHILE }
  "var"      { \_ -> VAR }

  "true"    { \_ -> BCONST True }
  "false"   { \_ -> BCONST False }

  "Int"     { \_ -> INT }
  "Bool"    { \_ -> BOOL }
  "String"  { \_ -> STRING }
  "Void"    { \_ -> VOID }

  "=="      { \_ -> EQEQ }
  "!="      { \_ -> NEQEQ }
  "<"       { \_ -> LESS }
  ">"       { \_ -> GREATER }
  "<="      { \_ -> LESSEQ }
  ">="      { \_ -> GREATEREQ }

  "&&"      { \_ -> ANDAND }
  "||"      { \_ -> OROR }

  "??"      { \_ -> QUERYQUERY }

  "cast"    { \_ -> CAST }

  "class"   { \_ -> CLASS }
  "init"    { \_ -> INIT }
  "static"  { \_ -> STATIC }
  "super"   { \_ -> SUPER }
  "override"{ \_ -> OVERRIDE }

  [0-9]+                { \yytext -> ICONST (read yytext) }
  "0x"[0-9A-Za-z]+      { \yytext -> ICONST (read yytext) }
  [a-z][a-zA-Z0-9]*     { \yytext -> IDENT yytext }
  [A-Z][a-zA-Z0-9]*     { \yytext -> CLASSNAME yytext }
  [\"]([^\\\"]|\\.)*[\"]        { \yytext -> SCONST (unescape $ init $ tail yytext) }


  .         { \yytext -> -- THE DOT RULE SHOULD BE THE LAST, SO IT ONLY
                         -- APPLIES IF ALL OTHER LEXER RULES FAIL
                         -- i.e., the next part of the input doesn't start
                         -- with any recognizable token.
                         -- Print an error message and die.
                         error ("Unexpected lexer input: " ++ show yytext)
            }

{

data Token =
    ELSE | FUNC | IF | PRINT | RETURN | WHILE | NIL
  | SEMI | COMMA | QUERY | COLON | BANG
  | PLUS | MINUS | STAR | SLASH
  | ASSIGN
  | EQEQ | NEQEQ | LESS | GREATER | LESSEQ | GREATEREQ
  | ANDAND | OROR
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET
  | DOT | ARROW

  | BCONST Bool
  | ICONST Integer
  | SCONST String

  | INT | BOOL | STRING | VOID

  | IDENT String
  | VAR
  | QUERYQUERY

  | CAST

  | CLASSNAME String
  | CLASS | INIT | STATIC | SUPER | OVERRIDE
  deriving (Show)

unescape :: String -> String
unescape [] = []
unescape ('\\' : 'n' : cs) = '\n' : unescape cs
unescape ('\\' : 't' : cs) = '\t' : unescape cs
unescape ('\\' : '\\' : cs) = '\\' : unescape cs
unescape ('\\' : '\"' : cs) = '\"' : unescape cs
unescape (c : cs) = c : unescape cs

}
