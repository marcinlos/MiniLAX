{
module MiniLAX.Parsing.Parser where

import MiniLAX.Parsing.Lexer
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { ParseMonad }

%left NOT_P
%left '*'
%left '+'
%left '<'

%token 
  Id                { Id _ $$ }
  IntConst          { Int _ $$ }
  RealConst         { Float _ $$ }
                    
  ':'               { Sym _ ":" }  
  ';'               { Sym _ ";" }
  '+'               { Sym _ "+" }  
  '*'               { Sym _ "*" }
  '('               { Sym _ "(" }
  ')'               { Sym _ ")" }
  '.'               { Sym _ "." }
  ','               { Sym _ "," }
  '['               { Sym _ "[" }
  ']'               { Sym _ "]" }
  '<'               { Sym _ "<" }
  ":="              { Sym _ ":=" }
  ".."              { Sym _ ".." }
  
  "ARRAY"           { Keyword _ "ARRAY" }
  "BEGIN"           { Keyword _ "PROGRAM" }
  "BOOLEAN"         { Keyword _ "BOOLEAN" }
  "DECLARE"         { Keyword _ "DECLARE" }
  "DO"              { Keyword _ "DO" }
  "ELSE"            { Keyword _ "ELSE" }
  "END"             { Keyword _ "END" }
  "FALSE"           { Keyword _ "FALSE" }
  "IF"              { Keyword _ "IF" }
  "INTEGER"         { Keyword _ "INTEGER" }
  "NOT"             { Keyword _ "NOT" }
  "OF"              { Keyword _ "OF" }
  "PROCEDURE"       { Keyword _ "PROCEDURE" }
  "PROGRAM"         { Keyword _ "PROGRAM" }
  "REAL"            { Keyword _ "REAL" }
  "THEN"            { Keyword _ "THEN" }
  "TRUE"            { Keyword _ "TRUE" }
  "VAR"             { Keyword _ "VAR" }
  "WHILE"           { Keyword _ "WHILE" }
  
%%

Program :: { String }
  : "PROGRAM" Id ';' Block '.'              { undefined }
  
ProcDecl :: { String }
  : ProcHead ';' Block                      { undefined }
  
Block :: { String }
  : "DECLARE" DeclSeq "BEGIN" StatSeq "END" { undefined }
  
DeclSeq :: { String }
  : Decl                                    { undefined }
  | DeclSeq ';' Decl                        { undefined }
  
Decl :: { String }
  : VarDecl                                 { undefined }
  | ProcDecl                                { undefined }
  
ProcHead :: { String }
  : "PROCEDURE" Id                          { undefined }
  | "PROCEDURE" Id '(' FormalSeq ')'        { undefined }
  
FormalSeq :: { String }
  : Formal                                  { undefined }
  | FormalSeq ';' Formal                    { undefined }
  
Formal :: { String }
  : "VAR" Id ':' Type                       { undefined }
  | Id ':' Type                             { undefined }
  
Type :: { String }
  : SimpleType                              { undefined }
  | ArrayType                               { undefined }
 
SimpleType :: { String }
  : "INTEGER"                               { undefined }
  | "REAL"                                  { undefined }
  | "BOOLEAN"                               { undefined }
  
ArrayType :: { String }
  : "ARRAY" '[' IntConst ".." IntConst ']' "OF" Type    { undefined }
  
VarDecl :: { String }
  : Id ':' Type                             { undefined }
  
Var :: { String }
  : Id                                      { undefined }
  | Var '[' Expr ']'                        { undefined }
  
Expr :: { String }
  : Expr '+' Expr                           { undefined }
  | Expr '*' Expr                           { undefined }
  | Expr '<' Expr                           { undefined }
  | "NOT" Expr %prec NOT_P                  { undefined }
  | '(' Expr ')'                            { undefined }
  | Var                                     { undefined }
  | IntConst                                { undefined }
  | RealConst                               { undefined }
  | "TRUE"                                  { undefined }
  | "FALSE"                                 { undefined }
  
StatSeq :: { String }
  : Stat                                    { undefined }
  | StatSeq ';' Stat                        { undefined }
  
Stat :: { String }
  : AssignStat                              { undefined }
  | ProcStat                                { undefined }
  | CondStat                                { undefined }
  | LoopStat                                { undefined }
  
AssignStat :: { String }
  : Var ":=" Expr                           { undefined }
  
ProcStat :: { String }
  : Id                                      { undefined }
  | Id '(' ExprSeq ')'                      { undefined }
  
ExprSeq :: { String }
  : Expr                                    { undefined }
  | ExprSeq ',' Expr                        { undefined }
  
CondStat :: { String }
  : "IF" Expr "THEN" StatSeq "ELSE" StatSeq "END" { undefined }
  
LoopStat :: { String }
  : "WHILE" Expr "DO" StatSeq "END"         { undefined }
  

{

-- | Type of error message
type ParseError = String

type ParseMonad = Either ParseError
 
parseError :: [Token] -> a
parseError tokens = 
    error $ "Parse error at " ++ (showPos pos) ++ "[" ++ token ++ "]"
    where (token, pos) = case tokens of
              t : _ -> ("(" ++ (show t) ++ ")", tokenPos t)
              _     -> ("", AlexPn 0 0 0) 

}

