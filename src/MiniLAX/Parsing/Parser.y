{
module MiniLAX.Parsing.Parser where

import MiniLAX.Parsing.Lexer
import qualified MiniLAX.AST as AST
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

Program :: { AST.Program }
  : "PROGRAM" Id ';' Block '.'              { undefined }
  
ProcDecl :: { AST.Decl }
  : ProcHead ';' Block                      { undefined }
  
Block :: { AST.Block }
  : "DECLARE" DeclSeq "BEGIN" StatSeq "END" { undefined }
  
DeclSeq :: { AST.DeclSeq }
  : Decl                                    { undefined }
  | DeclSeq ';' Decl                        { undefined }
  
Decl :: { AST.Decl }
  : VarDecl                                 { undefined }
  | ProcDecl                                { undefined }
  
ProcHead :: { (String, AST.FormalSeq) }
  : "PROCEDURE" Id                          { undefined }
  | "PROCEDURE" Id '(' FormalSeq ')'        { undefined }
  
FormalSeq :: { AST.FormalSeq }
  : Formal                                  { undefined }
  | FormalSeq ';' Formal                    { undefined }
  
Formal :: { AST.Formal }
  : "VAR" Id ':' Type                       { undefined }
  | Id ':' Type                             { undefined }
  
Type :: { AST.Type }
  : SimpleType                              { undefined }
  | ArrayType                               { undefined }
 
SimpleType :: { AST.Type }
  : "INTEGER"                               { undefined }
  | "REAL"                                  { undefined }
  | "BOOLEAN"                               { undefined }
  
ArrayType :: { AST.Type }
  : "ARRAY" '[' IntConst ".." IntConst ']' "OF" Type    { undefined }
  
VarDecl :: { AST.Decl }
  : Id ':' Type                             { undefined }
  
Var :: { AST.Var }
  : Id                                      { undefined }
  | Var '[' Expr ']'                        { undefined }
  
Expr :: { AST.Expr }
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
  
StatSeq :: { AST.StatSeq }
  : Stat                                    { undefined }
  | StatSeq ';' Stat                        { undefined }
  
Stat :: { AST.Stat }
  : AssignStat                              { undefined }
  | ProcStat                                { undefined }
  | CondStat                                { undefined }
  | LoopStat                                { undefined }
  
AssignStat :: { AST.Stat }
  : Var ":=" Expr                           { undefined }
  
ProcStat :: { AST.Stat }
  : Id                                      { undefined }
  | Id '(' ExprSeq ')'                      { undefined }
  
ExprSeq :: { AST.ExprSeq }
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
    error $ "Parse error at " ++ (showPos pos) ++ " [at " ++ token ++ "]"
    where (token, pos) = case tokens of
              t : _ -> ("(" ++ (show t) ++ ")", tokenPos t)
              _     -> ("", AlexPn 0 0 0) 

}

