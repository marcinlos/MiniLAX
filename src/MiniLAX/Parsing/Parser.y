{
module MiniLAX.Parsing.Parser where

import MiniLAX.Parsing.Lexer
import MiniLAX.Parsing
import MiniLAX.AST as AST
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
                    
  ':'               { Sym $$ ":" }  
  ';'               { Sym $$ ";" }
  '+'               { Sym $$ "+" }  
  '*'               { Sym $$ "*" }
  '('               { Sym $$ "(" }
  ')'               { Sym $$ ")" }
  '.'               { Sym $$ "." }
  ','               { Sym $$ "," }
  '['               { Sym $$ "[" }
  ']'               { Sym $$ "]" }
  '<'               { Sym $$ "<" }
  ":="              { Sym $$ ":=" }
  ".."              { Sym $$ ".." }
  
  "ARRAY"           { Keyword $$ "ARRAY" }
  "BEGIN"           { Keyword $$ "BEGIN" }
  "BOOLEAN"         { Keyword $$ "BOOLEAN" }
  "DECLARE"         { Keyword $$ "DECLARE" }
  "DO"              { Keyword $$ "DO" }
  "ELSE"            { Keyword $$ "ELSE" }
  "END"             { Keyword $$ "END" }
  "FALSE"           { Keyword $$ "FALSE" }
  "IF"              { Keyword $$ "IF" }
  "INTEGER"         { Keyword $$ "INTEGER" }
  "NOT"             { Keyword $$ "NOT" }
  "OF"              { Keyword $$ "OF" }
  "PROCEDURE"       { Keyword $$ "PROCEDURE" }
  "PROGRAM"         { Keyword $$ "PROGRAM" }
  "REAL"            { Keyword $$ "REAL" }
  "THEN"            { Keyword $$ "THEN" }
  "TRUE"            { Keyword $$ "TRUE" }
  "VAR"             { Keyword $$ "VAR" }
  "WHILE"           { Keyword $$ "WHILE" }
  
%%

Program :: { Program }
  : "PROGRAM" Id ';' Block '.'              { Program $2 $4 }
  
ProcDecl :: { Decl }
  : ProcHead ';' Block                      { ProcDecl $1 $3 }
  
Block :: { Block }
  : "DECLARE" DeclSeq 
    "BEGIN" StatSeq "END"                   { Block $2 $4 }
  
DeclSeq :: { DeclSeq }
  : Decl                                    { [$1] }
  | DeclSeq ';' Decl                        { $3 : $1 }
  
Decl :: { Decl }
  : VarDecl                                 { $1 }
  | ProcDecl                                { $1 }
  
ProcHead :: { ProcHead }
  : "PROCEDURE" Id                          { ProcHead $2 [] }
  | "PROCEDURE" Id '(' FormalSeq ')'        { ProcHead $2 $4 }
  
FormalSeq :: { FormalSeq }
  : Formal                                  { [$1] }
  | FormalSeq ';' Formal                    { $3 : $1 }
  
Formal :: { Formal }
  : "VAR" Id ':' Type                       { Formal $2 $4 VarParam }
  | Id ':' Type                             { Formal $1 $3 ValParam }
  
Type :: { Type }
  : SimpleType                              { $1 }
  | ArrayType                               { $1 }
 
SimpleType :: { Type }
  : "INTEGER"                               { IntegerT }
  | "REAL"                                  { RealT }
  | "BOOLEAN"                               { BooleanT }
  
ArrayType :: { Type }
  : "ARRAY" '[' IntConst ".." IntConst ']' 
    "OF" Type                               { ArrayT $8 $3 $5 }
  
VarDecl :: { Decl }
  : Id ':' Type                             { VarDecl $1 $3 }
  
Var :: { Var }
  : Id                                      { VarId $1 }
  | Var '[' Expr ']'                        { VarIndex $1 $3 }
  
Expr :: { Expr }
  : Expr '+' Expr                           { BinaryExpr Plus $1 $3 }
  | Expr '*' Expr                           { BinaryExpr Times $1 $3 }
  | Expr '<' Expr                           { BinaryExpr Less $1 $3 }
  | "NOT" Expr %prec NOT_P                  { UnaryExpr Not $2 }
  | '(' Expr ')'                            { $2 }
  | Var                                     { VarExpr $1 }
  | IntConst                                { IntConst $1 }
  | RealConst                               { RealConst $1 }
  | "TRUE"                                  { BoolConst TrueL }
  | "FALSE"                                 { BoolConst FalseL }
  
StatSeq :: { StatSeq }
  : Stat                                    { [$1] }
  | StatSeq ';' Stat                        { $3 : $1 }
  
Stat :: { Stat }
  : AssignStat                              { $1 }
  | ProcStat                                { $1 }
  | CondStat                                { $1 }
  | LoopStat                                { $1 }
  
AssignStat :: { Stat }
  : Var ":=" Expr                           { AssignStat $1 $3 }
  
ProcStat :: { Stat }
  : Id                                      { ProcStat $1 [] }
  | Id '(' ExprSeq ')'                      { ProcStat $1 $3 }
  
ExprSeq :: { ExprSeq }
  : Expr                                    { [$1] }
  | ExprSeq ',' Expr                        { $3 : $1 }
  
CondStat :: { Stat }
  : "IF" Expr "THEN" StatSeq 
    "ELSE" StatSeq "END"                    { CondStat $2 $4 $6 }
  
LoopStat :: { Stat }
  : "WHILE" Expr "DO" StatSeq "END"         { LoopStat $2 $4 }
  



