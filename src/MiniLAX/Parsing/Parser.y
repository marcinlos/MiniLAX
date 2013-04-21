{
module MiniLAX.Parsing.Parser where

import MiniLAX.Parsing.LexerCore
import MiniLAX.Parsing.ParserCore
import MiniLAX.Location
import MiniLAX.Compiler
import MiniLAX.AST as AST
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { Compiler }

%left NOT_P
%left '*'
%left '+'
%left '<'

%token
  Id                { Token (Id $$) _ _ }
  IntConst          { Token (Int $$) _ _ }
  RealConst         { Token (Float $$) _ _ }

  ':'               { Token (Sym ":") _ _ }  
  ';'               { Token (Sym ";") _ _ }
  '+'               { Token (Sym "+") _ _ }  
  '*'               { Token (Sym "*") _ _ }
  '('               { Token (Sym "(") _ _ }
  ')'               { Token (Sym ")") _ _ }
  '.'               { Token (Sym ".") _ _ }
  ','               { Token (Sym ",") _ _ }
  '['               { Token (Sym "[") _ _ }
  ']'               { Token (Sym "]") _ _ }
  '<'               { Token (Sym "<") _ _ }
  ":="              { Token (Sym ":=") _ _ }
  ".."              { Token (Sym "..") _ _ }

  "ARRAY"           { Token (Keyword "ARRAY") _ _ }
  "BEGIN"           { Token (Keyword "BEGIN") _ _ }
  "BOOLEAN"         { Token (Keyword "BOOLEAN") _ _ }
  "DECLARE"         { Token (Keyword "DECLARE") _ _ }
  "DO"              { Token (Keyword "DO") _ _ }
  "ELSE"            { Token (Keyword "ELSE") _ _ }
  "END"             { Token (Keyword "END") _ _ }
  "FALSE"           { Token (Keyword "FALSE") _ _ }
  "IF"              { Token (Keyword "IF") _ _ }
  "INTEGER"         { Token (Keyword "INTEGER") _ _ }
  "NOT"             { Token (Keyword "NOT") _ _ }
  "OF"              { Token (Keyword "OF") _ _ }
  "PROCEDURE"       { Token (Keyword "PROCEDURE") _ _ }
  "PROGRAM"         { Token (Keyword "PROGRAM") _ _ }
  "REAL"            { Token (Keyword "REAL") _ _ }
  "THEN"            { Token (Keyword "THEN") _ _ }
  "TRUE"            { Token (Keyword "TRUE") _ _ }
  "VAR"             { Token (Keyword "VAR") _ _ }
  "WHILE"           { Token (Keyword "WHILE") _ _ }
  
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
  
