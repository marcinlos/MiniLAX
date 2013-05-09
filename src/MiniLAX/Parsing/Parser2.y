{
module MiniLAX.Parsing.Parser2 (parse) where

import MiniLAX.Parsing.LexerCore
import MiniLAX.Parsing.ParserCore
import MiniLAX.Location
import MiniLAX.Compiler
import MiniLAX.AST.Annotated
import MiniLAX.AST.Util
import MiniLAX.Static.Types (ParamKind (..))

import MiniLAX.Diagnostic
}

%name doParse
%tokentype { Token }
%error { parseError }
%monad { CompilerT IO }


%left NOT_P
%left '*'
%left '+'
%left '<'

%token
  Id                { Token (Id _) _ _ }
  IntConst          { Token (Int _) _ _ }
  RealConst         { Token (Float _) _ _ }

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
  "}:->"            { Token (Sym "}:->") _ _ }

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

Program :: { Program Location } df
  : "PROGRAM" Id ';' Block '.'              { Program (tkPos $1) (mkName $2) $4 }
  
ProcDecl :: { Decl Location }
  : ProcHead ';' Block                      { ProcDecl (attr $1) $1 $3 }
  
Block :: { Block Location }
  : "DECLARE" DeclSeq 
    "BEGIN" StatSeq "END"                   { Block (tkPos $1) $2 $4 }
  
DeclSeq :: { [Decl Location] }
  : DeclSeqR                                { reverse $1 }
  
DeclSeqR :: { [Decl Location] }
  : Decl                                    { [$1] }
  | DeclSeqR ';' error                          {% errDeclSemi $2 $1 } 
  | DeclSeqR ';' Decl                       { $3 : $1 }
  
Decl :: { Decl Location }
  : VarDecl                                 { $1 }
  | ProcDecl                                { $1 }
  
ProcHead :: { ProcHead Location }
  : "PROCEDURE" Id                          { ProcHead (tkPos $1) (mkName $2) [] }
  | "PROCEDURE" Id '(' FormalSeq ')'        { ProcHead (tkPos $1) (mkName $2) $4 }
  
FormalSeq :: { [Formal Location] }
  : FormalSeqR                              { reverse $1 }
  
FormalSeqR :: { [Formal Location] }
  : Formal                                  { [$1] }
  | FormalSeqR ';' Formal                   { $3 : $1 }
  
Formal :: { Formal Location }
  : "VAR" Id ':' Type                       { Formal (tkPos $1) ByVar (mkName $2) $4 }
  | Id ':' Type                             { Formal (tkPos $1) ByVal (mkName $1) $3 }
  
Type :: { Type Location }
  : SimpleType                              { $1 }
  | ArrayType                               { $1 }
 
SimpleType :: { Type Location }
  : "INTEGER"                               { TyInt (tkPos $1) }
  | "REAL"                                  { TyReal (tkPos $1) }
  | "BOOLEAN"                               { TyBoolean (tkPos $1) }
  
ArrayType :: { Type Location }
  : "ARRAY" '[' IntConst ".." IntConst ']' 
    "OF" Type                               { TyArray (tkPos $1) $8 (mkLit $3) (mkLit $5) }
  
VarDecl :: { Decl Location }
  : Id ':' Type                             { VarDecl (tkPos $1) (mkName $1) $3 }
  
Var :: { Variable Location }
  : Id                                      { VarName (tkPos $1) (mkName $1) }
  | Var '[' Expr ']'                        { VarIndex (attr $1) $1 $3 }
  
Expr :: { Expr Location }
  : Expr '+' Expr                           { mkBin $1 $2 $3 }
  | Expr '*' Expr                           { mkBin $1 $2 $3 }
  | Expr '<' Expr                           { mkBin $1 $2 $3 }
  | "NOT" Expr %prec NOT_P                  { UnaryExpr (tkPos $1) (Not $ tkPos $1) $2 }
  | '(' Expr ')'                            { $2 }
  | Var                                     { VarExpr (attr $1) $1 }
  | IntConst                                { mkLitExpr $1 }
  | RealConst                               { mkLitExpr $1 }
  | "TRUE"                                  { mkLitExpr $1 }
  | "FALSE"                                 { mkLitExpr $1 }
  | "}:->"                                  { mkLitExpr $1 }
  
StatSeq :: { [Stmt Location] }
  : StatSeqR                                { reverse $1 }
  
StatSeqR :: { [Stmt Location] }
  : Stat                                    { [$1] }
  | StatSeqR ';' error                      {% errStmtSemi $2 $1 }  
  | StatSeqR ';' Stat                       { $3 : $1 }
  
Stat :: { Stmt Location }
  : AssignStat                              { $1 }
  | ProcStat                                { $1 }
  | CondStat                                { $1 }
  | LoopStat                                { $1 }
  
AssignStat :: { Stmt Location }
  : Var ":=" Expr                           { Assignment (attr $1) $1 $3 }
  
ProcStat :: { Stmt Location }
  : Id                                      { ProcCall (tkPos $1) (mkName $1) [] }
  | Id '(' ExprSeq ')'                      { ProcCall (tkPos $1) (mkName $1) $3 }
  
  
ExprSeq :: { [Expr Location] }
  : ExprSeqR                                { reverse $1 }
  
ExprSeqR :: { [Expr Location] }
  : Expr                                    { [$1] }
  | ExprSeqR ',' Expr                       { $3 : $1 }
  
CondStat :: { Stmt Location }
  : "IF" Expr "THEN" StatSeq 
    "ELSE" StatSeq "END"                    { IfThenElse (tkPos $1) $2 $4 $6 }
  
LoopStat :: { Stmt Location }
  : "WHILE" Expr "DO" StatSeq "END"         { While (tkPos $1) $2 $4 }
  
  
{ 

parse :: [Token] -> CompilerT IO (Program Location)
parse = doParse

}


