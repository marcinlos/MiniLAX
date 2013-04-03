{
module MiniLAX.Parsing.Lexer (
    Token (..), 
    alexScanTokens
) where
}

%wrapper "basic"

$letter = [a-zA-Z] 
$digit  = 0-9

@id             = $letter ($letter | $digit)*
@int_const      = $digit $digit*
@scale_factor   = "E" ("+" | "-") ? @int_const
@real_const     = (@int_const ?) "." @int_const 

tokens :-

  $white+                          ;
  "(*".*"*)"                       ;
  @int_const                       { \s -> Int (read s) }
  @real_const                      { \s -> Float (readFloat s) }
  [\:\;\=\+\-\*\/\(\)\.\,\[\]\<]   { \s -> Sym s }
  ":="                             { \s -> Sym s }
  ".."                             { \s -> Sym s }
  
  "ARRAY" | "BEGIN" | "BOOLEAN" | "DECLARE" | "DO" | "ELSE" | "END" |
  "FALSE" | "IF" | "INTEGER" | "NOT"| "OF" | "PROCEDURE" | "PROGRAM" |
  "READ" | "REAL" | "THEN" | "TRUE" | "VAR" | "WHILE" | "WRITE"
                                   { \s -> Keyword s }
  
  @id                              { \s -> Id s }

{

data Token =
    Sym String
  | Id String
  | Int Int 
  | Float Float
  | Keyword String
  | Err 
  deriving (Eq,Show)

readFloat :: String -> Float
readFloat s @ ('.' : _) = read ('0' : s)
readFloat s = read s

}
