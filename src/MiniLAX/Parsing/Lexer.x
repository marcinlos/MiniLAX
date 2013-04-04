{
module MiniLAX.Parsing.Lexer (
    Token (..), 
    alexScanTokens,
    AlexPosn (AlexPn),
    tokenPos,
    showPos
) where
}

%wrapper "posn"

$letter = [a-zA-Z] 
$digit  = 0-9

@id             = $letter ($letter | $digit)*
@int_const      = $digit $digit*
@scale_factor   = "E" ("+" | "-") ? @int_const
@real_const     = (@int_const ?) "." @int_const 

tokens :-

  $white+                          ;
  "(*" (. | \n)* "*)"              ;
  @int_const                       { \p s -> Int p (read s) }
  @real_const                      { \p s -> Float p (readFloat s) }
  [\:\;\=\+\-\*\/\(\)\.\,\[\]\<]   { \p s -> Sym p s }
  ":="                             { \p s -> Sym p s }
  ".."                             { \p s -> Sym p s }
  
  "ARRAY" | "BEGIN" | "BOOLEAN" | "DECLARE" | "DO" | "ELSE" | "END" |
  "FALSE" | "IF" | "INTEGER" | "NOT"| "OF" | "PROCEDURE" | "PROGRAM" |
  "READ" | "REAL" | "THEN" | "TRUE" | "VAR" | "WHILE" | "WRITE"
                                   { \s -> Keyword s }
  
  @id                              { \s -> Id s }

{

-- | Token structure
data Token =
    Sym AlexPosn String
  | Id AlexPosn String
  | Int AlexPosn Int 
  | Float AlexPosn Float
  | Keyword AlexPosn String
  | Err AlexPosn 
  deriving (Eq)
  
  
-- | Converts AlexPosn into a string "(line, col)"
showPos :: AlexPosn -> String
showPos (AlexPn _ line col) = "(" ++ (show line) ++ ", " ++ (show col) ++ ")"

instance Show Token where 
    show (Sym _ s)      = "'" ++ s ++ "'"
    show (Id _ s)       = "id " ++ s
    show (Int _ n)      = "int " ++ show n
    show (Float _ v)    = "float " ++ show v
    show (Keyword _ w)  = w
    show (Err _)        = "<error>"
    
-- | Function retrieving token position
tokenPos :: Token -> AlexPosn
tokenPos (Sym p _)      = p
tokenPos (Id p _)       = p
tokenPos (Int p _)      = p
tokenPos (Float p _)    = p
tokenPos (Keyword p _)  = p
tokenPos (Err p)        = p

-- | Helper function to parse floating point numbers. The problem with standard
--   read  function is that it cannot handle numbers with missing 0 before 
--   the dot
readFloat :: String -> Float
readFloat s @ ('.' : _) = read ('0' : s)
readFloat s = read s

}
