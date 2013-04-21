{
module MiniLAX.Parsing.Lexer2 (
    Token(..), 
    scanTokens
    -- alexScanTokens
) where

import MiniLAX.Location
import MiniLAX.Parsing.LexerDef

import Control.Monad
import Data.Maybe

}

%wrapper "monadUserState"

$letter = [a-zA-Z] 
$digit  = 0-9

@id             = $letter ($letter | $digit)*
@int_const      = $digit $digit*
@scale_factor   = "E" ("+" | "-") ? @int_const
@real_const     = (@int_const ?) "." @int_const 

tokens :-

  $white+                          ;
  "(*" (. | \n)* "*)"              ;
  @int_const                       { mkT (Int . read) }
  @real_const                      { mkT (Float . readFloat) }
  [\:\;\=\+\-\*\/\(\)\.\,\[\]\<]   { mkT Sym }
  ":="                             { mkT Sym }
  ".."                             { mkT Sym }
  
  "ARRAY" | "BEGIN" | "BOOLEAN" | "DECLARE" | "DO" | "ELSE" | "END" |
  "FALSE" | "IF" | "INTEGER" | "NOT"| "OF" | "PROCEDURE" | "PROGRAM" |
  "REAL" | "THEN" | "TRUE" | "VAR" | "WHILE" { mkT Keyword }
  
  @id                              { mkT Id }

{

alex2Loc :: AlexPosn -> Location
alex2Loc (AlexPn _ line col) = Location "" line col

alexEOF :: Alex Token
alexEOF = return $ Token EOF noloc ""
    where noloc = Location "-" 0 0

type LexerAction r = AlexInput -> Int -> Alex r

          
mkT :: (String -> TokenVal) -> LexerAction Token
mkT v (p, _, _, str) len = return $ Token (v lex) loc lex
    where loc = alex2Loc p
          lex = take len str
          
          
scanTokens :: String -> Either String [Token]
scanTokens s = runAlex s scanLoop
 
 
scanLoop :: Alex [Token]
scanLoop = do
    (a, v) <- addLocInfo alexMonadScan
    when (isJust v) $ lexerError (fromJust v)
    if tkVal a == EOF then return []
                      else scanLoop >>= return . (a :)
                      
addLocInfo :: Alex a -> Alex (a, Maybe String)
addLocInfo (Alex next) = Alex next'
    where next' s = case next s of
              Right (st, v) -> Right (st, (v, Nothing))
              Left msg      -> Right (s,  (undefined, Just msg)) 
              
              
lexerError :: String -> Alex a
lexerError msg = do
    (p, c, _, input) <- alexGetInput
    let inp = trim $ takeMax 30 (firstLine input)
        pos = if null input
                  then "at the end of file"
                  else if null inp 
                      then " before end of line"
                      else " at '" ++ inp ++ "'"
        disp = if (null msg) then "Lexer error"
                             else trim msg
    alexError $ disp ++ " at " ++ showAlexPosn p ++ pos
              
showAlexPosn :: AlexPosn -> String
showAlexPosn = show . alex2Loc
              
trim :: String -> String
trim = reverse . trimFront . reverse . trimFront
    where trimFront = dropWhile (== ' ' )
    
firstLine :: String -> String
firstLine = filter (/= '\r') . takeWhile (/= '\n')

takeMax :: Int -> String -> String
takeMax n s = if length s > n then take n s
                              else s


}