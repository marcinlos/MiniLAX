module MiniLAX.Parsing.Printer (
    showTokens
) where

import MiniLAX.Parsing.Lexer

import Data.List

showTokens :: [Token] -> String
showTokens = 
    intercalate "\n" . (map showOne)
    where showOne tok = showPos (tokenPos tok) ++ ": " ++ show tok 

