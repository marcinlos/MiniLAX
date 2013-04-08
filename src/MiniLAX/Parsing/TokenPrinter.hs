module MiniLAX.Parsing.TokenPrinter (
    showTokens
) where

import MiniLAX.Parsing.Lexer
import MiniLAX.Location

import Data.List

showTokens :: [Token] -> String
showTokens = 
    intercalate "\n" . map showOne
    where showOne tok = show (getLocation tok) ++ ": " ++ show tok 

