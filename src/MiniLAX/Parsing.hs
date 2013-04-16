module MiniLAX.Parsing where

-- we need token type
import MiniLAX.Parsing.Lexer

-- | Type of error message
type ParseError = String

type ParseMonad = Either ParseError
 
parseError :: [Token] -> a
parseError tokens = 
    error $ "Parse error at " ++ (showPos pos) ++ " [at " ++ token ++ "]"
    where (token, pos) = case tokens of
              t : _ -> (show t, tokenPos t)
              _     -> ("", AlexPn 0 0 0) 

