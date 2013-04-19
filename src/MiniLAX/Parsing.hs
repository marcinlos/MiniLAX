-- |
module MiniLAX.Parsing where

-- we need token type
import MiniLAX.Parsing.Lexer
--import MiniLAX.AST
--import MiniLAX.TreeDecoration
-- import Control.Monad.State

-- | Type of error message
type ParseError = String

-- | Type of parsing monad
type ParseMonad = Either ParseError

getAST :: ParseMonad a -> Either ParseError a
getAST = id



 
parseError :: [Token] -> a
parseError tokens = 
    error $ "Parse error at " ++ showPos pos ++ " [at " ++ token ++ "]"
    where (token, pos) = case tokens of
              t : _ -> (show t, tokenPos t)
              _     -> ("", AlexPn 0 0 0) 

