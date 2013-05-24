-- | Basic lexer definitions - tokens etc
module MiniLAX.Parsing.LexerCore (
    TokenVal (..),
    Token (..),
    idV,
    intV,
    floatV,
    readFloat,
    AlexUserState (..),
    alexInitUserState
)where

-- | We use position information 
import MiniLAX.Location


-- | Token types
data TokenVal =
    Sym String
  | Id String
  | Int Integer 
  | Float Float
  | Keyword String
  | EOF
  deriving (Eq, Show)
  
  
-- | Full token structure
data Token = Token {
    tkVal :: TokenVal,
    tkPos :: Location,
    tkLex :: String
}

instance Show Token where
    show Token { tkVal = val, tkPos = pos }
        = show val ++ " " ++ show pos
        
-- | Auxilary function fetching identifier from the token.
--   Warning: not total
idV :: Token -> String
idV Token { tkVal = Id val } = val
idV _ = error "Not an id"

-- | Auxilary function fetching integer from the token.
--   Warning: not total
intV :: Token -> Integer
intV Token { tkVal = Int n } = n
intV _ = error "Not an int"

-- | Auxilary function fetching floating point value from the token.
--   Warning: not total
floatV :: Token -> Float
floatV Token { tkVal = Float x } = x
floatV _ = error "Not a float"
        

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

-- | Helper function to parse floating point numbers. The problem with standard
--   read  function is that it cannot handle numbers with missing 0 before 
--   the dot
readFloat :: String -> Float
readFloat s @ ('.' : _) = read ('0' : s)
readFloat s = read s


-- | Interface of a parser compatible with the monadic lexer
-- class (Monad m) => MonadParser m where
--    feed :: Token -> m a -> m a

 

