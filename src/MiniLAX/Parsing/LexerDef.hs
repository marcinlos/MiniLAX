-- | 
module MiniLAX.Parsing.LexerDef where

-- | We use position information 
import MiniLAX.Location


-- | Token types
data TokenVal =
    Sym String
  | Id String
  | Int Int 
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

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

-- | Helper function to parse floating point numbers. The problem with standard
--   read  function is that it cannot handle numbers with missing 0 before 
--   the dot
readFloat :: String -> Float
readFloat s @ ('.' : _) = read ('0' : s)
readFloat s = read s


 

