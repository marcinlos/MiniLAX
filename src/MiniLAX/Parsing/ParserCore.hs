-- | Auxilary parsing functions
module MiniLAX.Parsing.ParserCore where

-- | Monad it lives in
import MiniLAX.Compiler
import MiniLAX.Parsing.LexerCore


parseError :: (Monad m) => [Token] -> CompilerT m a
parseError tokens = 
    throwC $ pos ++ ": parse error" ++ tok
    where (pos, tok) = case tokens of
              t : _ -> (show (tkPos t), " at " ++ show (tkVal t))
              _     -> ("At the end of input", "") 

