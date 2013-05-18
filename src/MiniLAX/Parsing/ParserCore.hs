-- | Auxilary parsing functions
module MiniLAX.Parsing.ParserCore where

--
import MiniLAX.Compiler
import MiniLAX.AST.Annotated
import MiniLAX.Location
import MiniLAX.Diagnostic
import MiniLAX.Parsing.LexerCore
import MiniLAX.Util.AttrMap


parseError :: (Monad m) => [Token] -> CompilerT m a
parseError tokens = 
    throwC $ pos ++ ": parse error" ++ tok
    where (pos, tok) = case tokens of
              t : _ -> (show (tkPos t), " at " ++ show (tkVal t))
              _     -> ("At the end of input", "") 

             
type ErrPrinter m a = Token -> a -> CompilerT m a

withErr :: (Monad m) => String -> Location -> a -> CompilerT m a
withErr msg loc a = emitError (Just loc) msg >> return a
              
withErr' :: (Monad m) => String -> ErrPrinter m a
withErr' = (. tkPos) . withErr  

errDeclSemi :: (Monad m) => ErrPrinter m [Decl Location]
errDeclSemi = withErr' "Semicolon after last declaration"

errStmtSemi :: (Monad m) => ErrPrinter m [Stmt Location]
errStmtSemi = withErr' "Semicolon after last statement"

toProps :: Program Location -> Program Properties
toProps = fmap (singleton "pos")
