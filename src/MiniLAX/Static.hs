-- | Static code analysis, prior to translation to IR
module MiniLAX.Static (
    analyze
) where

-- |
import Control.Monad
import Control.Monad.IO.Class

import MiniLAX.Compiler
import MiniLAX.Options
import MiniLAX.Printer
import MiniLAX.Static.Symbols
import MiniLAX.Static.Closures

import MiniLAX.AST.PrettyPrint

analyze :: (Functor m, MonadIO m) => Procedure -> CompilerT m Procedure
analyze p = do
    maybeDumpFreeVars p
    (lifted, free) <- lambdaLift p
    maybeDumpLifted lifted
    return lifted
    
    
maybeDumpLifted :: (Functor m, MonadIO m) => Procedure -> CompilerT m ()
maybeDumpLifted p =
    ifEnabled optDumpLambdaLifted $ do
        let s = printProc "" p
        liftIO $ putStrLn $ getString s
        
maybeDumpFreeVars :: (Functor m, MonadIO m) => Procedure -> CompilerT m ()
maybeDumpFreeVars p =
    ifEnabled optDumpFreeVars $
        liftIO $ putStrLn $ getString $ printFreeRec p
        


