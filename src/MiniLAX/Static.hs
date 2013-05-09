-- | Static code analysis, prior to translation to IR
module MiniLAX.Static (
    analyze
) where

-- Imports
import qualified Data.Map as M
import Control.Monad.IO.Class

import MiniLAX.Compiler
import MiniLAX.Options
import MiniLAX.Printer
import MiniLAX.Static.Symbols
import MiniLAX.Static.TypeCheck
import MiniLAX.Static.Closures


type Prog = (ProcMap, String)

analyze :: (Functor m, MonadIO m) => Procedure -> CompilerT m Prog
analyze p = do
    typecheck emptyTypeEnv p
    maybeDumpFreeVars p
    prog <- lambdaLift p
    maybeDumpLifted prog
    return prog
    
    
maybeDumpLifted :: (Functor m, MonadIO m) => Prog -> CompilerT m ()
maybeDumpLifted (m, _) =
    ifEnabled optDumpLambdaLifted $ mapM_ dumpProc $ M.elems m
    where dumpProc = liftIO . putStrLn . getString . printProc ""
        
maybeDumpFreeVars :: (Functor m, MonadIO m) => Procedure -> CompilerT m ()
maybeDumpFreeVars p =
    ifEnabled optDumpFreeVars $
        liftIO $ putStrLn $ getString $ printFreeRec p

