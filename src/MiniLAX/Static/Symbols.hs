-- | Symbol table building
module MiniLAX.Static.Symbols where

-- Imports
import Prelude hiding (foldl)
import qualified Data.Map as M
import Data.Foldable as F

import qualified MiniLAX.AST.Annotated as AST
import MiniLAX.AST.Util
import MiniLAX.Location 
import MiniLAX.Printer
import MiniLAX.Static.Types
import MiniLAX.AST.PrettyPrint
import MiniLAX.Util.AttrMap


type SMap = M.Map String
type ScopeId = [String]

data Local = Local { localName  :: String
                   , localProps :: Properties
                   , localType  :: Type
                   } 
    deriving (Show)


data Parameter = Parameter { paramName  :: String
                           , paramProps :: Properties
                           , paramType  :: Type
                           , paramKind  :: ParamKind
                           } 
    deriving (Show)


class Variable a where
    varName  :: a -> String
    varType  :: a -> Type
    varProps :: a -> Properties
    
instance Variable Local where
    varName  = localName
    varType  = localType
    varProps = localProps 
    
instance Variable Parameter where
    varName  = paramName
    varType  = paramType
    varProps = paramProps

getPos :: Properties -> Location
getPos = getAttr "pos"

data Procedure = Procedure { procName     :: String
                           , procProps    :: Properties
                           , procParams   :: [Parameter]
                           , procParamMap :: SMap Parameter
                           , procVars     :: SMap Local
                           , procNested   :: SMap Procedure
                           , procBody     :: [AST.Stmt Properties]
                           } 
    deriving (Show)


collectSymbols :: (Monad m) => AST.Program Properties -> m Procedure
collectSymbols (AST.Program props (AST.Name _ name) body) = do
    (vars, nested) <- fromBlock body
    return Procedure { procName     = name
                     , procProps    = props
                     , procParams   = []
                     , procParamMap = M.empty
                     , procVars     = vars
                     , procNested   = nested
                     , procBody     = stms
                     } 
    where AST.Block _ _ stms = body
          
type SymTable = (SMap Local, SMap Procedure)
          
fromBlock :: (Monad m) => AST.Block Properties -> m SymTable
fromBlock (AST.Block  _ decls _) =
    foldlM processDecl (M.empty, M.empty) decls

processDecl :: (Monad m) => SymTable -> AST.Decl Properties -> m SymTable
processDecl sym @ (vs, ps) decl = case decl of
    AST.VarDecl pos (AST.Name _ vname) vtype -> return (vs', ps)
        where vs' = M.insert vname var vs
              var = Local vname pos (ast2Type vtype)
              
    AST.ProcDecl props hd block -> do
        (vars, procs) <- fromBlock block
        (pms, pmap)   <- getParams hd
        let name      = AST.getName hd
            AST.Block _ _ stms = block
            proc = Procedure { procName     = name
                             , procProps    = props
                             , procParams   = pms
                             , procParamMap = pmap
                             , procVars     = vars
                             , procNested   = procs
                             , procBody     = stms
                             }
        insertProc proc sym
        
insertProc :: (Monad m) => Procedure -> SymTable -> m SymTable
insertProc proc (vs, ps) = return (vs, M.insert (procName proc) proc ps) 

getParams :: (Monad m) => 
    AST.ProcHead Properties -> m ([Parameter], SMap Parameter)
getParams (AST.ProcHead _ _ params) = 
    return (list, mp)
    where list = fmap formalToParam params
          mp   = foldl putParam M.empty list
          putParam m p = M.insert (paramName p) p m
    
    
formalToParam :: AST.Formal Properties -> Parameter
formalToParam (AST.Formal props kind name tp) = 
    Parameter { paramName  = AST.getName name
              , paramProps = props
              , paramType  = ast2Type tp
              , paramKind  = kind
              }


-- | Pretty-printing of gathered information

showPos :: Properties -> String
showPos props = maybe "(-)" show (tryAttr "pos" props :: Maybe Location)
   

printVars :: SMap Local -> PrinterMonad ()
printVars m = do 
    put "Vars " >> endl; indented $ 
        forM_ m $ \(Local name props tp) ->
            put name %% ": " >> out tp >> append " " %% showPos props >> endl 
        
printParams :: [Parameter] -> PrinterMonad ()
printParams m = do
    put "Params " >> endl; indented $
        forM_ m $ \(Parameter name props tp kind) -> do
            put name %% ": " >> out tp >> append " (" %% show kind %% ")" 
            append "   " %% showPos props >> endl

printStms :: (Show a) => [AST.Stmt a] -> PrinterMonad ()
printStms m = do 
    put "Vars " >> endl; indented $ 
        forM_ m $ \stm->
            put (show  $ AST.attr stm) %% show stm >> endl
            

printProc :: String -> Procedure -> PrinterMonad ()
printProc path Procedure { procName   = name
                         , procProps  = props
                         , procParams = params
                         , procVars   = vars
                         , procNested = nested
                         , procBody   = body
                         } = do
    let path' = path ++ (if null path then [] else "::") ++ name 
    put "Proc " %% path' %% " " %% showPos props %% "  " >> endl 
    indented $ do
        printParams params
        printVars vars
        put (replicate 50 '-') >> endl
        F.mapM_ out body
        endl
    forM_ nested (printProc path')


