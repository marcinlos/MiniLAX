-- | Symbol table building
module MiniLAX.Static.Symbols where

-- Imports
import qualified Data.Map as M
import Data.Traversable
import Control.Monad hiding (forM)

import qualified MiniLAX.AST.Annotated as AST
import MiniLAX.AST.Util
import MiniLAX.Location 
import MiniLAX.Printer
import MiniLAX.Compiler
import MiniLAX.Static.Types
import MiniLAX.AST.PrettyPrint


type SMap = M.Map String
type ScopeId = [String]

data Local = Local {
    localName  :: String,
    localPos   :: Location,
    localType  :: Type
} deriving (Show)


data Parameter = Parameter {
    paramName :: String,
    paramPos  :: Location,
    paramType :: Type,
    paramKind :: ParamKind
} deriving (Show)


class Variable a where
    varName :: a -> String
    varType :: a -> Type
    
instance Variable Local where
    varName = localName
    varType = localType
    
instance Variable Parameter where
    varName = paramName
    varType = paramType


data Procedure = Procedure {
    procName     :: String,
    procPos      :: Location,
    procParams   :: [Parameter],
    procParamMap :: SMap Parameter,
    procVars     :: SMap Local,
    procNested   :: SMap Procedure,
    procBody     :: [AST.Stmt Location]
} deriving (Show)



collectSymbols :: (Monad m) => AST.Program Location -> CompilerT m Procedure
collectSymbols (AST.Program pos (AST.Name _ name) body) =
    return Procedure {
        procName     = name,
        procPos      = pos,
        procParams   = [],
        procParamMap = M.empty,
        procVars     = vars,
        procNested   = nested,
        procBody     = stms
    } 
    where (vars, nested)     = fromBlock body
          AST.Block _ _ stms = body
          
type SymTable = (SMap Local, SMap Procedure)
          
fromBlock :: AST.Block Location -> SymTable
fromBlock (AST.Block  _ decls _) =
    foldl processDecl (M.empty, M.empty) decls

processDecl :: SymTable -> AST.Decl Location -> SymTable
processDecl (vs, ps) decl = 
    case decl of
        AST.VarDecl pos (AST.Name _ vname) vtype -> (vs', ps)
            where vs' = M.insert vname var vs
                  var = Local vname pos (ast2Type vtype)
                  
        AST.ProcDecl pos hd block -> (vs, ps')
            where ps'                = M.insert name proc ps
                  name               = AST.getName hd
                  (vars, procs)      = fromBlock block
                  (pms, pmap)        = getParams hd
                  AST.Block _ _ stms = block
                  proc = Procedure {
                      procName     = name,
                      procPos      = pos,
                      procParams   = pms,
                      procParamMap = pmap,
                      procVars     = vars,
                      procNested   = procs,
                      procBody     = stms
                  }

getParams :: AST.ProcHead Location -> ([Parameter], SMap Parameter)
getParams (AST.ProcHead _ _ params) = (list, mp)
    where list = fmap formalToParam params
          mp   = foldl putParam M.empty list
          putParam m p = M.insert (paramName p) p m
    
    
formalToParam :: AST.Formal Location -> Parameter
formalToParam (AST.Formal pos kind name tp) = 
    Parameter {
        paramName = AST.getName name,
        paramPos  = pos,
        paramType = ast2Type tp,
        paramKind = kind
    }


-- | Pretty-printing of gathered information
printType :: Type -> PrinterMonad ()
printType (ArrayT tp low high) = do 
    append "ArrayT [" %% show low %% ".." %% show high %% "] of "
    printType tp
    
printType other = append (show other)


printVars :: SMap Local -> PrinterMonad ()
printVars m = do 
    put "Vars " >> endl; indented $ 
        forM m $ \(Local name pos tp) ->
            put name %% ": " >> printType tp >> append " " %% show pos >> endl
        
printParams :: [Parameter] -> PrinterMonad ()
printParams m = do
    put "Params " >> endl; indented $
        forM m $ \(Parameter name pos tp kind) -> do
            put name %% ": " >> printType tp >> append " (" %% show kind %% ")" 
            append "   " %% show pos >> endl

printStms :: (Show a) => [AST.Stmt a] -> PrinterMonad ()
printStms m = do 
    put "Vars " >> endl; indented $ 
        forM m $ \stm->
            put (show  $ AST.attr stm) %% show stm >> endl
            

printProc :: String -> Procedure -> PrinterMonad ()
printProc path Procedure {
    procName   = name,
    procPos    = pos,
    procParams = params,
    procVars   = vars,
    procNested = nested,
    procBody   = body
} = do
    let path' = path ++ (if null path then [] else "::") ++ name 
    put "Proc " %% path' %% " " %% show pos %% "  " >> endl 
    indented $ do
        printParams params
        printVars vars
        put (replicate 50 '-') >> endl
        mapM_ out body
        endl
    void $ forM nested (printProc path')


