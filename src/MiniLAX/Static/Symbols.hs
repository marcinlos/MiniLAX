-- | Symbol table building
module MiniLAX.Static.Symbols where

-- |
-- import MiniLAX.Static.Types
-- import qualified MiniLAX.AST as AST 
import qualified MiniLAX.AST.Annotated as AST
import MiniLAX.AST.Util
import MiniLAX.Location 
import MiniLAX.Printer
import MiniLAX.Compiler

import MiniLAX.Static.Types

import Data.Map hiding (foldl)
import Data.Traversable
import Control.Monad hiding (forM)


type SMap = Map String
type ScopeId = [String]

data Variable = Variable {
    varName      :: String,
    varPos       :: Location,
    varType      :: Type
} deriving (Show)


data Parameter = Parameter {
    paramName :: String,
    paramPos  :: Location,
    paramType :: Type,
    paramKind :: ParamKind
} deriving (Show)


data Procedure = Procedure {
    procName     :: String,
    procPos      :: Location,
    procParams   :: SMap Parameter,
    procVars     :: SMap Variable,
    procNested   :: SMap Procedure 
} deriving (Show)


collectSymbols :: (Monad m) => AST.Program Location -> CompilerT m Procedure
collectSymbols (AST.Program pos (AST.Name _ name) body) = 
    return Procedure {
        procName   = name,
        procPos    = pos,
        procParams = empty,
        procVars   = vars,
        procNested = nested
    } 
    where (vars, nested) = fromBlock body
          
type SymTable = (SMap Variable, SMap Procedure)
          
fromBlock :: AST.Block Location -> SymTable
fromBlock (AST.Block  _ decls _) =
    foldl processDecl (empty, empty) decls

processDecl :: SymTable -> AST.Decl Location -> SymTable
processDecl (vs, ps) decl = 
    case decl of
        AST.VarDecl pos (AST.Name _ vname) vtype -> (vs', ps)
            where vs' = insert vname var vs
                  var = Variable vname pos (ast2Type vtype)
                  
        AST.ProcDecl pos hd block -> (vs, ps')
            where ps' = insert name proc ps
                  name = AST.getName hd
                  (pvs, pps) = fromBlock block
                  proc = Procedure {
                      procName   = name,
                      procPos    = pos,
                      procParams = getParams hd,
                      procVars   = pvs,
                      procNested = pps
                  }

getParams :: AST.ProcHead Location -> SMap Parameter
getParams (AST.ProcHead _ _ params) = foldl putParam empty params
    where putParam m f = insert name param m
               where param = formalToParam f
                     name = paramName param 
    
formalToParam :: AST.Formal Location -> Parameter
formalToParam (AST.Formal pos kind name tp) = 
    Parameter {
        paramName = AST.getName name,
        paramPos  = pos,
        paramType = ast2Type tp,
        paramKind = kind
    }


-- | Pretty-printing of gathered information

printVars :: SMap Variable -> PrinterMonad ()
printVars m = do 
    put "Vars "; bracketed $ 
        forM m $ \(Variable name pos tp) ->
            put name %% ": " %% show tp %% " " %% show pos >> endl
        
printParams :: SMap Parameter -> PrinterMonad ()
printParams m = do
    put "Params "; bracketed $
        forM m $ \(Parameter name pos tp kind) -> do
            put name %% ": " %% show tp %% " (" %% show kind %% ")" 
            put (show pos) >> endl
            

printProc :: String -> Procedure -> PrinterMonad ()
printProc path Procedure {
        procName   = name,
        procPos    = pos,
        procParams = params,
        procVars   = vars,
        procNested = nested
    } = do
        let path' = path ++ "/" ++ name 
        put "Proc " %% path' %% " " %% show pos; bracketed $ do
            printVars vars
            printParams params
        void $ forM nested (printProc path')


