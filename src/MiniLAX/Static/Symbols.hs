-- | Symbol table building
module MiniLAX.Static.Symbols where

-- |
-- import MiniLAX.Static.Types
import qualified MiniLAX.AST as AST 
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
    varType      :: Type
} deriving (Show)


data Parameter = Parameter {
    paramName :: String,
    paramType :: AST.Type,
    paramKind :: AST.ParamKind
} deriving (Show)


data Procedure = Procedure {
    procName     :: String,
    procParams   :: SMap Parameter,
    procVars     :: SMap Variable,
    procNested   :: SMap Procedure 
} deriving (Show)


collectSymbols :: AST.Program -> Compiler Procedure
collectSymbols (AST.Program name body) = 
    return Procedure {
        procName = name,
        procParams = empty,
        procVars = vars,
        procNested = nested
    } 
    where (vars, nested) = fromBlock body
          
type SymTable = (SMap Variable, SMap Procedure)
          
fromBlock :: AST.Block -> SymTable
fromBlock (AST.Block decls _) =
    foldl processDecl (empty, empty) decls

processDecl :: SymTable -> AST.Decl -> SymTable
processDecl (vs, ps) decl = 
    case decl of
        AST.VarDecl vname vtype -> (vs', ps)
            where vs' = insert vname var vs
                  var = Variable vname vtype
                  
        AST.ProcDecl hd block -> (vs, ps')
            where ps' = insert name proc ps
                  name = AST.procName hd
                  (pvs, pps) = fromBlock block
                  proc = Procedure {
                      procName = name,
                      procParams = getParams hd,
                      procVars = pvs,
                      procNested = pps
                  }

getParams :: AST.ProcHead -> SMap Parameter
getParams (AST.ProcHead _ params) = foldl putParam empty params
    where putParam m f = insert name param m
               where param = formalToParam f
                     name = paramName param 
    
formalToParam :: AST.Formal -> Parameter
formalToParam (AST.Formal n t k) = Parameter n t k


-- | Pretty-printing of gathered information

printVars :: SMap Variable -> PrinterMonad ()
printVars m = do 
    put "Vars "; bracketed $ 
        forM m $ \(Variable name tp) ->
            put name %% ": " %% show tp >> endl
        
printParams :: SMap Parameter -> PrinterMonad ()
printParams m = do
    put "Params "; bracketed $
        forM m $ \(Parameter name tp kind) ->
            put name %% ": " %% show tp %% " (" %% show kind %% ")" >> endl
            

printProc :: String -> Procedure -> PrinterMonad ()
printProc path Procedure {
        procName = name,
        procParams = params,
        procVars = vars,
        procNested = nested
    } = do
        let path' = path ++ "/" ++ name 
        put "Proc " %% path' %% " "; bracketed $ do
            printVars vars
            printParams params
        void $ forM nested (printProc path')


