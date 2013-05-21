-- | The purpose of this module is to analyze the code given as produced
-- by Symbols module (symbol tables + actual parsed code) and remove usage 
-- of variables from outside the procedure scope - that is, to turn closure
-- variables into actual function parameters. This process is known as 
-- lambda-lifting.
module MiniLAX.Static.Closures (
    lambdaLift,
    printFreeRec,
    usedVars,
    usedVarsStmt,
    isIndexed,
    Vars (..),
    Usage (..),
    ProcMap
) where

-- Imports
import Prelude hiding (mapM_, any)
import Data.Monoid
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List hiding (any)
import Data.Foldable
import Data.Maybe

--import Debug.Trace

import Control.Applicative ((<$>))
import Control.Monad.IO.Class

import MiniLAX.AST.Annotated as AST
import MiniLAX.Static.Symbols as S
import MiniLAX.Static.Types as T

import MiniLAX.Printer
import MiniLAX.Compiler
import MiniLAX.Static.Env (Env)
import qualified MiniLAX.Static.Env as E
import MiniLAX.Util.AttrMap

data Usage = Read | Write deriving (Eq, Enum, Show)

instance Monoid Usage where
    mempty = Read
    Write `mappend` _ = Write
    _ `mappend` Write = Write
    _ `mappend` _     = Read
    
-- | Datatype representing variable usage
newtype Vars = Vars { getVars :: SMap Usage }

{-
empty :: Vars 
empty = Vars M.empty
-}

varsToList :: Vars -> [String]
varsToList = M.keys . getVars

{-
insertRead, insertWrite :: String -> Vars -> Vars
insertRead = flip insertVar Read
insertWrite = flip insertVar Write
    
insertVar :: String -> Usage -> Vars -> Vars
insertVar name usage (Vars vars) = Vars vars'
    where vars' = M.insertWith mappend name usage vars
-}
  
instance Monoid Vars where
    mempty = Vars M.empty
    (Vars u) `mappend` (Vars v) = Vars vars
        where vars = M.unionWith mappend u v
        
without :: Vars -> Set String -> Vars
without (Vars v) s = Vars v'
    where v' = v M.\\ ss
          ss = M.fromSet (const Read) s
        
singleVar :: String -> Usage -> Vars
singleVar = (Vars .) . M.singleton 

isIndexed :: AST.Variable a -> Bool
isIndexed (VarName _ _) = False
isIndexed _             = True

-- | Determines variables used inside an expression
usedVars :: Expr a -> Vars
usedVars (UnaryExpr _ _ x) = usedVars x
usedVars (VarExpr _ v) = usedVarsIdx v
usedVars (LitExpr _ _) = mempty
usedVars (BinaryExpr _ _ x y) = vx <> vy
    where vx = usedVars x
          vy = usedVars y
usedVars (CastExpr _ _ e) = usedVars e
          
-- | Determines variables used inside a block of statements
usedVarsBlock :: [Stmt a] -> Vars
usedVarsBlock = mconcat . map usedVarsStmt

-- | Determines variables used in a variable/index expression as if it appears
-- on the right hand side of an assignment, or in non-assignment expression.
usedVarsIdx :: AST.Variable a -> Vars
usedVarsIdx (VarName _ (Name _ n)) = singleVar n Read
usedVarsIdx (VarIndex _ v i) = usedVarsIdx v `mappend` usedVars i

-- | Determines variables used in a variable/index expression as if it appears
-- as the target of an assigmnent statement.
usedVarsLHS :: AST.Variable a -> Vars
usedVarsLHS (VarName _ (Name _ n)) = singleVar n Write
usedVarsLHS v = usedVarsIdx v
          
-- | Determines variables used in a statement
usedVarsStmt :: Stmt a -> Vars
usedVarsStmt (Assignment _ v e) = usedVarsLHS v <> usedVars e
usedVarsStmt (ProcCall _ _ es) = mconcat (usedVars `fmap` es)
usedVarsStmt (IfThenElse _ cond ifTrue ifFalse) =
    usedVars cond <> ifTrue' <> ifFalse'
    where ifTrue'  = usedVarsBlock ifTrue
          ifFalse' = usedVarsBlock ifFalse
usedVarsStmt (While _ cond body) = usedVars cond <> usedVarsBlock body

-- | Determines variables used in a procedure body
usedVarsProc :: Procedure -> Vars
usedVarsProc Procedure { procBody = stms } = usedVarsBlock stms

-- | Determines names declared in a procedure (local variables and formal
-- parameters).
declaredInProc :: Procedure -> Set String
declaredInProc Procedure { procParamMap = params, procVars = vars} = 
    M.keysSet params `S.union` M.keysSet vars
    
-- | Determines free variables inside the procedure body.
computeFreeVariables :: Procedure -> Vars
computeFreeVariables p = used `without` declared
    where used     = usedVarsProc p
          declared = declaredInProc p
          
-- | Map containing procedures
type ProcMap = SMap Procedure
          
-- | Datatype used for patching procedure calls - containes names that need
-- to be appended to an argument list.
type Patch = SMap [String]
   
-- | Performs an actual lambda lifting on a given procedure.
liftLambdas :: Env T.Type -> Procedure -> (Procedure, Vars)
liftLambdas env p @ Procedure { procName     = name
                              , procNested   = nested
                              , procBody     = body
                              } = 
    (addParameters env free p', free)
    where p'       = p { procNested = nested', procBody = body' }
          body'    = patchStmt env' patchRec <$> body
          patchRec = M.insert name (varsToList free) patch
          free     = used `without` declaredInProc p 
          used     = usedVarsProc p <> vars
          (nested', patch, vars) = liftChildren env' nested
          env' = E.pushLayer name (makeTypeMap p) env
          
          
liftChildren :: Env T.Type -> ProcMap -> (ProcMap, Patch, Vars)
liftChildren env ps = (ps', patch, fold vars)
    where (ps', _) = patchChildren env patch $ fst <$> res
          vars     = snd <$> res
          res      = liftLambdas env <$> ps
          patch    = M.keys . getVars <$> vars
          
patchChildren :: Env T.Type -> Patch -> ProcMap -> (ProcMap, Patch)
patchChildren env patch procs = 
    if any (not . null) patch' 
        then patchChildren env patch' procs'
        else (procs', patch')
    where procs'  = fst <$> res
          vars    = snd <$> res
          res     = liftLambdas env <$> (patchProc env patch <$> procs)
          patch'   = M.keys . getVars <$> vars
    
-- | Updates formal parameter list and map with given variables. Environment
-- is used to determine types of the arguments.      
addParameters :: Env T.Type -> Vars -> Procedure -> Procedure
addParameters env (Vars vars) p @ Procedure { procParams = params
                                            , procParamMap = paramMap 
                                            } =
    p { procParams = params', procParamMap = paramMap' }
    where params'     = params ++ M.elems vars'
          paramMap'   = paramMap `M.union` vars'
          vars'       = M.mapWithKey mkParam vars
          mkParam s _ = Parameter { paramName  = s
                                  , paramProps = emptyAttr
                                  , paramKind  = ByVar
                                  , paramType  = typeOf s
                                  }
          typeOf s = fromJust $ E.lookup s env


makeTypeMap :: Procedure -> SMap T.Type
makeTypeMap Procedure { procParamMap = params, procVars = locals         } = 
    params' `M.union` locals'
    where params' = varType <$> params
          locals' = varType <$> locals

  
mkName :: String -> Name Properties
mkName = Name emptyAttr
  
mkVarExpr :: String -> Expr Properties
mkVarExpr = VarExpr emptyAttr . VarName emptyAttr . mkName

mkVarWithType :: Env T.Type -> String -> Expr Properties
mkVarWithType env name = VarExpr props $ VarName props $ mkName name
    where t = maybe (error $ "fuck :/" ++ show env ++ ", " ++ name) id (E.lookup name env)
          props = singleton "type" t


-- | Patches a procedure body and bodies of nested procedures. It does not
-- perform lambda lifting during the process.
patchProc :: Env T.Type -> Patch -> Procedure -> Procedure
patchProc env patch proc = proc { procBody = body', procNested = nested' } 
    where body'   = patchStmt env' patch <$> procBody proc
          nested' = patchProc env' patch <$> procNested proc
          env'    = E.pushAll vars env
          vars    = params' `M.union` locals'
          params' = paramType <$> procParamMap proc
          locals' = varType <$> procVars proc


-- | Given a patch, applies it to a single statement, recursively if necessary.
patchStmt :: Env T.Type -> Patch -> Stmt Properties -> Stmt Properties
patchStmt env patch c @ (ProcCall a n @ (Name _ s) args) = 
    case M.lookup s patch of
        Just vars -> ProcCall a n vars' 
            where vars' = args ++ (mkVarWithType env <$> vars)
        _ -> c
            
patchStmt env patch (IfThenElse a cond ifT ifF) = 
    IfThenElse a cond ifT' ifF'
    where ifT' = patchStmt env patch <$> ifT
          ifF' = patchStmt env patch <$> ifF
          
patchStmt env patch (While a cond body) = 
    While a cond body'
    where body' = patchStmt env patch <$> body
    
patchStmt _ _ asg = asg

-- | Renames procedure inside a statement using given mapping
renameProcs :: SMap String -> Stmt a -> Stmt a
renameProcs m c @ (ProcCall a (Name b s) args) = 
    case M.lookup s m of
        Just s' -> ProcCall a (Name b s') args
        _ -> c
        
renameProcs m (IfThenElse a cond ifT ifF) =
    IfThenElse a cond ifT' ifF'
    where ifT' = renameProcs m <$> ifT
          ifF' = renameProcs m <$> ifF
          
renameProcs m (While a cond body) =
    While a cond body'
    where body' = renameProcs m <$> body
          
renameProcs _ stmt = stmt

-- | Replaces local procedures with full paths
expandProcNames :: SMap String -> String -> Procedure -> Procedure
expandProcNames m prefix p @ Procedure { procName   = name
                                       , procNested = nested
                                       , procBody   = body
                                       } =
    p { procName   = prefix' 
      , procNested = M.mapKeysMonotonic prepend nested'
      , procBody   = renameProcs m' <$> body
      }
    where mself   = M.insert name prefix' m
          chs     = M.map (prepend . procName) nested
          m'      = mself `M.union` chs
          prefix' = prefix ++ sep ++ name
          prepend = (++) (prefix' ++ sep)
          nested' = expandProcNames m' prefix' <$> nested
          sep     = "__"



-- | Flattens the program structure, putting all the procedures in one scope.
-- Assumes lambda lifted input.
flatten :: Procedure -> ProcMap
flatten p @ Procedure { procName = name
                      , procNested = nested 
                      } =
    M.insert name p' ch
    where p' = p { procNested = M.empty }
          ch = M.fold M.union M.empty (flatten <$> nested) 
        

printFree :: Procedure -> PrinterMonad ()
printFree p @ Procedure { procName = name } =
    put name %% ": " %% frees >> endl
    where frees = intercalate ", " (show <$> list)
          list  = M.keys $ getVars $ computeFreeVariables p
          
          
printFreeRec :: Procedure -> PrinterMonad ()
printFreeRec p @ Procedure { procNested = nested } = do
    printFree p
    indented $ mapM_ printFreeRec $ M.elems nested


lambdaLift :: (MonadIO m) => Procedure -> CompilerT m (ProcMap, String)
lambdaLift p = return (procs, procName proc)
    where procs     = flatten proc
          (proc, _) = liftLambdas E.empty expanded
          expanded  = expandProcNames M.empty [] p
    

