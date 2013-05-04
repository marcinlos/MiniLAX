-- | The purpose of this module is to analyze the code given as produced
-- by Symbols module (symbol tables + actual parsed code) and remove usage 
-- of variables from outside the procedure scope - that is, to turn closure
-- variables into actual function parameters.
module MiniLAX.Static.Closures where

-- Imports
import Prelude hiding (mapM_, any)
import Data.Monoid
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List hiding (any)
import Data.Foldable
import Data.Maybe

import Debug.Trace

import Control.Applicative ((<$>))
import Control.Monad.IO.Class

import MiniLAX.AST.Annotated as AST
import MiniLAX.Static.Symbols as S
import MiniLAX.Static.Types as T

import MiniLAX.Location (Location)
import qualified MiniLAX.Location as L
import MiniLAX.Printer
import MiniLAX.Compiler
import MiniLAX.Static.Env (Env)
import qualified MiniLAX.Static.Env as E


data Usage = Read | Write deriving (Eq, Enum, Show)

instance Monoid Usage where
    mempty = Read
    Write `mappend` _ = Write
    _ `mappend` Write = Write
    _ `mappend` _     = Read
    


newtype Vars = Vars { getVars :: SMap Usage }

empty :: Vars 
empty = Vars M.empty

varsToList :: Vars -> [String]
varsToList = M.keys . getVars

insertRead, insertWrite :: String -> Vars -> Vars
insertRead = flip insertVar Read
insertWrite = flip insertVar Write
    
insertVar :: String -> Usage -> Vars -> Vars
insertVar name usage (Vars vars) = Vars vars'
    where vars' = M.insertWith mappend name usage vars
    
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


usedVars :: Expr a -> Vars
usedVars (UnaryExpr _ _ x) = usedVars x
usedVars (VarExpr _ v) = usedVarsIdx v
usedVars (LitExpr _ _) = mempty
usedVars (BinaryExpr _ _ x y) = vx `mappend` vy
    where vx = usedVars x
          vy = usedVars y
          
usedVarsBlock :: [Stmt a] -> Vars
usedVarsBlock = mconcat . map usedVarsStmt

usedVarsIdx :: AST.Variable a -> Vars
usedVarsIdx (VarName _ (Name _ n)) = singleVar n Read
usedVarsIdx (VarIndex _ v i) = usedVarsIdx v `mappend` usedVars i

usedVarsLHS :: AST.Variable a -> Vars
usedVarsLHS (VarName _ (Name _ n)) = singleVar n Write
usedVarsLHS v = usedVarsIdx v
          
usedVarsStmt :: Stmt a -> Vars
usedVarsStmt (Assignment _ v e) = usedVarsLHS v `mappend` usedVars e
usedVarsStmt (ProcCall _ _ es) = mconcat (usedVars `fmap` es)
usedVarsStmt (IfThenElse _ cond ifTrue ifFalse) =
    usedVars cond `mappend` ifTrue' `mappend` ifFalse'
    where ifTrue'  = usedVarsBlock ifTrue
          ifFalse' = usedVarsBlock ifFalse
usedVarsStmt (While _ cond body) = usedVars cond `mappend` usedVarsBlock body

usedVarsProc :: Procedure -> Vars
usedVarsProc Procedure { procBody = stms } = usedVarsBlock stms

declaredInProc :: Procedure -> Set String
declaredInProc Procedure { procParamMap = params, procVars = vars} = 
    M.keysSet params `S.union` M.keysSet vars
    

computeFreeVariables :: Procedure -> Vars
computeFreeVariables p = used `without` declared
    where used     = usedVarsProc p
          declared = declaredInProc p
          
type ProcMap = SMap Procedure
          
type Patch = SMap [String]
   
liftLambdas :: Env T.Type -> Procedure -> (Procedure, Vars)
liftLambdas env p @ Procedure { procName     = name
                              , procNested   = nested
                              , procBody     = body
                              } = 
    (addParameters env free p', free)
    where p'       = p { procNested = nested', procBody = body' }
          body'    = patchStmt patchRec  <$> body
          patchRec = M.insert name (varsToList free) patch
          free     = used `without` declaredInProc p 
          used     = usedVarsProc p `mappend` vars
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
          res     = liftLambdas env <$> (patchProc patch <$> procs)
          patch'   = M.keys . getVars <$> vars
          
addParameters :: Env T.Type -> Vars -> Procedure -> Procedure
addParameters env (Vars vars) p @ Procedure { procParams = params
                                            , procParamMap = paramMap 
                                            } =
    p { procParams = params', procParamMap = paramMap' }
    where params'     = params ++ M.elems vars'
          paramMap'   = paramMap `M.union` vars'
          vars'       = M.mapWithKey mkParam vars
          mkParam s _ = Parameter { paramName = s
                                  , paramPos  = L.empty
                                  , paramKind = ByVar
                                  , paramType = typeOf s
                                  }
          typeOf s = fromJust $ E.lookup s env


makeTypeMap :: Procedure -> SMap T.Type
makeTypeMap Procedure { procParamMap = params
                      , procVars = locals 
                      } = 
    params' `M.union` locals'
    where params' = varType <$> params
          locals' = varType <$> locals

  
mkName :: String -> Name Location
mkName = Name L.empty
  
mkVarExpr :: String -> Expr Location
mkVarExpr = VarExpr L.empty . VarName L.empty . mkName


patchProc :: Patch -> Procedure -> Procedure
patchProc patch p @ Procedure { procBody = body
                              , procNested = nested 
                              } = 
    p { procBody   = patchStmt patch <$> body
      , procNested = patchProc patch <$> nested }


patchStmt :: Patch -> Stmt Location -> Stmt Location
patchStmt patch c @ (ProcCall a n @ (Name _ s) args) = 
    case M.lookup s patch of
        Just vars -> ProcCall a n vars' 
            where vars' = args ++ (mkVarExpr <$> vars)
        _ -> c
            
patchStmt patch (IfThenElse a cond ifT ifF) = 
    IfThenElse a cond ifT' ifF'
    where ifT' = patchStmt patch <$> ifT
          ifF' = patchStmt patch <$> ifF
          
patchStmt patch (While a cond body) = 
    While a cond body'
    where body' = patchStmt patch <$> body
    
patchStmt _ asg = asg
        

printFree :: Procedure -> PrinterMonad ()
printFree p @ Procedure { procName = name } =
    put name %% ": " %% frees >> endl
    where frees = intercalate ", " (show <$> list)
          list  = M.keys $ getVars $ computeFreeVariables p
          
          
printFreeRec :: Procedure -> PrinterMonad ()
printFreeRec p @ Procedure { procNested = nested } = do
    printFree p
    indented $ mapM_ printFreeRec $ M.elems nested


lambdaLift :: (MonadIO m) => Procedure -> CompilerT m (Procedure, Vars)
lambdaLift = return . liftLambdas E.empty

