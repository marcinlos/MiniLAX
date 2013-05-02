{-# LANGUAGE ExistentialQuantification #-}
-- | The purpose of this module is to analyze the code given as produced
-- by Symbols module (symbol tables + actual parsed code) and remove usage 
-- of variables from outside the procedure scope - that is, to turn closure
-- variables into actual function parameters.
module MiniLAX.Static.Closures where

-- Imports
import Data.Monoid
import qualified Data.Map as M

import MiniLAX.AST.Annotated
import MiniLAX.Static.Symbols



data Usage = Read | Write

instance Monoid Usage where
    Write `mappend` _ = Write
    _ `mappend` Write = Write
    _ `mappend` _     = Read
    

{-
type Binding = ([Procedure], Variable)

computeClosureVariables :: [Procedure] -> ([Binding], [Binding])
computeClosureVariables (p : ps) = 
    ([], [])
computeClosureVariables _ = ([], [])
-}


data Env = Env [(SMap Parameter, SMap Local)]


newtype Vars = Vars (SMap Usage)

empty :: Vars 
empty = Vars M.empty

insertRead, insertWrite :: String -> Vars -> Vars
insertRead = flip insert Read
insertWrite = flip insert Write
    
insert :: String -> Usage -> Vars -> Vars
insert name usage (Vars vars) = Vars vars'
    where vars' = M.insertWith mappend name usage vars
    
instance Monoid Vars where
    mempty = Vars M.empty
    (Vars u) `mappend` (Vars v) = Vars vars
        where vars = M.unionWith mappend u v
        
singleVar :: String -> Usage -> Vars
singleVar = (Vars .) . M.singleton 

{-
computeFreeVariables :: Procedure -> Vars
computeFreeVariables Procedure {
        procParamMap = params,
        procVars     = vars
    } = Vars missing
    where missing = used `M.difference` unknown
          params' = M.keySet params
          -}

usedVars :: Expr a -> Vars
usedVars (UnaryExpr _ _ x) = usedVars x
usedVars (VarExpr _ v) = singleVar (getName v) Read
usedVars (LitExpr _ _) = mempty
usedVars (BinaryExpr _ _ x y) = vx `mappend` vy
    where vx = usedVars x
          vy = usedVars y
          
usedVarsBlock :: [Stmt a] -> Vars
usedVarsBlock = mconcat . map usedVarsStmt
          
usedVarsStmt :: Stmt a -> Vars
usedVarsStmt (Assignment _ v e) = insertWrite (getName v) (usedVars e)
usedVarsStmt (ProcCall _ _ es) = mconcat (usedVars `fmap` es)
usedVarsStmt (IfThenElse _ cond ifTrue ifFalse) =
    usedVars cond `mappend` ifTrue' `mappend` ifFalse'
    where ifTrue'  = usedVarsBlock ifTrue
          ifFalse' = usedVarsBlock ifFalse
usedVarsStmt (While _ cond body) = usedVars cond `mappend` usedVarsBlock body






