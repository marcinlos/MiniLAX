{-# LANGUAGE FlexibleInstances #-}
-- | Typechecking module
module MiniLAX.Static.TypeCheck (
    typecheck,
    emptyTypeEnv
) where

-- Imports
import Prelude hiding (mapM_)
import Control.Monad hiding (mapM_)
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Data.Foldable

import MiniLAX.Location
import MiniLAX.Static.Types
import MiniLAX.AST.Annotated hiding (Type, Program)
import qualified MiniLAX.AST.Annotated as AST
import MiniLAX.Printer
import MiniLAX.AST.PrettyPrint
import MiniLAX.Compiler
import MiniLAX.Diagnostic
import MiniLAX.Static.Symbols
import MiniLAX.Static.Env as E


type TypeEnv = (Env Type, Env Procedure) 

emptyTypeEnv :: TypeEnv
emptyTypeEnv = (E.empty, E.empty)

putVars :: SMap Type -> TypeEnv -> TypeEnv
putVars v (e, p) = (pushAll v e, p)

putProcs :: SMap Procedure -> TypeEnv -> TypeEnv
putProcs ps (e, p) = (e, pushAll ps p)

addCast :: (a -> AST.Type a) -> Expr a -> Expr a
addCast t e = CastExpr loc (t loc) e
    where loc = attr e

coerce :: Coercion -> Expr a -> Expr a
coerce None     = id
coerce Int2Real = addCast TyReal
coerce Real2Int = addCast TyInt

ensureBoolean :: (Functor m, Monad m) => 
    TypeEnv -> 
    Expr Location -> 
    String -> 
    CompilerT m ()
ensureBoolean env e c = do
    t <- computeType env e
    when (t /= BooleanT) $ 
        let tstr = getString $ out t
            msg  = c ++ ": Expected boolean, got " ++ tstr 
        in emitError (Just $ attr e) msg
          
-- ensureIndexable :: (Monad m) => Env -> E
 

doFail :: (Monad m) => String -> CompilerT m Type
doFail = return . TypeError . Just

doFail_ :: (Monad m) => CompilerT m Type
doFail_ = return $ TypeError Nothing

typeError :: (Monad m) => Location -> String -> CompilerT m Type
typeError loc msg = emitError (Just loc) msg >> doFail_

class Typecheckable a where
    typecheck :: (Functor m, Monad m) => TypeEnv -> a -> CompilerT m ()


class (Typecheckable a) => Typed a where
    computeType :: (Functor m, Monad m) => TypeEnv -> a -> CompilerT m Type
    computeType_ :: (Functor m, Monad m) => TypeEnv -> a -> CompilerT m () 
    computeType_ = (void .) .computeType
    
    
instance Typecheckable Procedure where
    typecheck (v, p) Procedure { procName     = name
                               , procNested   = nested
                               , procParamMap = params
                               , procVars     = locals
                               , procBody     = body 
                               } = do
        mapM_ (typecheck env') nested
        mapM_ (typecheck env') body
        where env'    = putVars vars . putProcs nested $ clean
              clean   = (push name v, push name p)
              params' = varType <$> params
              locals' = varType <$> locals
              vars    = params' `M.union` locals'
    
              
instance Typecheckable (Stmt Location) where
    typecheck env (Assignment l v e) = do
        void $ computeType env v
        void $ computeType env e
    
    typecheck (vars, procs) (ProcCall a n args) =
        case E.lookup name procs of
            Just proc -> return ()
            Nothing -> emitError loc $ "Unknown procedure: `" ++ name ++ "'"
        where name = getName n
              loc  = Just a
              
    typecheck env (IfThenElse _ cond ifT ifF) =
        typecheck env cond >> check ifT >> check ifF 
        where check = mapM_ (typecheck env)
            
    typecheck env (While _ cond body) = do 
        typecheck env cond 
        mapM_ (typecheck env) body

    
instance Typed (Expr Location) where
    computeType env (VarExpr _ v) = computeType env v 
              
    computeType _ _ = return $ TypeError Nothing
    
    
instance Typed (AST.Variable Location) where
    computeType (vars, _) (VarName _ n) =
        case E.lookup name vars of
            Just t -> return t
            Nothing -> typeError loc $ "Unresolved variable: `" ++ name ++ "'"
        where name = getName n
              loc  = attr n
              
    computeType env (VarIndex l v i) = do
        base <- computeType env v
        idx  <-computeType env i
        case base of 
            ArrayT t _ _ -> return t
            _ -> let tstr = getString $ out base 
                 in typeError l $ "Cannot index non-array type " ++ tstr


instance Typecheckable (Expr Location) where
    typecheck = (void .) . computeType

instance Typecheckable (AST.Variable Location) where
    typecheck = (void .) . computeType
              
    
    




