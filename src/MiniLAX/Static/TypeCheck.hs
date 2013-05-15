{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | Typechecking module
module MiniLAX.Static.TypeCheck (
    typecheck,
    emptyTypeEnv
) where

-- Imports
import Prelude hiding (mapM, mapM_)
import Control.Monad hiding (mapM, mapM_)
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Data.Traversable

import MiniLAX.Location
import MiniLAX.Static.Types hiding (getType)
import MiniLAX.AST.Annotated hiding (Type, Program)
import qualified MiniLAX.AST.Annotated as AST
import MiniLAX.Printer
import MiniLAX.AST.PrettyPrint
import MiniLAX.Diagnostic
import MiniLAX.Util.Flag
import MiniLAX.Static.Symbols
import MiniLAX.Static.Env as E
import MiniLAX.Util.AttrMap
import MiniLAX.AST.Util (ast2Type)


type TypeEnv = (Env Type, Env Procedure) 

emptyTypeEnv :: TypeEnv
emptyTypeEnv = (E.empty, E.empty)

putVars :: SMap Type -> TypeEnv -> TypeEnv
putVars v (e, p) = (pushAll v e, p)

putProcs :: SMap Procedure -> TypeEnv -> TypeEnv
putProcs ps (e, p) = (e, pushAll ps p)


type ExprP = Expr Properties

addCast :: (Properties -> AST.Type Properties) -> ExprP -> ExprP
addCast f e = setType (ast2Type t) $ CastExpr props t e
    where props = attr e
          t     = f props

coerce :: Coercion -> ExprP -> ExprP
coerce None     = id
coerce Int2Real = addCast TyReal
coerce Real2Int = addCast TyInt

setError :: (MonadFlag m) => m ()
setError = setFlag

ensureBoolean :: (Functor m, MonadDiag m, MonadFlag m) => 
    TypeEnv -> ExprP -> String -> m (ExprP, Type)
ensureBoolean env e c = do
    res @ (_, t) <- computeType env e
    when (t /= BooleanT && t /= TypeError) $ do 
        let tstr = getString $ out t
            msg  = c ++ ": Expected boolean, got " ++ tstr
            pos  = Just $ attr e .#. "pos" 
        emitError pos msg
        setError
    return res
          
ensureIndex :: (Functor m, MonadDiag m, MonadFlag m) => 
    TypeEnv -> ExprP -> m ExprP
ensureIndex env e = do
    (e', t) <- computeType env e
    case t of
        IntegerT -> return e'
        RealT -> do let pos = Just $ attr e .#. "pos"
                        msg = "Converting real to integer as an array index"
                    emitWarn pos msg
                    return e'
        _     -> do let pos = Just $ attr e .#. "pos"
                        tstr = getString $ out t
                        msg = "Cannot use " ++ tstr ++ " as an array index"
                    emitError pos msg
                    doFail_ e'  

withType :: (Monad m, Annotated a Properties) => Type -> a -> m (a, Type)
withType t a = return (setType t a, t) 
 

doFail :: (MonadDiag m, MonadFlag m, Annotated a Properties) => a -> m (a, Type) 
doFail e = setError >> withType TypeError e

doFail_ :: (MonadDiag m, MonadFlag m, Annotated a Properties) => a ->  m a
doFail_ m = return fst `ap` doFail m 

--typeError :: (Monad m, Annotated a Properties) => 
--    a -> Location -> String -> CompilerT m (a, Type)
--typeError v loc msg = emitError (Just loc) msg >> doFail v 


--getType :: (Annotated a Properties) => a -> Type
--getType = getAttr "type" . attr

setType :: (Annotated a Properties) => Type -> a -> a
setType t = modifyAttr (putAttr "type" t)


class Typecheckable a where
    typecheck :: (Functor m, MonadDiag m, MonadFlag m) => TypeEnv -> a -> m a

class (Typecheckable a) => Typed a where
    computeType :: (Functor m, MonadDiag m, MonadFlag m) => 
        TypeEnv -> a -> m (a, Type)
        
    computeType_ :: (Functor m, MonadDiag m, MonadFlag m) => 
        TypeEnv -> a -> m ()
    computeType_ = (void .) .computeType
    
    
instance Typecheckable Procedure where
    typecheck (v, p) proc @ Procedure { procName     = name
                                      , procNested   = nested
                                      , procParamMap = params
                                      , procVars     = locals
                                      , procBody     = body 
                                      } = do
        nested' <- mapM (typecheck env') nested
        body'   <- mapM (typecheck env') body
        return proc { procNested = nested', procBody = body' }
        where env'    = putVars vars . putProcs nested $ clean
              clean   = (push name v, push name p)
              params' = varType <$> params
              locals' = varType <$> locals
              vars    = params' `M.union` locals'
    
              
instance Typecheckable (Stmt Properties) where
    typecheck env (Assignment l v e) = do
        (v', vt) <- computeType env v
        (e', et) <- computeType env e
        -- TODO: check assignability 
        return $ Assignment l v' e'
    
    typecheck env @ (vars, procs) c @ (ProcCall a n args) =
        case E.lookup name procs of
            Just proc -> do
                argst' <- mapM (computeType env) args
                -- TODO: check count and types
                return c
            Nothing -> do
                emitError loc $ "Unknown procedure: `" ++ name ++ "'"
                doFail_ c
        where name = getName n
              loc  = Just (a .#. "pos")
              
    typecheck env (IfThenElse props cond ifT ifF) = do
        (cond', _) <- ensureBoolean env cond "As an IF condition"
        ifT'       <- check ifT
        ifF'       <- check ifF
        return $ IfThenElse props cond' ifT' ifF'
        where check = mapM (typecheck env)
            
    typecheck env (While props cond body) = do 
        (cond', _) <- ensureBoolean env cond "As a WHILE condition" 
        body'      <- mapM (typecheck env) body
        return $ While props cond' body'

    
instance Typed (Expr Properties) where
    computeType env (VarExpr props v) = do
        (v', vt) <- computeType env v
        withType vt $ VarExpr props v'
        
    computeType env (LitExpr props lit) = do
        (lit', t) <- computeType env lit
        withType t $ LitExpr props lit' 
        
    computeType _ expr @ (CastExpr _ t _) = withType (ast2Type t) expr
        
    computeType env (UnaryExpr props op @ (Not _) e) = do
        (e', _) <- ensureBoolean env e "As a NOT argument"
        withType BooleanT $ UnaryExpr props op e'
        
    computeType env (BinaryExpr props op l r) = do
        (l', lt) <- computeType env l
        (r', rt) <- computeType env r
        let (t, cl, cr) = checkBin op lt rt
            l'' = coerce cl l'
            r'' = coerce cr r'
            e   = BinaryExpr props op l'' r''
        withType t e
        where checkBin (Less _) = cmp
              checkBin _        = arit
    
    
instance Typed (AST.Variable Properties) where
    computeType (vars, _) var @ (VarName props n) =
        case E.lookup name vars of
            Just t -> withType t var
            Nothing -> do emitError loc msg
                          setError
                          doFail var
                          where msg = "Unresolved variable: `" ++ name ++ "'"
        where name = getName n
              loc  = props .#. "pos"
              
    computeType env var @ (VarIndex props b i) = do
        (b', bt) <- computeType env b
        i' <- ensureIndex env i
        case bt of 
            ArrayT t _ _ -> withType t $ VarIndex props b' i'
            _ -> do emitError loc $ "Cannot index non-array type " ++ tstr
                    setError
                    doFail var
                    where tstr = getString $ out bt
                          loc  = Just $ props .#. "pos" 

instance Typed (AST.Literal Properties) where
    computeType _ var @ (LitInt _ _)  = withType IntegerT var
    computeType _ (LitMichal prop)    = withType IntegerT $ LitInt prop 3
    computeType _ var @ (LitReal _ _) = withType RealT var
    computeType _ var @ (LitTrue _)   = withType BooleanT var
    computeType _ var @ (LitFalse _)  = withType BooleanT var

instance Typecheckable (AST.Literal Properties) where
    typecheck env e = return fst `ap` computeType env e
    
instance Typecheckable (Expr Properties) where
    typecheck env e = return fst `ap` computeType env e

instance Typecheckable (AST.Variable Properties) where
    typecheck env e = return fst `ap` computeType env e




