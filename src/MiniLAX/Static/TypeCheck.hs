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

import MiniLAX.Static.Types hiding (getType)
import MiniLAX.AST.Annotated hiding (Type, Program)
import qualified MiniLAX.AST.Annotated as AST
import MiniLAX.Printer
import MiniLAX.AST.PrettyPrint
import MiniLAX.Diagnostic
import MiniLAX.Location
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
                    
type VarP = AST.Variable Properties
                    
ensureAssignable :: (Functor m, MonadDiag m, MonadFlag m) => 
    TypeEnv -> VarP -> m (VarP, Type)
ensureAssignable env v = do
    (v', t) <- computeType env v
    if isArray t then do let pos = Just $ attr v .#. "pos"
                             msg = "Cannot assign to array type"
                         emitError pos msg
                         doFail v
                 else withType t v'
                 
ensureConvertibleTo :: (Functor m, MonadDiag m, MonadFlag m) =>
    TypeEnv -> Type -> ExprP -> m (ExprP, Type)
ensureConvertibleTo env t e = do
    (e', t') <- computeType env e
    case coercion t t' of
        Just c -> return (coerce c e', t)
        Nothing -> do let pos = Just $ attr e .#. "pos"
                          msg = "Cannot convert " ++ src ++ " to " ++ target 
                          src = getString $ out t'
                          target = getString $ out t
                      emitError pos msg
                      doFail e
                      
ensureUsableAsVar :: (Functor m, MonadDiag m, MonadFlag m) =>
    TypeEnv -> Type -> ExprP -> m (ExprP, Type)
ensureUsableAsVar env t e = do
    res @ (_, t') <- computeType env e
    case coercion t t' of
        Just None 
            | isVar e   -> return res
            | otherwise -> do 
                let pos = Just $ attr e .#. "pos"
                    msg = "Cannot use expression as a VAR argument, it " ++
                          "isn't L-value"
                emitError pos msg
                doFail e
        Just c -> do 
            let pos = Just $ attr e .#. "pos"
                msg = "Cannot use expression as a VAR argument - " ++
                      "actual type " ++ srcStr ++ " does not match " ++
                      "required VAR type " ++ paramStr ++ " exactly, " ++
                      "coercion " ++ show c ++ " needed"
                srcStr = getString $ out t' 
                paramStr = getString $ out t  
            emitError pos msg
            doFail e
        Nothing -> do 
            let pos = Just $ attr e .#. "pos"
                msg = "Cannot use expression as a VAR argument - " ++
                      "actual type " ++ srcStr ++ " does not match " ++
                      "required VAR type " ++ paramStr
                srcStr = getString $ out t' 
                paramStr = getString $ out t  
            emitError pos msg
            doFail e
    where isVar VarExpr {} = True
          isVar _          = False
    
    
checkProcCall :: (Functor m, MonadDiag m, MonadFlag m) =>
    TypeEnv -> Maybe Location -> Procedure -> [ExprP] -> m [ExprP]
checkProcCall env pos Procedure { procName = name, procParams = params } es = do
    let expected = length params
        actual   = length es
    when (expected /= actual) $ do 
        let what = if expected < actual then "Too many parameters"
                                        else "Missing parameters"
            msg = what ++ " in a call of procedure `" ++
                  name ++ "' (" ++ show actual ++ " present, " ++ 
                  show expected ++ " expected)"
        emitError pos msg
        setError
    mapM check $ zip params es
    where check = uncurry $ checkArg env


checkArg :: (Functor m, MonadDiag m, MonadFlag m) => 
    TypeEnv -> Parameter -> ExprP -> m ExprP
checkArg env param e =
    let t  = paramType param
        check = case paramKind param of
                    ByVal -> ensureConvertibleTo
                    ByVar -> ensureUsableAsVar
    in fst <$> check env t e
            

withType :: (Monad m, Annotated a Properties) => Type -> a -> m (a, Type)
withType t a = return (setType t a, t) 
 

doFail :: (MonadDiag m, MonadFlag m, Annotated a Properties) => a -> m (a, Type) 
doFail e = setError >> withType TypeError e

doFail_ :: (MonadDiag m, MonadFlag m, Annotated a Properties) => a ->  m a
doFail_ m = return fst `ap` doFail m 


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
        (v', vt) <- ensureAssignable env v
        (e', _) <- ensureConvertibleTo env vt e
        return $ Assignment l v' e'
    
    typecheck env @ (_, procs) c @ (ProcCall a n args) =
        case E.lookup name procs of
            Just proc -> do
                args' <- checkProcCall env loc proc args
                return $ ProcCall a n args'
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
        
    computeType env expr @ (BinaryExpr props op l r) = do
        (l', lt) <- computeType env l
        (r', rt) <- computeType env r
        let (t, cl, cr) = checkBin op lt rt
            l'' = coerce cl l'
            r'' = coerce cr r'
            e   = BinaryExpr props op l'' r''
        case t of
            TypeError -> do 
                let pos = Just $ attr expr .#. "pos"
                    msg = "Cannot apply binary operation '" ++ opStr ++ "'" ++ 
                          " to arguments of types " ++ lStr ++ " and " ++ rStr
                    lStr  = getString $ out lt
                    rStr  = getString $ out rt 
                    opStr = getString $ out op
                emitError pos msg
                doFail expr 
            _ -> withType t e
        where checkBin (Less _) = cmp
              checkBin _        = arit
    
    
instance Typed (AST.Variable Properties) where
    computeType (vars, _) var @ (VarName props n) =
        case E.lookup name vars of
            Just t -> withType t var
            Nothing -> do 
                emitError loc msg
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




