{-# LANGUAGE FlexibleInstances #-}
-- |
module MiniLAX.IR.Generate where

--
import Prelude hiding (mapM)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Control.Monad.Trans.State

import MiniLAX.IR as IR
import MiniLAX.Static.Symbols
import MiniLAX.AST.Annotated as AST
import MiniLAX.Static.Types as T
import MiniLAX.Printer
import MiniLAX.Util.AttrMap
import MiniLAX.IR.CodeGen

type Code = [IR]

printCode :: Code -> PrinterMonad ()
printCode = mapM_ prettyPrint

instance MayHaveType Properties where
    getType = tryAttr "type"
    
type ProcMap = Map String Procedure

type Context = ProcMap


printProceduresIR :: SMap (Procedure, Code) -> PrinterMonad ()
printProceduresIR procs = mapM_ (uncurry printProcIR . snd) $ M.toList procs

printProcIR :: Procedure -> Code -> PrinterMonad ()
printProcIR proc code = do 
    append (procName proc) >> endl 
    append $ replicate 30 '-'
    endl
    indentedBy 2 $ printCode code
    endl

generateIR :: Context -> Procedure -> Code
generateIR ctx p = fst $ runState (gen ctx p) initState  

gen :: Context -> Procedure -> CodeGen Code
gen ctx Procedure { procBody = body } = do
    mapM_ (genStmt ctx) body
    emit Ret 
    instrList
    
genArithCmp :: (MayHaveType a) => 
    Context -> Expr a -> 
    (T.Type -> Label -> IR) -> 
    Label -> CodeGen ()
genArithCmp ctx (BinaryExpr _ (Less _) l r) comp dest = do
    genExpr ctx l
    genExpr ctx r
    emit $ comp t dest
    where t = justGetType $ attr l
    
genArithCmp _ _ _ _ = error "Internal error: Not an arithmetic comparison"
    
genBranch :: (MayHaveType a) => Context -> Expr a -> Label -> CodeGen ()
genBranch ctx e @ (BinaryExpr _ (Less _) _ _) dest =
    genArithCmp ctx e lessOf dest
    where lessOf IntegerT = IfLessInt
          lessOf RealT    = IfLessReal
          lessOf _ = error "Internal error: Invalid type in comparison"
          
genBranch proc e dest = do
    genExpr proc e
    emit $ IfBool dest

genBranchNeg :: (MayHaveType a) => Context -> Expr a -> Label -> CodeGen ()
genBranchNeg ctx e @ (BinaryExpr _ (Less _) _ _) dest = 
    genArithCmp ctx e lessOf dest
    where lessOf IntegerT = IfGteInt
          lessOf RealT    = IfGteReal
          lessOf _ = error "Internal error: Invalid type in comparison (neg cmp)"

genBranchNeg ctx e dest = do
    genExpr ctx e
    emit $ IfNotBool dest

genStmt :: (MayHaveType a) => Context -> Stmt a -> CodeGen ()
genStmt ctx (Assignment _ l r) = do
    genVar ctx l
    genExpr ctx r
    emit $ storeArrayByType t
    where t = justGetType $ attr l  
    
genStmt proc (IfThenElse _ cond ifT ifF) = do
    labElse  <- mkLabel
    labAfter <- mkLabel
    genBranchNeg proc cond labElse
    genStmt proc `mapM_` ifT
    emit $ Jump labAfter
    emitLabel labElse
    genStmt proc `mapM_` ifF
    emitLabel labAfter 
    
genStmt proc (While _ cond body) = do
    labBefore <- emitNewLabel
    labAfter  <- mkLabel
    genBranchNeg proc cond labAfter
    genStmt proc `mapM_` body
    emit $ Jump labBefore
    emitLabel labAfter
    
genStmt ctx (ProcCall _ (Name _ n) args) = do
    let Procedure { procParams = params } = ctx ! n
    emit $ PreCall n
    mapM_ pushParam' $ zipWith ((,) . paramKind) params args
    emit $ Call n
    where pushParam' = uncurry $ pushParam ctx
    
genStmt ctx (Write _ e) = do
    genExpr ctx e
    emit $ writeByType t
    where t = justGetType $ attr e
    
genStmt ctx (Read _ v) = do
    genVar ctx v
    emit $ readByType t
    where t = justGetType $ attr v
    
pushParam :: (MayHaveType a) => Context -> ParamKind -> Expr a -> CodeGen ()
pushParam ctx ByVal e = genExpr ctx e
pushParam ctx ByVar (VarExpr _ e) = genVar ctx e
pushParam _ _ _ = error "Internal error: Trying to use non-lvalue as VAR"
    
    
loadLit :: Literal a -> IR          
loadLit (LitInt _ n)  = LoadIntConst n
loadLit (LitReal _ x) = LoadRealConst x 
loadLit (LitMichal _) = LoadIntConst 3
loadLit (LitTrue _)   = LoadBoolConst True
loadLit (LitFalse _)  = LoadBoolConst False


genExpr :: (MayHaveType a) => Context -> Expr a -> CodeGen ()
genExpr _ (LitExpr _ literal) = emit $ loadLit literal
             

genExpr ctx (CastExpr _ t e) = genExpr ctx e >> emit cast
    where cast = case t of TyInt _  -> IR.Real2Int
                           TyReal _ -> IR.Int2Real
                           _ -> error "Invalid cast"
                           
genExpr ctx (UnaryExpr _ (AST.Not _) e) = genExpr ctx e >> emit NotBool
                                   
                                      
genExpr ctx (BinaryExpr _ op left right) = do
    genExpr ctx left
    genExpr ctx right
    emit $ chooseBinOp t' op
    where t' = justGetType $ attr left

genExpr p (VarExpr _ var) = genVal p var
    
chooseBinOp :: T.Type -> AST.BinOp a -> IR
chooseBinOp IntegerT (AST.Plus _)  = AddInt
chooseBinOp IntegerT (AST.Times _) = MulInt
chooseBinOp IntegerT (AST.Less _)  = LessInt
chooseBinOp RealT (AST.Plus _)     = AddReal
chooseBinOp RealT (AST.Times _)    = MulReal
chooseBinOp RealT (AST.Less _)     = LessReal
chooseBinOp _ _ = error "Invalid binary operation"
    
    
genVar :: (MayHaveType a) => Context -> AST.Variable a -> CodeGen ()
genVar _ (VarName t (Name _ n)) =
    emit load
    --unless (isArray tp) $ emit $ LoadIntConst 0
    where load = case tp of BooleanT  -> LoadBoolVar n
                            IntegerT  -> LoadIntVar n
                            RealT     -> LoadRealVar n
                            ArrayT {} -> LoadArray n
                            _ -> error $ "Internal error: Invalid type " ++ 
                                         show tp ++ " (genVar)"
          tp = justGetType t
          
genVar ctx (VarIndex _ (VarName _ (Name _ n)) idx) = do
    emit $ LoadArray n
    genExpr ctx idx

genVar ctx (VarIndex _ base idx) = do 
    genVar ctx base
    emit FetchArrayArray
    genExpr ctx idx
    
genVal :: (MayHaveType a) => Context -> AST.Variable a -> CodeGen ()
genVal ctx v = do 
    genVar ctx v
    emit $ fetchArrayByType t
    where t = justGetType $ attr v 
    
    
opArith :: AST.BinOp a -> Bool
opArith (AST.Plus _)  = True
opArith (AST.Times _) = True
opArith _ = False 
    
    

    