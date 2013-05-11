-- |
module MiniLAX.IR.Generate where

-- 
import Data.Monoid
import Control.Monad.Trans.State

import MiniLAX.IR as IR
import MiniLAX.Static.Symbols
import MiniLAX.AST.Annotated as AST
import MiniLAX.Static.Types as T

import Data.List

data GenState = GenState { }

initState :: GenState
initState = GenState { }

data CodePart = CodePart { codeInstr :: [IR] }
 
instance Monoid CodePart where
    mempty = CodePart []
    a `mappend` b = CodePart $ codeInstr a ++ codeInstr b 



generateIR :: Procedure -> CodePart
generateIR p = fst $ runState (gen p) initState  


gen :: Procedure -> State GenState CodePart
gen = undefined

simple :: [IR] -> State GenState CodePart
simple = return . CodePart


genExpr :: (MayHaveType a) => Procedure -> Expr a -> State GenState CodePart
genExpr _ (LitExpr _ literal) = 
    case literal of
        LitInt _ n  -> simple [LoadIntConst n]
        LitReal _ x -> simple [LoadRealConst x]
        LitMichal _ -> simple [LoadIntConst 3]
        LitTrue _   -> simple [LoadBoolConst True]
        LitFalse _  -> simple [LoadBoolConst False]

genExpr p (CastExpr _ t e) =
    return . (<> CodePart [cast]) =<< genExpr p e
    where cast = case t of TyInt _  -> IR.Real2Int
                           TyReal _ -> IR.Int2Real
                           _ -> error "Invalid cast"
genExpr p (UnaryExpr _ op e) =
    case op of
       AST.Not _ -> return . (<> CodePart [NotBool]) =<< genExpr p e
                                      
genExpr p (BinaryExpr t op left right) = do
    left'  <- genExpr p left
    right' <- genExpr p right
    return $ left' <> right' <> CodePart [chooseBinOp t' op]
    where t' = justGetType $ attr left

genExpr p (VarExpr _ var) = do
    undefined
    
chooseBinOp :: T.Type -> AST.BinOp a -> IR
chooseBinOp IntegerT (AST.Plus _)  = AddInt
chooseBinOp IntegerT (AST.Times _) = MulInt
chooseBinOp IntegerT (AST.Less _)  = LessInt
chooseBinOp RealT (AST.Plus _)     = AddReal
chooseBinOp RealT (AST.Times _)    = MulReal
chooseBinOp RealT (AST.Less _)     = LessReal
chooseBinOp _ _ = error "Invalid binary operation"
    
    
genVar :: (MayHaveType a) => 
    Procedure -> AST.Variable a -> State GenState CodePart
genVar p (VarName t (Name _ n)) = simple [load]
    where load = case tp of IntegerT -> LoadIntVar n
                            RealT    -> LoadRealVar n
                            _ -> error "Invalid type"
          tp = justGetType t
          
genVar p (VarIndex _ (VarName _ (Name _ n)) idx) =
    genExpr p idx >>= return . (CodePart [LoadArray n] <>) 

genVar p (VarIndex t base idx) = do 
    base' <- genVar p base
    idx'  <- genExpr p idx
    return $ base' <> idx'
    
genVal :: (MayHaveType a) =>
    Procedure -> AST.Variable a -> State GenState CodePart
genVal = undefined
    
    
opArith :: AST.BinOp a -> Bool
opArith (AST.Plus _)  = True
opArith (AST.Times _) = True
opArith _ = False 
    
    
    
    
    
    
    
    