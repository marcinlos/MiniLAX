{-# LANGUAGE FlexibleInstances #-}
-- |
module MiniLAX.IR.Generate where

--
import Prelude hiding (mapM)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Trans.State
import Data.Traversable
import Control.Monad (void, liftM, liftM2)

import MiniLAX.IR as IR
import MiniLAX.Static.Symbols
import MiniLAX.AST.Annotated as AST
import MiniLAX.Static.Types as T
import MiniLAX.Printer
import MiniLAX.Util.AttrMap


data GenState = GenState { genStateNextLabel :: Int }

initState :: GenState
initState = GenState { genStateNextLabel = 1 }

data CodePart = CodePart { codeInstr :: [IR] }


type GenIR = State GenState
 
instance Monoid CodePart where
    mempty = CodePart []
    a `mappend` b = CodePart $ codeInstr a ++ codeInstr b 

instance Printable CodePart where
    prettyPrint = mapM_ prettyPrint . codeInstr

instance MayHaveType Properties where
    getType = tryAttr "type"
    
type ProcMap = Map String Procedure

printProceduresIR :: ProcMap -> PrinterMonad ()
printProceduresIR procs = void $ mapM printProcIR procs

printProcIR :: Procedure -> PrinterMonad ()
printProcIR proc @ Procedure { procName = name } = do 
    append name >> endl 
    append $ replicate 30 '-'
    endl
    indentedBy 2 $ prettyPrint $ generateIR proc
    endl

generateIR :: Procedure -> CodePart
generateIR p = fst $ runState (gen p) initState  

infixl 2 .||.

(.||.) :: CodePart -> IR -> CodePart
part .||. instr = part <> CodePart [instr]

gen :: Procedure -> GenIR CodePart
gen proc @ Procedure { procBody = body } = 
    liftM mconcat $ mapM (genStmt proc) body

simple :: [IR] -> GenIR CodePart
simple = return . CodePart

genStmt :: (MayHaveType a) => Procedure -> Stmt a -> GenIR CodePart
genStmt proc (Assignment props l r) = do
    l' <- genVar proc l
    r' <- genExpr proc r
    return $ l' <> r' .||. (storeArrayByType $ justGetType (attr l))  


genExpr :: (MayHaveType a) => Procedure -> Expr a -> GenIR CodePart
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
                                      
genExpr p e @ (BinaryExpr t op left right) = do
    left'  <- genExpr p left
    right' <- genExpr p right
    return $ left' <> right' .||. chooseBinOp t' op
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
    
    
genVar :: (MayHaveType a) => Procedure -> AST.Variable a -> GenIR CodePart
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
    return $ (base' .||. FetchArrayArray) <> idx' 
    
genVal :: (MayHaveType a) => Procedure -> AST.Variable a -> GenIR CodePart
genVal p v = liftM2 (.||.) (genVar p v) (return $ fetchArrayByType t)
    where t = justGetType $ attr v 
    
    
opArith :: AST.BinOp a -> Bool
opArith (AST.Plus _)  = True
opArith (AST.Times _) = True
opArith _ = False 
    
    
    
    
    
    
    
    