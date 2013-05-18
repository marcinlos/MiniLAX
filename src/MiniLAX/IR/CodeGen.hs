-- | Code generation monad and helper functions
module MiniLAX.IR.CodeGen where

--
import Data.Sequence
import Data.Traversable
import Data.Foldable
import Data.Monoid

import Control.Applicative ((<$>))
import Control.Monad.Trans.State

import MiniLAX.IR


data GenState = GenState { stateNextLabel :: Int
                         , stateInstr     :: Seq IR
                         } 
                         
initState :: GenState
initState = GenState { stateNextLabel = 1
                     , stateInstr     = empty
                     }
                     
putInstr :: IR -> GenState -> GenState
putInstr code s @ GenState { stateInstr = instrs } = 
    s { stateInstr = instrs |> code }
    
                         
type CodeGen = State GenState


instrSeq :: CodeGen (Seq IR)
instrSeq = gets stateInstr

instrList :: CodeGen [IR]
instrList = gets $ toList . stateInstr

emit :: IR -> CodeGen ()
emit = modify . putInstr

mkLabel :: CodeGen Label
mkLabel = do
    n <- (+1) <$> gets stateNextLabel
    modify $ \x -> x { stateNextLabel = n }
    return $ Label n
    
emitLabel :: CodeGen Label
emitLabel = do
    l <- mkLabel
    emit $ PutLabel l
    return l
    
data CtrlDest = Return
              | Goto Label
              | FallThrough

continue :: CtrlDest -> CodeGen ()
continue Return      = emit Ret
continue (Goto dest) = emit $ Jump dest
continue FallThrough = return () 

followedBy :: CtrlDest -> CodeGen a -> CodeGen ()
followedBy dest cg = cg >> continue dest
              

