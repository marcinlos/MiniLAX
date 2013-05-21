-- | Code generation monad and helper functions
module MiniLAX.IR.CodeGen (
    GenState (..),
    initState,
    CodeGen,
    putInstr,
    instrSeq,
    instrList,
    emit,
    mkLabel,
    emitLabel,
    emitNewLabel,
    CtrlDest,
    continue,
    followedBy
) where

--
import Data.Sequence
import Data.Foldable

import Control.Monad.Trans.State

import MiniLAX.IR


data GenState = GenState { stateNextLabel :: Int
                         , stateInstr     :: Seq IR
                         } 
                         
initState :: GenState
initState = GenState { stateNextLabel = 0
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
    n <- gets stateNextLabel
    modify $ \x -> x { stateNextLabel = n + 1 }
    return $ Label n
    
emitLabel :: Label -> CodeGen ()
emitLabel = emit . PutLabel
    
emitNewLabel :: CodeGen Label
emitNewLabel = do
    l <- mkLabel
    emitLabel l
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
              

