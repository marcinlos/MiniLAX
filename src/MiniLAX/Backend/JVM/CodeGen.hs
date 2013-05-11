-- | Code generation monad and functions
-- Well, ok, actually not "code" as in "bytecode", it's jasmin assembly.
module MiniLAX.Backend.JVM.CodeGen where

-- Imports
import Control.Monad.Trans.Error
import Control.Monad.Trans.State

import MiniLAX.Diagnostic
import MiniLAX.IR
import MiniLAX.Printer

type ErrMsg = String

-- | Code generation monad
newtype CodeGen a = CodeGen { 
    runCodeGen :: DiagT (State CodeGenState) a
}

-- | State maintained during generating the code
data CodeGenState = CodeGenState {
    
}

