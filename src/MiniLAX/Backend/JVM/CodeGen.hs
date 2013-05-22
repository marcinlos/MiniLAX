-- | Code generation monad and functions
-- Well, ok, actually not "code" as in "bytecode", it's jasmin assembly.
module MiniLAX.Backend.JVM.CodeGen where

-- Imports
import Data.Map (Map)

import MiniLAX.IR
import MiniLAX.Printer
import MiniLAX.Static.Symbols

genJVM :: (Map String Procedure, String) -> PrinterMonad ()
genJVM = undefined