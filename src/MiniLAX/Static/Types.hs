-- | Type system
module MiniLAX.Static.Types (
    Type (..),
    ParamKind (..)
) where

-- | Pretty-printing instances
import MiniLAX.Printer

-- | MiniLAX data type
data Type = IntegerT | RealT | BooleanT 
  | ArrayT { 
        arrayElemType :: Type, 
        arrayLower    :: Int, 
        arrayUpper    :: Int 
    } 
  deriving (Eq, Show)

-- | Represents var/val nature of procedure parameters    
data ParamKind = VarParam | ValParam
    deriving (Eq, Show)

    
            
instance Printable Type where
    prettyPrint (ArrayT el low high) = do
        put "Array "; bracketed $ do
            put "Lower bound: " %% show low >> endl
            put "Upper bound: " %% show high >> endl
            put "Element type " >> bracketed (prettyPrint el) 

    prettyPrint IntegerT = put "INTEGER" >> endl
    prettyPrint RealT    = put "REAL" >> endl
    prettyPrint BooleanT = put "BOOLEAN" >> endl
    
instance Printable ParamKind where
    prettyPrint VarParam = put "Var" >> endl
    prettyPrint ValParam = put "Val" >> endl