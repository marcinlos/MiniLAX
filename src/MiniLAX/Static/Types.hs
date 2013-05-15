{-# LANGUAGE DeriveDataTypeable #-}
-- | Type system
module MiniLAX.Static.Types (
    Type (..),
    ParamKind (..),
    Coercion (..),
    MayHaveType (..),
    arit,
    cmp
) where

--
import Data.Maybe

--
import Data.Typeable
-- | Pretty-printing instances
import MiniLAX.Printer

-- | MiniLAX data type
data Type = IntegerT 
          | RealT 
          | BooleanT 
          | ArrayT { arrayElemType :: Type 
                   , arrayLower    :: Int
                   , arrayUpper    :: Int 
                   }
          | TypeError
          deriving (Eq, Show, Typeable)

-- | Represents var/val nature of procedure parameters    
data ParamKind = ByVar | ByVal  deriving (Eq, Show)


class MayHaveType a where
    getType :: a -> Maybe Type
    justGetType :: a -> Type
    justGetType = fromJust . getType
    
instance MayHaveType Type where getType = Just


data Coercion = None | Int2Real | Real2Int 
    

exact :: Type -> (Type, Coercion, Coercion)
exact t = (t, None, None)

leftC :: Coercion -> Type -> (Type, Coercion, Coercion)
leftC c t = (t, c, None)

rightC :: Coercion -> Type -> (Type, Coercion, Coercion)
rightC c t = (t, None, c)

arit :: Type -> Type -> (Type, Coercion, Coercion) 
arit IntegerT IntegerT = exact IntegerT
arit IntegerT RealT    = leftC Int2Real RealT
arit RealT IntegerT    = rightC Int2Real RealT
arit BooleanT BooleanT = exact TypeError
arit _ _ = exact $ TypeError 

cmp :: Type -> Type -> (Type, Coercion, Coercion) 
cmp IntegerT IntegerT = exact BooleanT
cmp IntegerT RealT    = leftC Int2Real BooleanT
cmp RealT IntegerT    = rightC Int2Real BooleanT
cmp RealT RealT       = exact BooleanT
cmp _ _ = exact $ TypeError
    
            
instance Printable Type where
    prettyPrint (ArrayT el low high) = do
        put "Array "; bracketed $ do
            put "Lower bound: " %% show low
            put "Upper bound: " %% show high
            put "Element type " >> bracketed (prettyPrint el) 

    prettyPrint IntegerT  = append "INTEGER"
    prettyPrint RealT     = append "REAL"
    prettyPrint BooleanT  = append "BOOLEAN"
    prettyPrint TypeError = append "(* type error *)"

instance Printable ParamKind where
    prettyPrint ByVar = append "Var"
    prettyPrint ByVal = append "Val"
    
    