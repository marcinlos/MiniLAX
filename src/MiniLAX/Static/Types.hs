{-# LANGUAGE DeriveDataTypeable #-}
-- | Type system
module MiniLAX.Static.Types (
    Type (..),
    ParamKind (..),
    Coercion (..),
    MayHaveType (..),
    isArray,
    arrayBaseType,
    arrayDim,
    arrayBounds,
    arit,
    cmp,
    coercion
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
                   , arrayLower    :: Integer
                   , arrayUpper    :: Integer
                   }
          | TypeError
          deriving (Eq, Show, Typeable)

-- | Represents var/val nature of procedure parameters    
data ParamKind = ByVar | ByVal  deriving (Eq, Show)


arrayBaseType :: Type -> Type
arrayBaseType (ArrayT t _ _) = arrayBaseType t
arrayBaseType t = t


arrayDim :: Type -> Integer
arrayDim (ArrayT t _ _) = arrayDim t + 1
arrayDim _ = 0

arrayBounds :: Type -> [(Integer, Integer)]
arrayBounds (ArrayT t low high) = (low, high) : arrayBounds t
arrayBounds _ = []

class MayHaveType a where
    getType :: a -> Maybe Type
    justGetType :: a -> Type
    justGetType = fromJust . getType
    
instance MayHaveType Type where getType = Just


data Coercion = None | Int2Real | Real2Int 
    deriving (Eq, Show)

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
arit RealT RealT       = exact RealT
arit BooleanT BooleanT = exact TypeError
arit _ _ = exact TypeError 

cmp :: Type -> Type -> (Type, Coercion, Coercion) 
cmp IntegerT IntegerT = exact BooleanT
cmp IntegerT RealT    = leftC Int2Real BooleanT
cmp RealT IntegerT    = rightC Int2Real BooleanT
cmp RealT RealT       = exact BooleanT
cmp _ _ = exact TypeError

isArray :: Type -> Bool
isArray ArrayT {} = True
isArray _ = False

coercion :: Type -> Type -> Maybe Coercion
coercion IntegerT IntegerT = Just None
coercion RealT RealT = Just None
coercion IntegerT RealT = Just Real2Int
coercion RealT IntegerT = Just Int2Real
coercion a b | a == b    = Just None
             | otherwise = Nothing
    
            
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
    
    