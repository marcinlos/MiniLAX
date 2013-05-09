-- | Type system
module MiniLAX.Static.Types (
    Type (..),
    ParamKind (..),
    BinOp (..),
    UnOp (..),
    Coercion (..),
    checkBin
) where

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
          | TypeError (Maybe String) 
          deriving (Eq, Show)

-- | Represents var/val nature of procedure parameters    
data ParamKind = ByVar | ByVal  deriving (Eq, Show)

-- | Enumeration of binary operators
data BinOp = Plus | Times | Less deriving (Eq, Show)

-- | Enumeration of unary operators
data UnOp = Not deriving (Eq, Show)  


data Coercion = None | Int2Real | Real2Int 
    
    
checkBin :: BinOp -> Type -> Type -> (Type, Coercion, Coercion)
checkBin Plus  = arit
checkBin Times = arit
checkBin Less  = cmp

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
arit BooleanT BooleanT = exact $ TypeError (Just "Cannot add two boolean values")
arit _ _ = exact $ TypeError Nothing

cmp :: Type -> Type -> (Type, Coercion, Coercion) 
cmp IntegerT IntegerT = exact BooleanT
cmp IntegerT RealT    = leftC Int2Real BooleanT
cmp RealT IntegerT    = rightC Int2Real BooleanT
cmp RealT RealT       = exact BooleanT
cmp _ _ = exact $ TypeError Nothing
    
            
instance Printable Type where
    prettyPrint (ArrayT el low high) = do
        put "Array "; bracketed $ do
            put "Lower bound: " %% show low
            put "Upper bound: " %% show high
            put "Element type " >> bracketed (prettyPrint el) 

    prettyPrint IntegerT  = append "INTEGER"
    prettyPrint RealT     = append "REAL"
    prettyPrint BooleanT  = append "BOOLEAN"
    prettyPrint (TypeError (Just msg)) = append "(* error: " %% msg %% "*)"
    prettyPrint (TypeError Nothing) = append "(* error *)"

instance Printable ParamKind where
    prettyPrint ByVar = append "Var"
    prettyPrint ByVal = append "Val"
    
    