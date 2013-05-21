-- | Definitions of intermediate representation elements
module MiniLAX.IR where

-- 
import MiniLAX.Printer
import MiniLAX.Static.Types

-- | Label
newtype Label = Label Int deriving (Eq)

instance Show Label where
    show (Label n) = 'L' : show n

data IR = LoadInt String
        | LoadReal String
        | LoadBool String
        | LoadIntVar String
        | LoadRealVar String
        | LoadBoolVar String
        | LoadIntConst Int
        | LoadRealConst Float
        | LoadBoolConst Bool
        | LoadArray String
        | FetchArrayInt
        | FetchArrayReal
        | FetchArrayBool
        | FetchArrayArray
        | StoreInt String
        | StoreReal String
        | StoreBool String
        | StoreArrayInt
        | StoreArrayReal
        | StoreArrayBool
        | Jump Label
        | PreCall String
        | Call String
        | IfBool Label
        | IfNotBool Label
        | IfGteInt Label
        | IfGteReal Label
        | IfLessInt Label
        | IfLessReal Label
        | LessInt 
        | LessReal
        | NotBool
        | AddInt
        | AddReal
        | MulInt
        | MulReal
        | Int2Real
        | Real2Int 
        | WrapArgInt String
        | WrapArgReal String
        | WrapArgBool String
        | PutLabel Label
        | Ret
        deriving (Eq, Show)


instance Printable Label where
    prettyPrint = append . show 

        
instance Printable IR where
    prettyPrint (PutLabel lab) = prettyPrint lab %% ":" >> endl
    prettyPrint i = (put $ show i) >> endl
    
fetchArrayByType :: Type -> IR
fetchArrayByType IntegerT = FetchArrayInt
fetchArrayByType RealT    = FetchArrayReal
fetchArrayByType BooleanT = FetchArrayBool
fetchArrayByType ArrayT {} = FetchArrayArray

storeArrayByType :: Type -> IR
storeArrayByType IntegerT = StoreArrayInt
storeArrayByType RealT    = StoreArrayReal
storeArrayByType BooleanT = StoreArrayBool

