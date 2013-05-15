-- | Definitions of intermediate representation elements
module MiniLAX.IR where

-- 
import MiniLAX.Printer

-- | Label
newtype Label = Label Int deriving (Eq, Show)

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
        | Return
        deriving (Eq, Show)


instance Printable Label where
    prettyPrint (Label n) = append $ "L" ++ show n 

        
instance Printable IR where
    prettyPrint (PutLabel lab) = prettyPrint lab %% ":" >> endl
    prettyPrint i = (put $ show i) >> endl



{-
-- | Address
data Addr = Local Int   -- ^ Local variable 
          | Tmp Int     -- ^ Value on the stack
          | CInt Int    -- ^ Integer constant
          | CReal Float -- ^ Real constant
-} 
   
{-       
data IR = 
    AddI 
  | AddF
  | MulI
  | MulF
  | LoadI Addr
  | LoadF Addr
  | LoadB Addr
  | ALoadI Addr
  | ALoadF Addr
  | ALoadB Addr
  | ALoadA Addr
  | StoreI Addr
  | StoreF Addr
  | StoreB Addr
  | AStoreI Addr
  | AStoreF Addr
  | AStoreB Addr
  | AStoreA Addr
  | IfLess Label
  | Int2Float
  | Float2Int
  | Call String
  | Ret (Maybe Addr)
  | PutLabel Label
-}  
