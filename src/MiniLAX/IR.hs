-- | Definitions of intermediate representation elements
module MiniLAX.IR where

-- | Label
newtype Label = Label { labelNum :: Int }


-- | Address
data Addr = Local Int   -- ^ Local variable 
          | Tmp Int     -- ^ Value on the stack
          | CInt Int    -- ^ Integer constant
          | CReal Float -- ^ Real constant