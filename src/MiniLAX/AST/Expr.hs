{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Extended expression AST
module MiniLAX.AST.Expr (

) where

--
import Data.HList
import Data.HList.Label2 

data PersonT = PersonT deriving (Show)

data NameL = NameL
instance Show NameL where
    show = const "name"
    
data AgeL = AgeL
instance Show AgeL where
    show = const "age"
    
data SizeL = SizeL
instance Show SizeL where
    show = const "size"


name = firstLabel PersonT NameL
age  = nextLabel name AgeL
size = nextLabel age SizeL
