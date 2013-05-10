-- | Functions for introducing level of indirection in arbitrary Traversable
-- structures. Actual content is replaced with identifiers, data contained
-- in the structures previously is placed in a map.
module MiniLAX.Util.LabelGen (
    generateLabels
) where

--
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable
import Control.Monad.Trans.State

{-
class MonadLabel m a b | m -> a b where
    nextId :: a -> m b
-}

data GenState l a = GenState { nextLabel :: l
                             , values    :: Map l a
                             }
                             
                             
extract :: (Ord l) => (a -> l -> l) -> a -> State (GenState l a) l
extract f a = do
    GenState l m <- get
    let l' = f a l
        m' = M.insert l' a m 
    put $ GenState l' m' 
    return l'


generateLabels :: (Ord l, Traversable t) => 
    (a -> l -> l) -> 
    l -> 
    t a -> 
    (t l, Map l a, l)
generateLabels f l t = (t', vals, l')
    where (t', GenState l' vals) = runState res (GenState l M.empty)
          res = traverse (extract f) t
              