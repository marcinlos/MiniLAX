-- | Datatype representing environment (variable bindings)
module MiniLAX.Static.Env (
    Env (..),
    empty,
    root,
    insert,
    lookup,
    lookupWithPath,
    push,
    pushAll,
    pushLayer,
    pop,
    path
) where

-- Imports
import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Data.Map (Map)
import qualified Data.Map as M

-- 
type SMap = Map String
type Label = String

-- | Type representing environment
newtype Env a = Env { getEnv :: [(Label, SMap a)] }
    deriving (Show)

apply :: ([(Label, SMap a)] -> [(Label, SMap a)]) -> Env a -> Env a
apply f = Env . f . getEnv

insertAux :: String -> a -> (Label, SMap a) -> (Label, SMap a)
insertAux s v (l, m) = (l, M.insert s v m) 

lookupChain :: String -> [(Label, SMap a)] -> Maybe (a, [Label])
lookupChain s ((_, m) : es) = 
    case M.lookup s m of
        Just v -> return (v, fst <$> es)
        _      -> lookupChain s es
        
lookupChain _ [] = Nothing
    


empty :: Env a
empty = Env { getEnv = [] }

root :: String -> Env a
root s = Env { getEnv = [(s, M.empty)] }

insert :: String -> a -> Env a -> Env a
insert s v (Env (e : es)) = Env (insertAux s v e : es)
insert _ _ _ = error "Env.insert: empty environment"

lookup :: String -> Env a -> Maybe a
lookup s (Env e) = fst <$> lookupChain s e

lookupWithPath :: String -> Env a -> Maybe (a, [Label])
lookupWithPath s (Env e) = lookupChain s e

push :: String -> Env a -> Env a
push s = apply ((s, M.empty) :)

pushAll :: SMap a -> Env a -> Env a
pushAll m (Env ((l, vs) : es)) = Env ((l, m `M.union` vs) : es)  
pushAll _ _ = error "Env.pushAll: empty environment"

pushLayer :: String -> SMap a -> Env a -> Env a
pushLayer s m = pushAll m . push s

pop :: Env a -> Env a
pop = apply init

path :: Env a -> [String]
path = reverse . (fst <$>) . getEnv

 

