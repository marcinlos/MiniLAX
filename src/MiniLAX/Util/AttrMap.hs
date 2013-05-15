-- | Dynamic attribute map
module MiniLAX.Util.AttrMap (
    AttrMap,
    Properties,
    emptyAttr,
    putAttr,
    tryAttr,
    getAttr,
    hasAttr,
    singleton,
    (.#.)
) where

--
import Data.Typeable
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe


newtype AttrMap a = AttrMap { attrMap :: Map a Dynamic }
    deriving (Show)

type Properties = AttrMap String

infix 9 .#.

emptyAttr :: AttrMap a
emptyAttr = AttrMap M.empty

putAttr :: (Ord a, Typeable b) => a -> b -> AttrMap a -> AttrMap a
putAttr k v AttrMap { attrMap = m } = AttrMap $ M.insert k (toDyn v) m 

-- Pointfree: ((flip (.) attrMap) .) . (((((AttrMap .) .) . (. toDyn)) .) M.insert)

tryAttr :: (Ord a, Typeable b) => a -> AttrMap a -> Maybe b
tryAttr k AttrMap { attrMap = m } = M.lookup k m >>= fromDynamic

getAttr :: (Ord a, Typeable b) => a -> AttrMap a -> b
getAttr = (fromJust .) . tryAttr

hasAttr :: (Ord a) => a -> AttrMap a -> Bool
hasAttr k = M.member k . attrMap

singleton :: (Typeable b) => a -> b -> AttrMap a
singleton k = AttrMap . M.singleton k . toDyn

(.#.) :: (Ord a, Typeable b) => AttrMap a -> a -> b
(.#.) = flip getAttr