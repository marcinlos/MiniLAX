-- | Module containing definitions of structures and functions providing an
--   easy way to carry and display information about position in the source
--   code.
module MiniLAX.Location (
    Location (..),
    HasLocation (..),
    locationUnknown
) where

-- | Name clash 
import Prelude hiding (getLine)

-- | Position information for source code constructs
data Location = Location { 
    getFile :: String,
    getLine :: Int,
    getCol  :: Int
}

locationUnknown :: Location
locationUnknown = Location {
    getFile = "?",
    getLine = 0,
    getCol = 0
}

-- | Typeclass of a being having location
class HasLocation a where
    getLocation :: a -> Location
    
-- | Pretty-prints location information 
instance Show Location where
    show (Location _ line col) = show (line, col)
