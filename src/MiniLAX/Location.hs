-- | Module containing definitions of structures and functions providing an
--   easy way to carry and display information about position in the source
--   code.
module MiniLAX.Location (
    Location (..),
    SourceElement (..)
) where

-- | Position information for source code constructs
data Location = Location { 
    getFile :: String,
    getLine :: Int,
    getCol  :: Int
}

-- | Typeclass of a source code level construct, having location
class SourceElement a where
    getLocation :: a -> Location
    
-- | Pretty-prints location information 
instance Show Location where
    show (Location _ line col) = show (line, col)
