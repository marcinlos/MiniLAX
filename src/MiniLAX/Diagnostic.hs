-- | Definitions of compiler diagnostic messages
module MiniLAX.Diagnostic where

-- | Location information may be desirable
import MiniLAX.Location

import Data.Maybe
import Control.Applicative

-- | Type of the message
data MsgType =
    Success     -- ^ Some action succeede
  | Info        -- ^ Generic neutral information
  | Debug       -- ^ Detailed debug information
  | Warn        -- ^ Warning
  | Error       -- ^ Error
  | Fatal       -- ^ Critical error
  deriving (Eq, Show, Ord)

-- | Diagnostic message structure
data Message = Message {
    msgType      :: MsgType,
    msgLocation  :: Maybe Location,
    msgText      :: String
} 

-- | Maybe it should differentiate between message types more?
--   For example, print failures red?
instance Show Message where
    show (Message tp loc text) = 
        typeInfo ++ locInfo ++ text
        where typeInfo = show tp ++ " "
              locInfo = fromMaybe "" (show <$> loc) 
              

msgSuccess :: Maybe Location -> String -> Message
msgSuccess = Message Success

msgInfo :: Maybe Location -> String -> Message
msgInfo    = Message Info

msgDebug :: Maybe Location -> String -> Message
msgDebug   = Message Debug

msgWarn :: Maybe Location -> String -> Message
msgWarn    = Message Warn

msgError :: Maybe Location -> String -> Message
msgError   = Message Error

msgFatal :: Maybe Location -> String -> Message
msgFatal   = Message Fatal
