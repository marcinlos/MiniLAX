{-# LANGUAGE FlexibleInstances #-}
-- | Definitions of compiler diagnostic messages
module MiniLAX.Diagnostic where

-- | Location information may be desirable
import MiniLAX.Location

import Data.Maybe
import Data.Sequence
import Control.Applicative
import Control.Monad.Trans.Writer
import Control.Monad.Identity

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
        where typeInfo = "[" ++ show tp ++ "] "
              locInfo = fromMaybe "" (show <$> loc) 
              
-- | Auxilary functions shortening emiting diagnostics 
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

-- | Class of diagnostic-enabled monad
class (Monad m) => MonadDiag m where
    emit :: Message -> m ()
    
    emitSuccess :: Maybe Location -> String -> m ()
    emitSuccess = (emit .) . Message Success
    
    emitSuccess_ :: String -> m()
    emitSuccess_ = emitSuccess Nothing
    
    emitInfo :: Maybe Location -> String -> m ()
    emitInfo = (emit .) . Message Info
    
    emitInfo_ :: String -> m()
    emitInfo_ = emitInfo Nothing
    
    emitDebug :: Maybe Location -> String -> m ()
    emitDebug = (emit .) . Message Debug
    
    emitDebug_ :: String -> m()
    emitDebug_ = emitDebug Nothing
    
    emitWarn :: Maybe Location -> String -> m ()
    emitWarn = (emit .) . Message Warn
    
    emitWarn_ :: String -> m()
    emitWarn_ = emitWarn Nothing
    
    emitError :: Maybe Location -> String -> m ()
    emitError = (emit .) . Message Error
    
    emitError_ :: String -> m()
    emitError_ = emitError Nothing
    
    emitFatal :: Maybe Location -> String -> m ()
    emitFatal = (emit .) . Message Fatal
    
    emitFatal_ :: String -> m()
    emitFatal_ = emitFatal Nothing
    
    
type DiagT m = WriterT (Seq Message) m

type Diag a = DiagT Identity a

-- | Instance definition for DiagT
instance (Monad m) => MonadDiag (WriterT (Seq Message) m) where
    emit = tell . singleton 


