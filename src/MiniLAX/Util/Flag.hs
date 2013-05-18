{-# LANGUAGE FlexibleInstances #-}
-- | Monadic flag based on State monad, just for a simple interface
module MiniLAX.Util.Flag (
    MonadFlag (..),
    FlagT,
    runFlagT
) where

--
import Control.Monad.Trans.State


class (Monad m) => MonadFlag m where
    setFlagState :: Bool -> m ()
    
    checkFlag :: m Bool

    setFlag :: m ()
    setFlag = setFlagState True
    
    resetFlag :: m ()
    resetFlag = setFlagState False
    
type FlagT = StateT Bool

instance (Monad m) => MonadFlag (FlagT m) where
    setFlagState = put
    checkFlag    = get
    
runFlagT :: FlagT m a -> m (a, Bool)
runFlagT flag = runStateT flag False