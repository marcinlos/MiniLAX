-- | Complier (top-level) monad
module MiniLAX.Compiler (
    CompilerT (..),
    runC,
    throwC,
    catchC,
    throwIfFlag
) where

-- | Diagnostic messages
import MiniLAX.Diagnostic
import MiniLAX.Options
import MiniLAX.Util.Flag

import Data.Sequence

import Control.Monad (ap, when)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error


newtype CompilerT m a = CompilerT { 
    runCompilerT :: ErrorT String (FlagT (DiagT (ConfT m))) a
}


instance (Functor m) => Functor (CompilerT m) where
    fmap f = CompilerT . fmap f . runCompilerT

instance (Monad m) => Monad (CompilerT m) where
    return = CompilerT . return
    m >>= f = CompilerT $ runCompilerT m >>= (runCompilerT . f)
        

instance (Monad m) => MonadDiag (CompilerT m) where
    emit = CompilerT . lift . lift . emit
    
instance (MonadIO m) => MonadIO (CompilerT m) where
    liftIO = CompilerT . liftIO 

liftReader :: (Monad m) => ConfT m a -> CompilerT m a
liftReader = CompilerT . lift . lift . lift


instance (Monad m) => MonadConf (CompilerT m) where
    config = liftReader config
    nonopts = liftReader nonopts
    
instance (Monad m) => MonadFlag (CompilerT m) where
    setFlagState = CompilerT . lift . setFlagState
    checkFlag = CompilerT $ lift checkFlag


runC :: (Monad m) =>
    Options -> [String] -> CompilerT m a -> m (Either String a, Seq Message)
runC opts args c =
    let err = runCompilerT c
        fg  = runErrorT err
        wr  = return fst `ap` runFlagT fg
        rd  = runWriterT wr
    in runReaderT rd (opts, args)
    
  
throwC :: (Monad m) => String -> CompilerT m a
throwC = CompilerT . throwError

catchC :: (Monad m) => 
    CompilerT m a -> (String -> CompilerT m a) -> CompilerT m a
catchC v h = CompilerT $ v' `catchError` h'
    where v' = runCompilerT v
          h' = runCompilerT . h
          
throwIfFlag :: (Monad m) => String -> CompilerT m ()
throwIfFlag msg = checkFlag >>= flip when (throwC msg)

    
