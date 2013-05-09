-- | Complier (top-level) monad
module MiniLAX.Compiler (
    CompilerT (..),
    runC,
    throwC
) where

-- | Diagnostic messages
import MiniLAX.Diagnostic
import MiniLAX.Options

import Data.Sequence

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error


newtype CompilerT m a = CompilerT { 
    runCompiler :: ErrorT String (DiagT (ConfT m)) a
}


instance (Functor m) => Functor (CompilerT m) where
    fmap f = CompilerT . fmap f . runCompiler

instance (Monad m) => Monad (CompilerT m) where
    return = CompilerT . return
    m >>= f = CompilerT $ runCompiler m >>= (runCompiler . f)

instance (Monad m) => MonadDiag (CompilerT m) where
    emit = CompilerT . lift . emit
    
instance (MonadIO m) => MonadIO (CompilerT m) where
    liftIO = CompilerT . liftIO 

liftReader :: (Monad m) => ConfT m a -> CompilerT m a
liftReader = CompilerT . lift . lift


instance (Monad m) => MonadConf (CompilerT m) where
    config = liftReader config
    nonopts = liftReader nonopts


runC :: Options -> [String] -> CompilerT m a -> m (Either String a, Seq Message)
runC opts args c =
    let err = runCompiler c
        wr  = runErrorT err
        rd  = runWriterT wr
    in runReaderT rd (opts, args)
    
  
throwC :: (Monad m) => String -> CompilerT m a
throwC = CompilerT . throwError

