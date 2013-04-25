-- | Complier (top-level) monad
module MiniLAX.Compiler (
    CompilerT (..),
    getOpt,
    getNonopts,
    config,
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
    runCompiler :: ErrorT String (DiagT (ReaderT (Options, [String]) m)) a
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

liftReader :: (Monad m) => ReaderT (Options, [String]) m a -> CompilerT m a
liftReader = CompilerT . lift . lift

getOpt :: (Monad m) => (Options -> a) -> CompilerT m a
getOpt f = liftReader $ asks (f . fst)

getNonopts :: (Monad m) => CompilerT m [String]
getNonopts = liftReader $ asks snd

config :: (Monad m) => CompilerT m Options
config = liftReader $ asks fst

    
runC :: Options -> [String] -> CompilerT m a -> m (Either String a, Seq Message)
runC opts args c =
    let err = runCompiler c
        wr  = runErrorT err
        rd  = runWriterT wr
    in runReaderT rd (opts, args)
    
  
throwC :: (Monad m) => String -> CompilerT m a
throwC = CompilerT . throwError

