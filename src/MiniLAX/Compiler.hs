-- | Complier (top-level) monad
module MiniLAX.Compiler (
    Compiler (..),
    getOpt,
    getNonopts,
    config,
    runC,
    throwC
) where

-- | Diagnostic messages
import MiniLAX.Diagnostic
import MiniLAX.Options

import Control.Applicative

import Data.Sequence

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error


newtype Compiler a = Compiler { 
    runCompiler :: ErrorT String (DiagT (ReaderT (Options, [String]) IO)) a
}

instance Functor Compiler where
    fmap f = Compiler . fmap f . runCompiler

instance Monad Compiler where
    return = Compiler . return
    m >>= f = Compiler $ runCompiler m >>= (runCompiler . f)

instance MonadDiag Compiler where
    emit = Compiler . lift . emit
    
instance MonadIO Compiler where
    liftIO = Compiler . liftIO 

liftReader :: ReaderT (Options, [String]) IO a -> Compiler a
liftReader = Compiler . lift . lift

getOpt :: (Options -> a) -> Compiler a
getOpt f = liftReader $ asks (f . fst)

getNonopts :: Compiler [String]
getNonopts = liftReader $ asks snd

config :: Compiler Options
config = liftReader $ asks fst

    
runC :: Options -> [String] -> Compiler a -> IO (Either String a, Seq Message)
runC opts args c =
    let err = runCompiler c
        wr  = runErrorT err
        rd  = runWriterT wr
    in runReaderT rd (opts, args)
    
  
throwC :: String -> Compiler a
throwC = Compiler . throwError

