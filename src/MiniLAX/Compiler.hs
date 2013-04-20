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
    runCompiler :: ErrorT String (ReaderT (Options, [String]) (DiagT IO)) a
}

instance Functor Compiler where
    fmap f = Compiler . fmap f . runCompiler

instance Monad Compiler where
    return = Compiler . return
    m >>= f = Compiler $ runCompiler m >>= (runCompiler . f)

instance MonadDiag Compiler where
    emit = Compiler . lift . lift . emit
    
instance MonadIO Compiler where
    liftIO = Compiler . liftIO 

getOpt :: (Options -> a) -> Compiler a
getOpt f = Compiler $ f . fst <$> lift ask

getNonopts :: Compiler [String]
getNonopts = Compiler $ snd <$> lift ask

config :: Compiler Options
config = Compiler $ fst <$> lift ask

    
runC :: Options -> [String] -> Compiler a -> IO (Either String a, Seq Message)
runC opts args c =
    let err = runCompiler c
        rd  = runErrorT err
        dg  = runReaderT rd (opts, args)
    in runWriterT dg
  
throwC :: String -> Compiler a
throwC = Compiler . throwError

