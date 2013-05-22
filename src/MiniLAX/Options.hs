{-# LANGUAGE FlexibleInstances #-}
-- | Module containing definitions and actions for command line options
module MiniLAX.Options where

-- | Imports
import System.Exit
import System.Console.GetOpt
import System.IO

import Control.Monad
import Control.Monad.Trans.Reader

import qualified Data.ByteString.Lazy as BS

-- | Version of a program
version :: String
version = "0.01"

-- | Auxilary function to parse input
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

        
-- | Single record to contain all the program options          
data Options = Options { 
    optInputName               :: String,
    optInput                   :: IO String,
    optOutput                  :: BS.ByteString -> IO (),
    optVerbosity               :: Int,
    optDumpTokens              :: Bool,
    optDumpAst                 :: Bool,
    optDumpAstFlat             :: Bool,
    optDumpSymbolTable         :: Bool,
    optDumpFreeVars            :: Bool,
    optDumpLambdaLifted        :: Bool,
    optDumpIR                  :: Bool,
    optDumpASM                 :: Bool
}

-- | Default value of the options
defaultOptions :: Options
defaultOptions = Options {
    optInputName               = "-",
    optInput                   = getContents,
    optOutput                  = BS.putStr,
    optVerbosity               = 3,
    optDumpTokens              = False,
    optDumpAst                 = False,
    optDumpAstFlat             = False,
    optDumpSymbolTable         = False,
    optDumpFreeVars            = False,
    optDumpLambdaLifted        = False,
    optDumpIR                  = False,
    optDumpASM                 = False
}


-- | Class of configuration-providing monad
class(Monad m) => MonadConf m where
    config :: m Options
    nonopts :: m [String]

    getOption :: (Options -> a) ->  m a
    getOption f = f `liftM` config
    

-- | Monad transformer providing configuration
type ConfT = ReaderT (Options, [String])

instance (Monad m) => MonadConf (ConfT m) where
    config = asks fst
    nonopts = asks snd
    
ifEnabled :: (Functor m, MonadConf m) => (Options -> Bool) -> m a -> m ()
ifEnabled opt action = do
    value <- getOption opt
    when value $ void action


-- | Function building options structure and undertaking necessary actions
--   based on command line arguments. Actions are defined by 'options' list.
parseOptions :: [String] -> IO (Options, [String])
parseOptions args = do
    let (actions, nonOpts, errs) = getOpt Permute options args
    case errs of 
        [] -> do opts <- foldl (>>=) (return defaultOptions) actions
                 parseNonopts nonOpts opts
        _  -> ioError $ userError (concat errs ++ usageInfo header options)
    where header = "Usage: mlax [OPTION...] file"
    
    
-- | Function interpreting non-option command line arguments, i.e. list of
--   input files. For now handling multiple input files is not necessary,
--   so in case of multiple input files the first one is considered and a 
--   warning is issued.
parseNonopts :: [String] -> Options -> IO (Options, [String])
parseNonopts args opts =
    case args of
        []     -> return (opts, args)
        [path] -> changeInput path []
        path : rest -> do
            hPutStrLn stderr $ "Warning: More than one input file, " ++ 
                "only " ++ path ++ " shall be processed"
            changeInput path rest
    where changeInput path rest = do
              let input = if path /= "-" 
                      then openFile path ReadMode >>= hGetContents
                      else getContents
              return (opts {
                  optInput = input, 
                  optInputName = path 
              }, rest)
     
     
-- | Actions corresponding to all the available options
options :: [OptDescr (Options -> IO Options)]
options = [
    Option "o" ["output"]
        (ReqArg
            (\file opts -> return opts { optOutput = BS.writeFile file })
            "FILE")
        "Output file",
        
    Option [] ["dump-tokens"]
        (NoArg $ \opts -> return opts { optDumpTokens = True })
        "Prints tokens of the input and terminates",
        
    Option [] ["dump-ast"]
        (OptArg (\val opts -> do
            opts' <- case val of 
                Just mode 
                    | mode == "flat" -> 
                          return opts { optDumpAstFlat = True }
                    | otherwise -> do
                          hPutStrLn stderr $ "Unknown mode `" ++ mode ++ "'"
                          return opts
                Nothing   -> return opts
            return opts' { optDumpAst = True }) 
            "MODE")
        "Prints original form of AST of the input and terminates",
        
    Option [] ["dump-symbols"]
        (NoArg $ \opts -> return opts { optDumpSymbolTable = True })
        "Outputs symbol table generated from the original source",
       
    Option [] ["dump-free-vars"]
        (NoArg $ \opts -> return opts { optDumpFreeVars = True })
        ("Outputs free variables for each procedure code (first iteration, " ++
            "does not include variables introduced by lifting sibling " ++
            "procedures)"),
        
    Option [] ["dump-lifted"]
        (NoArg $ \opts -> return opts { optDumpLambdaLifted = True })
        "Outputs lambda-lifted code",
        
    Option [] ["dump-ir"]
        (NoArg $ \opts -> return opts { optDumpIR = True })
        "Outputs intermediate code",
        
    Option [] ["dump-asm"]
        (NoArg $ \opts -> return opts { optDumpASM = True })
        "Outputs jasmin assembly generated from the input",

    Option "v" ["verbosity"]
        (ReqArg (\n opts -> 
            case readMaybe n of
                Just k -> return opts { optVerbosity = k}
                Nothing -> do
                    hPutStrLn stderr $ "Invalid verbosity level `" ++ n ++ "'"
                    exitSuccess)
            "N")
        "Controls level of verbosity - how chatty is the compiler, default is 3",
        
    Option "V" ["version"]
        (NoArg $ \_ -> do
            putStrLn versionString
            exitSuccess)
        "Prints version information and terminates",
        
    Option "h" ["help"]
        (NoArg $ \_ -> do
            putStrLn $ usageInfo "mlax" options
            exitSuccess)
        "Prints usage info and terminates"
    ]
    
versionString :: String
versionString = "MiniLAX compiler, version " ++ version
