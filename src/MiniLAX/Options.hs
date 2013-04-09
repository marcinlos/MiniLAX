-- | Module containing definitions and actions for command line options
module MiniLAX.Options where

-- | Imports
import System.Exit
import System.Console.GetOpt
import System.IO

import qualified Data.ByteString.Lazy as BS

-- | Version of a program
version :: String
version = "0.01"

        
-- | Single record to contain all the program options          
data Options = Options { 
    optInputName   :: String,
    optInput       :: IO String,
    optOutput      :: BS.ByteString -> IO (),
    optVerbose     :: Bool,
    optDumpTokens  :: Bool,
    optDumpAst     :: Bool,
    optDumpAstFlat :: Bool,
    optDumpJasmin  :: Bool
}

-- | Default value of the options
defaultOptions :: Options
defaultOptions = Options {
    optVerbose     = False,
    optInputName   = "-",
    optInput       = getContents,
    optOutput      = BS.putStr,
    optDumpTokens  = False,
    optDumpAst     = False,
    optDumpAstFlat = False,
    optDumpJasmin  = False
}


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
  where 
    changeInput path rest = do
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
        
    Option "j" ["dump-jasmin"]
        (NoArg $ \opts -> return opts { optDumpJasmin = True })
        "Outputs jasmin assembly generated from the input",

    Option "v" ["verbose"]
        (NoArg $ \opts -> return opts { optVerbose = True })
        "Enables additional output for debugging purposes",
        
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
