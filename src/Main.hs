module Main (
    main 
  , Options(..) 
) where

import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad

import MiniLAX.Parsing.Lexer as L

import qualified Data.ByteString.Lazy as BS

-- | Version of a program
version :: String
version = "1.0"


main :: IO ()
main = do
    (opts, args) <- parseOptions =<< getArgs
    when (optVerbose opts) $ do
        putStrLn "Verbose mode ON"
        putStrLn "Input files: "
        forM_ args $ putStrLn . ('\t' :)
    content <- optInput opts
    let tokens = alexScanTokens content    
    print tokens    
        
-- | Single record to contain all the program options          
data Options = Options {
    optInput   :: IO String,
    optOutput  :: BS.ByteString -> IO (),
    optVerbose :: Bool
}

-- | Default value of the options
defaultOptions :: Options
defaultOptions = Options {
    optVerbose = False,
    optInput   = getContents,
    optOutput  = BS.putStr
}
        

-- | Function building options structure and undertaking necessary actions
--   based on command line arguments. Actions are defined by 'options' list.
parseOptions :: [String] -> IO (Options, [String])
parseOptions args = do
    let (actions, nonOpts, errs) = getOpt Permute options args
    case errs of 
        [] -> do opts <- foldl (>>=) (return defaultOptions) actions
                 return (opts, nonOpts)
        _  -> ioError $ userError (concat errs ++ usageInfo header options)
    where header = "Usage: mlax [OPTION...] file"
     
     
-- | Actions corresponding to all the available options
options :: [OptDescr (Options -> IO Options)]
options = [
    Option "o" ["output"]
        (ReqArg
            (\file opts -> return opts { optOutput = BS.writeFile file })
            "FILE")
        "Output file",

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
    