module Main (
    main
  , Options(..)
) where

import System.Environment
import System.Console.CmdArgs


main :: IO ()
main = do
    args <- getArgs
    return ()
    
    
    
data Options = Options {
    optVerbose :: Bool
}
    