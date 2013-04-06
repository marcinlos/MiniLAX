module Main (
    main, 
    Options(..) 
) where

-- |
import Control.Monad

import System.Environment
import MiniLAX.Options

import MiniLAX.Parsing.Lexer
import MiniLAX.Parsing.Parser
import MiniLAX.Parsing.Printer



main :: IO ()
main = do
    (opts, args) <- parseOptions =<< getArgs
    when (optVerbose opts) $ do
        putStrLn "Verbose mode ON"
        putStrLn "Input files: "
        forM_ args $ putStrLn . ('\t' :)
    content <- optInput opts
    let tokens = alexScanTokens content
    when (optTokenize opts) $ do
        putStrLn (showTokens tokens)
    --print $ parse tokens
        

    