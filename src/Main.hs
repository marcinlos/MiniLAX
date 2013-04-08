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

import MiniLAX.Printer
import MiniLAX.AST.Printer



main :: IO ()
main = do
    (opts, args) <- parseOptions =<< getArgs
    when (optVerbose opts) $ do
        putStrLn "Verbose mode ON"
        putStrLn "Input file(s): "
        forM_ args $ putStrLn . ('\t' :)
    content <- optInput opts
    let tokens = alexScanTokens content
    when (optDumpTokens opts) $
        putStrLn (showTokens tokens)
    case parse tokens of
        Right ast -> 
            when (optDumpAst opts) $ do
                putStrLn (show ast) 
                putStrLn . getString . prettyPrint $ ast
        Left err ->
            putStrLn err
        
