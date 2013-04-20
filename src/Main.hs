module Main (
    main, 
    Options(..) 
) where

-- |
import Prelude hiding (catch)
import Control.Monad
import Control.Exception
import System.Environment
import System.IO
import System.Exit

import MiniLAX.Options

import MiniLAX.Parsing.Lexer
import MiniLAX.Parsing.Parser
import MiniLAX.Parsing.TokenPrinter

import MiniLAX.Printer
import MiniLAX.AST

import MiniLAX.Static.Symbols

import MiniLAX.Backend.JVM.Skeleton

main :: IO ()
main = run `catch` errorHandler

run :: IO ()
run = do
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
        Right ast -> do
            maybeDumpAST opts ast
            putStrLn . getString . printProc "" . collectSymbols $ ast
        Left err ->
            putStrLn err
    when (optDumpJasmin opts) $
        putStrLn . getString $ example
            
errorHandler :: IOError -> IO ()
errorHandler e = do
    hPutStrLn stderr $ "Error: " ++ show e
    exitFailure
    

maybeDumpAST :: Options -> Program -> IO ()
maybeDumpAST opts ast = 
    when (optDumpAst opts) $ putStrLn (s ast)
    where s = if optDumpAstFlat opts
                  then show 
                  else getString . prettyPrint

    