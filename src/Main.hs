-- | Entry point of an application
module Main (
    main, 
    Options(..) 
) where

-- |
import Prelude hiding (catch, mapM)
import System.Environment
import System.IO
import System.Exit

import Data.Traversable as Trav (forM)

import Control.Applicative

import Control.Exception
import Control.Monad
import Control.Monad.Trans

import MiniLAX.Compiler
import MiniLAX.Options

import MiniLAX.Parsing.Lexer
import MiniLAX.Parsing.Parser
import MiniLAX.Parsing.TokenPrinter

import MiniLAX.Printer
import MiniLAX.AST

import MiniLAX.Static.Symbols

import MiniLAX.Backend.JVM.Skeleton


main :: IO ()
main = run' `catch` errorHandler

{-
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
        
-}

        
run' :: IO ()
run' = do
    (opts, args) <- parseOptions =<< getArgs
    (res, diag) <- runC opts args $ do
        greeting
        content <- liftIO $ optInput opts
        let tokens = alexScanTokens content
        maybeDumpTokens tokens
        ast <- parse tokens
        maybeDumpAST ast

    void $ Trav.forM diag print
    case res of 
        Right _ -> putStrLn "Success"
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ err
            exitFailure
            

errorHandler :: IOError -> IO ()
errorHandler e = do
    hPutStrLn stderr $ "Error: " ++ show e
    exitFailure
    
    
greeting :: Compiler ()
greeting = do
    verbose <- optVerbose <$> config
    args    <- getNonopts
    liftIO $ when verbose $ do
        putStrLn "Verbose mode ON"
        putStrLn "Input file(s): "
        forM_ args $ putStrLn . ('\t' :)
    
    
maybeDumpTokens :: [Token] -> Compiler ()
maybeDumpTokens tokens = do
    shouldDump <- getOpt optDumpTokens
    when shouldDump $
        liftIO $ putStrLn $ showTokens tokens


maybeDumpAST :: Program -> Compiler ()
maybeDumpAST ast = do
    shouldDump <- getOpt optDumpAst
    when shouldDump $ do
        flat <- getOpt optDumpAstFlat
        let s = if flat then show 
                        else getString . prettyPrint 
        liftIO $ putStrLn (s ast)

    