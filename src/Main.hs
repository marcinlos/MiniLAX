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
import MiniLAX.Parsing.LexerCore ()
import MiniLAX.Parsing.Parser
import MiniLAX.Printer
import MiniLAX.AST

import MiniLAX.Static.Symbols

import MiniLAX.Backend.JVM.Skeleton ()


main :: IO ()
main = run `catch` errorHandler

run :: IO ()
run = do
    (opts, args) <- parseOptions =<< getArgs
    (res, diag) <- runC opts args $ do
        greeting
        input  <- liftIO $ optInput opts
        tokens <- tokenize input
        maybeDumpTokens tokens
        ast <- parse tokens
        maybeDumpAST ast
        sym <- collectSymbols ast
        maybeDumpSymbols sym
    void $ Trav.forM diag print
    case res of 
        Right _ -> return ()
        Left err -> do
            hPutStrLn stderr err
            exitFailure

errorHandler :: IOError -> IO ()
errorHandler e = do
    hPutStrLn stderr $ "Error: " ++ show e
    exitFailure
    
    
greeting :: Compiler ()
greeting = do
    verbosity <- optVerbosity <$> config
    args      <- getNonopts
    liftIO $ when (verbosity > 3) $ do
        putStrLn "Verbose mode ON"
        putStrLn "Input file(s): "
        forM_ args $ putStrLn . ('\t' :)
        
        
tokenize :: String -> Compiler [Token]
tokenize s = either throwC return (scanTokens s)
    
    
maybeDumpTokens :: [Token] -> Compiler ()
maybeDumpTokens tokens = do
    shouldDump <- getOpt optDumpTokens
    when shouldDump $
        liftIO $ mapM_ print tokens
        

maybeDumpAST :: Program -> Compiler ()
maybeDumpAST ast = do
    shouldDump <- getOpt optDumpAst
    when shouldDump $ do
        flat <- getOpt optDumpAstFlat
        let s = if flat then show 
                        else getString . prettyPrint 
        liftIO $ putStrLn (s ast)
        
        
maybeDumpSymbols :: Procedure -> Compiler ()
maybeDumpSymbols syms = do
    shouldDump <- getOpt optDumpSymbolTable
    when shouldDump $
        let s = printProc "" syms
        in liftIO $ putStrLn $ getString s
    