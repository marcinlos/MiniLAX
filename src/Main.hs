{-# LANGUAGE RankNTypes #-}
-- | Entry point of an application
module Main (
    main, 
    Options(..) 
) where

-- |
import Prelude hiding (mapM)
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
import MiniLAX.Location

import MiniLAX.Parsing.Lexer
import MiniLAX.Parsing.LexerCore ()
import MiniLAX.Printer
import MiniLAX.AST.Annotated
--import MiniLAX.AST.PrettyPrint

import MiniLAX.Parsing.Parser2

import MiniLAX.Static.Symbols
import MiniLAX.Static

import MiniLAX.Backend.JVM.Skeleton ()


main :: IO ()
main = run `catch` errorHandler

type Compiler = CompilerT IO

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
        sym' <- analyze sym
        return ()
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
    args      <- nonopts
    liftIO $ when (verbosity > 3) $ do
        putStrLn "Verbose mode ON"
        putStrLn "Input file(s): "
        forM_ args $ putStrLn . ('\t' :)
        
        
tokenize :: String -> Compiler [Token]
tokenize s = either throwC return (scanTokens s)
    
    
maybeDumpTokens :: [Token] -> Compiler ()
maybeDumpTokens tokens =
    ifEnabled optDumpTokens $
        liftIO $ mapM_ print tokens
        

maybeDumpAST :: (Show a) => Program a -> Compiler ()
maybeDumpAST ast =
    ifEnabled optDumpAst $ do
        -- flat <- getOpt optDumpAstFlat
        let s = show {- if flat then show 
                        else getString . prettyPrint -} 
        liftIO $ putStrLn (s ast)
        
        
maybeDumpSymbols :: Procedure -> Compiler ()
maybeDumpSymbols syms =
    ifEnabled optDumpSymbolTable $ do
        let s = printProc "" syms
        liftIO $ putStrLn $ getString s
    