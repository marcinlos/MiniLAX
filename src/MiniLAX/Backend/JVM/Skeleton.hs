-- | Module containing functions generating the class structure 
module MiniLAX.Backend.JVM.Skeleton where

-- | Printer may come in handy
import MiniLAX.Printer

--import Control.Monad

-- | Prints jasmin directives used at the beginning of the file 
classHeader :: String -> PrinterMonad ()
classHeader name  = do
    put ".bytecode 49.0" >> endl
    put ".class public " %% name >> endl
    put ".super java/lang/Object" >> endl
    
-- | Prints default constructor
defaultConstructor :: PrinterMonad ()
defaultConstructor = do
    put ".method public <init>()V" >> endl
    indented $ do
        put "aload_0" >> endl
        put "invokespecial java/lang/Object/<init>()V" >> endl
        put "return" >> endl
    put ".end method" >> endl
    
-- | Prints content inside main method
javaMain :: PrinterMonad a -> PrinterMonad ()
javaMain m = do
    put ".method public static main([Ljava/lang/String;)V" >> endl
    indented m
    put ".end method" >> endl
    
limitStack :: Int -> PrinterMonad ()
limitStack n = put ".limit stack " %% show n >> endl
    
-- | Prepares string for source code output
quote :: String -> String
quote s = '"' : quote' s
    where quote' ('"' : cs) = '\\' : '"' : quote' cs
          quote' ('\n' : cs) = '\\' : quote' cs 
          quote' (c : cs) = c : quote' cs
          quote' [] = "\""
    
-- | Prints instructions printing given string
syso :: String -> PrinterMonad ()
syso s = do
    put "getstatic java/lang/System/out Ljava/io/PrintStream;" >> endl
    put "ldc " %% quote s >> endl
    put "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V" >> endl
    
-- | Simple "Hello World" program
example :: PrinterMonad ()
example = do
    classHeader "hw" >> endl
    defaultConstructor >> endl
    javaMain $ do
        limitStack 2
        syso "Hello World!"
        syso "Mutherfucker"
        put "return" >> endl
