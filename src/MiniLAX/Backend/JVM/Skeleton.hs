-- | Module containing functions generating the class structure 
module MiniLAX.Backend.JVM.Skeleton where

-- | Printer may come in handy
import MiniLAX.Printer

--import Control.Monad

-- TODO: Learn Template Haskell, for god's sake!

lab :: String -> PrinterMonad ()
lab s = append s %% ":" >> endl

iconst :: Int -> PrinterMonad ()
iconst n | n == -1               = put "iconst_m1" >> endl
         | n >= 0    && n <= 5   = put "iconst_" %% show n >> endl
         | n >= -128 && n <= 127 = put "bipush " %% show n >> endl
         | otherwise             = put "ldc " %% show n >> endl
         
aaload :: PrinterMonad ()
aaload = put "aaload" >> endl

aastore :: PrinterMonad ()
aastore = put "aastore" >> endl

aload :: Int -> PrinterMonad ()
aload n | n <= 3    = put "aload_" %% show n >> endl
        | otherwise = put "aload " %% show n >> endl
        
astore :: Int -> PrinterMonad ()
astore n | n <= 3    = put "astore_" %% show n >> endl
         | otherwise = put "astore " %% show n >> endl
         
baload :: PrinterMonad () 
baload = put "baload" >> endl

bastore :: PrinterMonad ()
bastore = put "bastore" >> endl

fadd :: PrinterMonad ()
fadd = put "fadd" >> endl

fsub :: PrinterMonad ()
fsub = put "fsub" >> endl

fmul :: PrinterMonad ()
fmul = put "fmul" >> endl

fdiv :: PrinterMonad ()
fdiv = put "fdiv" >> endl

fneg :: PrinterMonad ()
fneg = put "fneg" >> endl

faload :: PrinterMonad ()
faload = put "faload" >> endl

fastore :: PrinterMonad ()
fastore = put "fastore" >> endl

fcmpg :: PrinterMonad ()
fcmpg = put "fcmpg" >> endl

fcmpl :: PrinterMonad ()
fcmpl = put "fcmpl" >> endl

fconst :: Float -> PrinterMonad ()
fconst 0.0 = put "fconst_0" >> endl
fconst 1.0 = put "fconst_1" >> endl
fconst 2.0 = put "fconst_2" >> endl
fconst x   = put "ldc " %% show x >> endl

fload :: Int -> PrinterMonad ()
fload n | n <= 3    = put "fload_" %% show n >> endl
        | otherwise = put "fload " %% show n >> endl
        
fstore :: Int -> PrinterMonad ()
fstore n | n <= 3    = put "fstore_" %% show n >> endl
         | otherwise = put "fstore " %% show n >> endl
         
goto :: String -> PrinterMonad ()
goto s = put "goto " %% s >> endl 

i2f :: PrinterMonad ()
i2f = put "i2f" >> endl

f2i :: PrinterMonad ()
f2i = put "f2i" >> endl

iadd :: PrinterMonad ()
iadd = put "iadd" >> endl

isub :: PrinterMonad ()
isub = put "isub" >> endl

imul :: PrinterMonad ()
imul = put "imul" >> endl

ineg :: PrinterMonad ()
ineg = put "imul" >> endl

iload :: Int -> PrinterMonad ()
iload n | n <= 3    = put "iload_" %% show n >> endl
        | otherwise = put "iload " %% show n >> endl
        
istore :: Int -> PrinterMonad ()
istore n | n <= 3    = put "istore_" %% show n >> endl
         | otherwise = put "istore " %% show n >> endl
         
iflt :: String -> PrinterMonad ()
iflt dest = put "iflt " %% dest >> endl

ifge :: String -> PrinterMonad ()
ifge dest = put "ifge " %% dest >> endl

ifgt :: String -> PrinterMonad ()
ifgt dest = put "ifgt " %% dest >> endl

ifle :: String -> PrinterMonad ()
ifle dest = put "ifle " %% dest >> endl

new :: String -> PrinterMonad ()
new s = put "new " %% s >> endl

newarray :: String -> PrinterMonad ()
newarray s = put "newarray " %% s >> endl

multinewarray :: String -> Int -> PrinterMonad ()
multinewarray s n = put "multinewarray " %% s %% show n >> endl

getstatic :: String -> String -> PrinterMonad ()
getstatic f t = put "getstatic " %% f %% " " %% t >> endl

invokevirtual :: String -> PrinterMonad ()
invokevirtual m = put "invokevirtual " %% m >> endl

invokespecial :: String -> PrinterMonad () 
invokespecial m = put "invokespecial " %% m >> endl

returnJ :: PrinterMonad ()
returnJ = put "return" >> endl

dup :: PrinterMonad ()
dup = put "dup" >> endl

swap :: PrinterMonad ()
swap = put "swap" >> endl

-- | Special header
fileHeader :: PrinterMonad ()
fileHeader = do
    put "; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" >> endl;
    put ";         Generated with MiniLAX compiler" >> endl
    put "; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" >> endl;


-- | Prints jasmin directives used at the beginning of the file 
classHeader :: String -> PrinterMonad ()
classHeader name  = do
    put ".bytecode 49.0" >> endl
    put ".class public " %% name >> endl
    put ".super java/lang/Object" >> endl
    endl
    
-- | Prints default constructor
defaultConstructor :: PrinterMonad ()
defaultConstructor =
    method "<init>()V" $ do
        put "aload_0" >> endl
        invokespecial "java/lang/Object/<init>()V"
        put "return" >> endl
    
-- | Prints a method of given signature and bodyprzy
method :: String -> PrinterMonad a -> PrinterMonad ()
method sig body = do
    put ".method public " %% sig >> endl
    indentedBy 2 body
    put ".end method" >> endl
    endl
    
-- | Prints content inside main method
javaMain :: PrinterMonad a -> PrinterMonad ()
javaMain = method "static main([Ljava/lang/String;)V"
    
    
limitStack :: Int -> PrinterMonad ()
limitStack n = put ".limit stack " %% show n >> endl

limitLocals :: Int -> PrinterMonad ()
limitLocals n = put ".limit locals " %% show n >> endl


    
-- | Prepares string for source code output
quote :: String -> String
quote s = '"' : quote' s
    where quote' ('"' : cs) = '\\' : 'g' : quote' cs
          quote' ('\n' : cs) = '\\' : quote' cs 
          quote' (c : cs) = c : quote' cs
          quote' [] = "\""
    
-- | Prints instructions printing given string
syso :: String -> PrinterMonad ()
syso s = do
    getstatic "java/lang/System/out" "Ljava/io/PrintStream;"
    put "ldc " %% quote s >> endl
    invokevirtual "java/io/PrintStream/println(Ljava/lang/String;)V"
    
pushSysOut :: PrinterMonad ()
pushSysOut = getstatic "java/lang/System/out" "Ljava/io/PrintStream;"
 
printInt :: PrinterMonad ()
printInt = do
    pushSysOut
    swap
    invokevirtual "java/io/PrintStream/println(I)V"
    
printFloat :: PrinterMonad ()
printFloat = do
    pushSysOut
    swap
    invokevirtual "java/io/PrintStream/println(F)V"
    
-- | Simple "Hello World" program
example :: PrinterMonad ()
example = do
    fileHeader
    classHeader "hw"
    --defaultConstructor
    javaMain $ do
        limitStack 1000
        limitLocals 3
        syso "Hello World!"
        iconst 0
        istore 1
        iconst 1
        istore 2
        
        lab "loop"
        iload 2
        dup
        iload 1
        iadd
        istore 2
        dup
        printInt
        dup
        istore 1
        iconst 1000
        isub
        ifge "after"
        
        goto "loop"
        lab "after"        
        returnJ


        