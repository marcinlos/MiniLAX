{-# LANGUAGE ExistentialQuantification #-}

-- | Module containing functions used to pretty-print AST
module MiniLAX.AST.Printer where

-- | To print locations of AST elements
import MiniLAX.Location

import MiniLAX.AST
import Control.Applicative
import Data.Monoid


data ASTVal = forall a. (ASTElement a) => ASTVal { unwrap :: a }

class ASTElement a where
    children :: a -> [ASTVal]
    
    
instance ASTElement Type where
    children (ArrayT e _ _) = ASTVal <$> [e]
    children _ = []
    
instance ASTElement BinOp where
    children = const []
    
instance ASTElement UnOp where
    children = const []
    
    
instance ASTElement Expr where
    children (BinaryExpr op left right) = ASTVal op : (ASTVal <$> [left, right])
    children (UnaryExpr op expr) = [ASTVal op, ASTVal expr]
    children _ = []
     
instance ASTElement Var where
    children (VarIndex base index) = [ASTVal base, ASTVal index]
    children _ = []
    
    
instance ASTElement Stat where
    children (AssignStat left right) = [ASTVal left, ASTVal right]
    children (ProcStat _ args) = ASTVal <$> args
    children (CondStat cond t f) = ASTVal cond : (ASTVal <$> t) ++ (ASTVal <$> f)
    children (LoopStat cond body) = ASTVal cond : (ASTVal <$> body)
    
instance ASTElement Decl where
    children (ProcDecl _ body) = [ASTVal body]
    children _ = []
    
instance ASTElement Block where
    children (Block decls stats) = (ASTVal <$> decls) ++ (ASTVal <$> stats)
    
instance ASTElement Program where
    children (Program _ body) = [ASTVal body]
    
data PrintState = PrintState {
    getContent :: String -> String,
    getIndent  :: Int
}



instance Monoid PrintState where
    mempty = PrintState {
        getContent = id,
        getIndent = 0
    }
    mappend (PrintState c _) (PrintState c' i') =
        PrintState {
            getContent = c . c',
            getIndent = i'
        }


newtype PrinterMonad a = PrinterMonad { 
    runPrinter :: PrintState -> (a, PrintState) 
}

getString :: PrinterMonad a -> String
getString m = getContent s ""
    where (_, s) = runPrinter m mempty

instance Monad PrinterMonad where
    return a = PrinterMonad $ \s -> (a, s)
    m >>= f = PrinterMonad {
        runPrinter = \s ->
            let (v, s') = runPrinter m s
                s'' = s `mappend` s'
            in runPrinter (f v) s''
    }
    
getIndentLvl :: PrinterMonad Int
getIndentLvl = PrinterMonad $ \s ->
    let n = getIndent s 
    in (n, s)

append :: ShowS -> PrinterMonad ()
append str = PrinterMonad $ \s @ PrintState { getContent = old } ->
    let s' = s { getContent =  old . str }
    in ((), s')
    
put :: String -> PrinterMonad ()
put s = append (s++)

endl :: PrinterMonad ()
--endl = put "\n" >> getIndentLvl >>= put . flip replicate ' ' . (*3)
endl = do
    put "\n"
    n <- getIndentLvl
    put $ replicate (n * 3) ' '


infixl 5 %%

(%%) :: PrinterMonad () -> String -> PrinterMonad ()
m %% s = m >> put s

indent :: Int -> PrinterMonad ()
indent k = PrinterMonad $ \s @ PrintState { getIndent = n } ->
    let s' = s { getIndent = n + k } 
    in ((), s')

shRight, shLeft :: PrinterMonad ()
shRight = indent 1
shLeft  = indent (-1)

indented :: PrinterMonad a -> PrinterMonad ()
indented s = shRight >> s >> shLeft

class Printable a where
    prettyPrint :: a -> PrinterMonad ()
    
ind :: Int -> ShowS
ind = (++) . flip replicate ' ' . (*3)
    
instance Printable Program where
    prettyPrint (Program name body) = do
        --put "Program '"
        --put name 
        --put "' {"
        --put "1" %% "2" %% "3" %% "4" %% "5" %% "6" %% "7"
        put "1"
        put "2"
        put "3"
        --put "4"
        --put "5"
        --put "6"
        --put "}" %% "Twoja matka to chuj"
        {-ind n . ("Program '" ++) . (name ++) . ("' {\n" ++)
        . prettyPrint (n + 1) body
        . ind n . ("}\n" ++)-}
        
        
instance Printable Block where
    prettyPrint (Block decls stats) = do
        put "Decls {\n"
        put "}\n"
        put "Stats {\n"
        put "}\n"
    
    
