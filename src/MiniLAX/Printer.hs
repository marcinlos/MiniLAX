-- | Module containing definition of pretty printing monad automatically
--   handling indentation.
module MiniLAX.Printer (
    Printable (..),
    PrinterMonad (..),
    getString,
    getIndentLvl, 
    append,
    put,
    endl,
    ind,
    (%%),
    indent, 
    indentedBy,
    indented,
    shLeftBy,
    shRightBy,
    shLeft,
    shRight,
    bracketed
) where

import Data.Monoid

class Printable a where
    prettyPrint :: a -> PrinterMonad ()

   
data PrintState = PrintState {
    getContent :: String -> String,
    getIndent  :: Int
}


instance Monoid PrintState where
    mempty = PrintState {
        getContent = id,
        getIndent = 0
    }
    mappend (PrintState c _) (PrintState c' i') = PrintState {
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
    return a = PrinterMonad $ \s -> (a, s { getContent = id })
    m >>= f = PrinterMonad {
        runPrinter = \s ->
            let (v, s') = runPrinter m s
                (a, s'') = runPrinter (f v) s'
            in (a, s' `mappend` s'')
    }
    
instance Functor PrinterMonad where
    fmap = (=<<) . (return .)
    
getIndentLvl :: PrinterMonad Int
getIndentLvl = PrinterMonad $ \s ->
    let n = getIndent s 
    in (n, s { getContent = id })

append :: String -> PrinterMonad ()
append str = PrinterMonad $ \s -> ((), s { getContent = (str ++) })
    
put :: String -> PrinterMonad ()
put s = ind >> append s

endl :: PrinterMonad ()
endl = append "\n"
    
ind :: PrinterMonad ()
ind = getIndentLvl >>= append . flip replicate ' ' . (*2)


infixl 5 %%

(%%) :: PrinterMonad () -> String -> PrinterMonad ()
m %% s = m >> append s

indent :: Int -> PrinterMonad ()
indent k = PrinterMonad $ \s @ PrintState { getIndent = n } ->
    let s' = s { getIndent = n + k, getContent = id } 
    in ((), s')
    
shRightBy, shLeftBy :: Int -> PrinterMonad ()
shRightBy = indent
shLeftBy = indent . negate

shRight, shLeft :: PrinterMonad ()
shRight = shRightBy 1
shLeft  = shLeftBy 1

indented :: PrinterMonad a -> PrinterMonad ()
indented = indentedBy 1

indentedBy :: Int -> PrinterMonad a -> PrinterMonad ()
indentedBy n s = shRightBy n >> s >> shLeftBy n  

bracketed :: PrinterMonad a -> PrinterMonad ()
bracketed s = append "{" >> endl >> indented s >> put "}" >> endl
    

    