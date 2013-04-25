-- | Pretty-printing of program AST
module MiniLAX.AST.PrettyPrint (
    pretty,
    Pretty (..)
) where

-- |
import MiniLAX.AST
import MiniLAX.Printer


pretty :: Program -> String
pretty = const "?" --getString . out


class Pretty a where
    out :: a -> PrinterMonad ()

{-
instance Pretty Program where
    out (Program name body) = do
        put "PROGRAM " %% name %% ";" >> endl >> endl
        out body %% "."
        
instance Pretty Block where
    out (Block decls stats) = do
        put "DECLARE" >> endl
        indented $ mapM_ out $ reverse decls
        put "BEGIN" >> endl
        indented $ mapM_ out $ reverse stats
        put "END"
        
instance Pretty Decl where
    out (VarDecl name tp) = 
        put name >> out tp >> endl
        
    out (ProcDecl head body) = do
        out head >> endl
        
instance Pretty ProcHead where
    out (ProcHead name params) = do
        put name %% "(" >> params >> ");" >> endl
        where params = put "(params)"
    
instance Pretty Stat where
    out (AssignStat left right) = 
        out left %% " := " >> out right >> endl
        
    out (ProcStat name args) = do
        put name >> if null args then put ""
                                 else put " "
        endl
        --put "Call "; bracketed $ do
        --    put "Name: '" %% name %% "'" >> endl
        --    put "Args " >> bracketed (mapM_ prettyPrint $ reverse args)
        
    out (CondStat cond true false) = do
        put "IF " >> out cond %% " THEN" >> endl
        indented $ mapM_ out $ reverse true
        put "ELSE" >> endl
        indented $ mapM_ out $ reverse false
        put "END" >> endl
            
    out (LoopStat cond body) = do
        put "WHILE " >> out cond %% " DO" >> endl
        indented $ mapM_ out $ reverse body
        put "END" >> endl
        
instance Pretty Expr where
    out _ = put "(expr)"
    
instance Pretty Var where
    out _ = put "(var)"
    
instance Pretty Type where
    out _ = put "(type)"
-}

