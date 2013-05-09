-- | Pretty-printing of program AST
module MiniLAX.AST.PrettyPrint (
    pretty,
    Pretty (..)
) where

-- |
import Control.Applicative ((<$>))
import Data.List
import MiniLAX.AST.Annotated 
import MiniLAX.Printer
import qualified MiniLAX.Static.Types as T


pretty :: Program a -> String
pretty = const "?" --getString . out


class Pretty a where
    out :: a -> PrinterMonad ()

instance Pretty T.Type where
    out (T.ArrayT tp low high) = do
        append "ARRAY [" %% show low %% ".." %% show high %% "] OF "
        out tp
    out other = prettyPrint other


instance Pretty (Name a) where
    out (Name _ name) = append name


{-   Maybe later     
instance Pretty (Program a) where
    out (Program _ name body) = do
        put "PROGRAM " >> out name %% ";" >> endl >> endl
        out body %% "."
        
instance Pretty (Block a) where
    out (Block _ decls stats) = do
        put "DECLARE" >> endl
        indented $ mapM_ out $ reverse decls
        put "BEGIN" >> endl
        indented $ mapM_ out $ reverse stats
        put "END"
instance Pretty (Decl a) where
    out (VarDecl _ name tp) = 
        out name >> out tp >> endl

    out (ProcDecl _ head body) =
        out head >> endl
        
instance Pretty (ProcHead a) where
    out (ProcHead _ name params) = do
        ind >> out name %% "(" >> params' %% ");" >> endl
        where params' = put "(params)"
-}  
    
instance Pretty (Stmt a) where
    out (Assignment _ left right) = 
        ind >> out left %% " := " >> out right >> endl
        
    out (ProcCall _ name args) = do
        ind >> out name %% "("
        let args' =  out <$> args
        sequence_ (intersperse (append ", ") args')
        append ")" >> endl
        
    out (IfThenElse _ cond true false) = do
        put "IF " >> out cond %% " THEN" >> endl
        indented $ mapM_ out  true
        put "ELSE" >> endl
        indented $ mapM_ out false
        put "END" >> endl
            
    out (While _ cond body) = do
        put "WHILE " >> out cond %% " DO" >> endl
        indented $ mapM_ out body
        put "END" >> endl
        
instance Pretty (Expr a) where
    out (BinaryExpr _ op left right) = 
        out left %% " " >> out op %% " " >> out right
    out (UnaryExpr _ op e) = out op %% " " >> out e
    out (LitExpr _ lit) = out lit
    out (VarExpr _ var) = out var
    out (CastExpr _ t e) = put "(" >> out t %% ": " >> out e %% ") "
    
instance Pretty (Variable a) where
    out (VarName _ name) = out name
    out (VarIndex _ name expr) = out name %% "[" >> out expr %% "]"
    
instance Pretty (BinOp a) where
    out (Plus _) = append "+"
    out (Times _) = append "-"
    out (Less _) = append "<"
    
instance Pretty (UnOp a) where
    out (Not _) = append "NOT"
    
instance Pretty (Literal a) where
    out (LitInt _ n) = append $ show n
    out (LitReal _ x) = append $ show x
    out (LitMichal _) = append "<michal>"
    out (LitTrue _) = append "TRUE"
    out (LitFalse _) = append "FALSE"
    
instance Pretty (Type a) where
    out _ = append "(type)"


