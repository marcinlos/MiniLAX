{-# LANGUAGE ExistentialQuantification #-}
    
-- | Module containing functions used to pretty-print AST
module MiniLAX.AST.Printer where
    
-- | To print locations of AST elements
import MiniLAX.Location

import MiniLAX.Printer

import MiniLAX.AST
import Control.Applicative
import Control.Monad


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


    
instance Printable Program where
    prettyPrint (Program name body) = do
        put "Program '" %% name %% "' " >> bracketed (prettyPrint body)
        
instance Printable Block where
    prettyPrint (Block decls stats) = do
        put "Decls " >> bracketed (mapM_ prettyPrint decls)
        put "Stats " >> bracketed (mapM_ prettyPrint stats)
        
instance Printable Decl where
    prettyPrint (VarDecl name type_) = do
        put "Var "; bracketed $ do
            put "Name: '" %% name %% "'" >> endl
            put "Type " >> bracketed (prettyPrint type_)
    
    prettyPrint (ProcDecl info body) = do
        put "Proc "; bracketed $ do
            prettyPrint info
            prettyPrint body
        
instance Printable Type where
    prettyPrint (ArrayT el low high) = do
        put "Array "; bracketed $ do
            put "Lower bound: " %% show low >> endl
            put "Upper bound: " %% show high >> endl
            put "Element type " >> bracketed (prettyPrint el) 

    prettyPrint IntegerT = put "INTEGER" >> endl
    prettyPrint RealT    = put "REAL" >> endl
    prettyPrint BooleanT = put "BOOLEAN" >> endl
    
instance Printable BinOp where
    prettyPrint = put . show
    
instance Printable UnOp where
    prettyPrint = put . show
    
instance Printable ParamType where
    prettyPrint VarParam = put "Var" >> endl
    prettyPrint ValParam = put "Val" >> endl
    
instance Printable ProcHead where
    prettyPrint (ProcHead name params) = do
        put "Name '" %% name %% "'" >> endl
        put "Params " >> bracketed (mapM_ prettyPrint params)
        
instance Printable Formal where
    prettyPrint (Formal name type_ kind) = do
        put "Formal "; bracketed $ do
            put "Name: '" %% name %% "'" >> endl
            put "Kind: " %% show kind >> endl  
            put "Type " >> bracketed (prettyPrint type_)
            
instance Printable Stat where
    prettyPrint (AssignStat left right) = do
        put "Assignment "; bracketed $ do
            put "Left "  >> bracketed (prettyPrint left)
            put "Right " >> bracketed (prettyPrint right)
        
    prettyPrint (ProcStat name args) = do
        put "Call "; bracketed $ do
            put "Name: '" %% name %% "'" >> endl
            put "Args " >> bracketed (mapM_ prettyPrint args)
        
    prettyPrint (CondStat cond true false) = do
        put "If "; bracketed $ do
            put "True "  >> bracketed (mapM_ prettyPrint true)
            put "False " >> bracketed (mapM_ prettyPrint false)
            
    prettyPrint (LoopStat cond body) = do
        put "While "; bracketed $ do
            put "Cond " >> bracketed (prettyPrint cond)
            put "Body " >> bracketed (mapM_ prettyPrint body)
        
instance Printable Var where
    prettyPrint (VarId name) = 
        put "VarName: '" %% name %% "'" >> endl
        
    prettyPrint (VarIndex base index) = do
        put "Variable "; bracketed $ do
            put "Base "  >> bracketed (prettyPrint base)
            put "Index " >> bracketed (prettyPrint index)
        
instance Printable Expr where
    prettyPrint = const (put "?" >> endl)
        
        
    

    
 
