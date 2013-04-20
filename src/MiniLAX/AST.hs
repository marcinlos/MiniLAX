-- | Module exporting AST definitions
module MiniLAX.AST (
    Program (..),
    Block (..),
    Decl (..),
    DeclSeq,
    ProcHead (..),
    ParamKind (..),
    Formal (..),
    FormalSeq,
    Type (..),
    Stat (..),
    StatSeq,
    Expr (..),
    ExprSeq,
    BoolLiteral (..),
    Var (..),
    BinOp (..),
    UnOp (..),
) where

-- | To avoid orphaned instances
import MiniLAX.Printer


-- | Enumeration of binary operators
data BinOp = Plus | Times | Less
    deriving (Eq, Show)

-- | Enumeration of unary operators
data UnOp = Not
    deriving (Eq, Show)

-- | Represents var/val nature of procedure parameters    
data ParamKind = VarParam | ValParam
    deriving (Eq, Show)

-- | Whole program - name and ordinary block, ending with a dot
data Program = Program {
    progName :: String,
    progBody :: Block
} deriving (Eq, Show)

-- | Code block - declarations and statements
data Block = Block {
    blockDecls :: DeclSeq,
    blockStats :: StatSeq
} deriving (Eq, Show)

type DeclSeq = [Decl]

-- | Formal parameters of a function
data Formal = Formal {
    formalName :: String,
    formalType :: Type,
    formalKind :: ParamKind
} deriving (Eq, Show)

type FormalSeq = [Formal]

-- | Declaration of a variable/procedure
data Decl = 
    VarDecl {
        varName :: String,
        varType :: Type
    }
  | ProcDecl {
        procHead :: ProcHead,
        procBody :: Block
    } 
  deriving (Eq, Show)
  
-- | Procedure signature - name and parameter definitions
data ProcHead = ProcHead {
    procName   :: String,
    procParams :: FormalSeq
} deriving (Eq, Show)

type ExprSeq = [Expr]

-- | Expression, many kinds of
data Expr = 
    BinaryExpr {
        binOp    :: BinOp,
        binLeft  :: Expr,
        binRight :: Expr
    }
  | UnaryExpr {
        unOp   :: UnOp,
        unExpr :: Expr
    }
  | IntConst Int
  | RealConst Float
  | VarExpr Var
  | BoolConst BoolLiteral
  deriving (Eq, Show)
  
-- | Boolean literal value
data BoolLiteral = TrueL | FalseL
    deriving (Eq, Show)
  
-- | a[i1][i2]...[in] expression
data Var = VarId String 
  | VarIndex {
        varBase  :: Var,
        varIndex ::  Expr
    }
  deriving (Eq, Show)
  
type StatSeq = [Stat]

-- | Statement
data Stat = 
    AssignStat {
        assignLeft  :: Var,
        assignRight :: Expr
    }
  | ProcStat {
        callName :: String,
        callArgs :: ExprSeq
    }
  | CondStat {
        condCond  :: Expr,
        condTrue  :: StatSeq,
        condFalse :: StatSeq
    }
  | LoopStat {
        loopCond :: Expr,
        loopBody :: StatSeq
    }
  deriving (Eq, Show)

-- | MiniLAX data type
data Type = IntegerT | RealT | BooleanT 
  | ArrayT { 
        arrayElemType :: Type, 
        arrayLower    :: Int, 
        arrayUpper    :: Int 
    } 
  deriving (Eq, Show)
  
-- ----------------------------------------------
-- Pretty-printing
-- ----------------------------------------------  
  
instance Printable Program where
    prettyPrint (Program name body) =
        put "Program '" %% name %% "' " >> bracketed (prettyPrint body)
        
instance Printable Block where
    prettyPrint (Block decls stats) = do
        put "Decls " >> bracketed (mapM_ prettyPrint $ reverse decls)
        put "Stats " >> bracketed (mapM_ prettyPrint $ reverse stats)
        
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
    
instance Printable ParamKind where
    prettyPrint VarParam = put "Var" >> endl
    prettyPrint ValParam = put "Val" >> endl
    
instance Printable ProcHead where
    prettyPrint (ProcHead name params) = do
        put "Name '" %% name %% "'" >> endl
        put "Params " >> bracketed (mapM_ prettyPrint $ reverse params)
        
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
            put "Args " >> bracketed (mapM_ prettyPrint $ reverse args)
        
    prettyPrint (CondStat cond true false) = do
        put "If "; bracketed $ do
            put "Cond " >> bracketed (prettyPrint cond)
            put "True "  >> bracketed (mapM_ prettyPrint $ reverse true)
            put "False " >> bracketed (mapM_ prettyPrint $ reverse false)
            
    prettyPrint (LoopStat cond body) = do
        put "While "; bracketed $ do
            put "Cond " >> bracketed (prettyPrint cond)
            put "Body " >> bracketed (mapM_ prettyPrint $ reverse body)
        
instance Printable Var where
    prettyPrint (VarId name) = 
        put "VarName: '" %% name %% "'" >> endl
        
    prettyPrint (VarIndex base index) = do
        put "Variable "; bracketed $ do
            put "Base "  >> bracketed (prettyPrint base)
            put "Index " >> bracketed (prettyPrint index)
        
instance Printable Expr where
    prettyPrint (BinaryExpr op left right) = do
        put "Binary "; bracketed $ do
            put "Op: " %% show op >> endl
            put "Left "  >> bracketed (prettyPrint left)
            put "Right " >> bracketed (prettyPrint right)
            
    prettyPrint (UnaryExpr op expr) = do
        put "Unary "; bracketed $ do
            put "Op: " %% show op >> endl
            put "Expr " >> bracketed (prettyPrint expr)
            
    prettyPrint (IntConst n) =
        put "IntConst " %% show n >> endl
        
    prettyPrint (RealConst x) = 
        put "RealConst " %% show x >> endl
        
    prettyPrint (BoolConst b) =
        put "BoolConst " %% show b >> endl
        
    prettyPrint (VarExpr var) = 
        put "VarExpr " >> bracketed (prettyPrint var)
  
