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
  
