-- | Module exporting AST definitions
module MiniLAX.AST (
    Program (..),
    Block (..),
    Decl (..),
    DeclSeq,
    ProcHead (..),
    ParamType (..),
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
data ParamType = VarParam | ValParam
    deriving (Eq, Show)

data Program = Program {
    progName :: String,
    progBody :: Block
} deriving (Eq, Show)

data Block = Block {
    blockDecls :: DeclSeq,
    blockStats :: StatSeq
} deriving (Eq, Show)

type DeclSeq = [Decl]

data Formal = Formal {
    formalName :: String,
    formalType :: Type,
    formalKind :: ParamType
} deriving (Eq, Show)

type FormalSeq = [Formal]

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
  
data ProcHead = ProcHead {
    procName   :: String,
    procParams :: FormalSeq
} deriving (Eq, Show)

type ExprSeq = [Expr]

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
  
data BoolLiteral = TrueL | FalseL
    deriving (Eq, Show)
  
data Var = VarId String 
  | VarIndex {
        varBase  :: Var,
        varIndex ::  Expr
    }
  deriving (Eq, Show)
  
type StatSeq = [Stat]

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


data Type = IntegerT | RealT | BooleanT 
  | ArrayT { 
        arrayElemType :: Type, 
        arrayLower    :: Int, 
        arrayUpper    :: Int 
    } 
  deriving (Eq, Show)
  
