module MiniLAX.AST (
    Program (..),
    Block (..),
    Decl (..),
    DeclSeq,
    ParamType (..),
    Formal (..),
    FormalSeq,
    Type (..),
    Stat (..),
    StatSeq,
    Expr (..),
    ExprSeq,
    Var (..),
    BinOp (..),
    UnOp (..),
) where

-- |
import MiniLAX.Location

data BinOp = Plus | Times | Less
    deriving (Eq, Show)

data UnOp = Not
    deriving (Eq, Show)
    
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
        procName   :: String,
        procParams :: FormalSeq,
        procBody   :: Block
    } 
  deriving (Eq, Show)

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
  | Id String
  | VarExpr Var
  | TrueL
  | FalseL
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
  