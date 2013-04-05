module MiniLAX.AST (
    BinOp (..),
    UnOp (..),
    Expr (..),
    Var (..),
    StatSeq,
    Stat (..),
    Type (..),
    Block (..),
    Formal (..),
    Decl (..),
    ParamType (..),
) where

-- |
import MiniLAX.Location

data BinOp = Plus | Times | Less

data UnOp = Not

data ParamType = VarParam | ValParam

data Block = Block {
    blockDecls :: DeclSeq,
    blockStats :: StatSeq
}

type DeclSeq = [Decl]

data Formal = Formal {
    formalName :: String,
    formalType :: Type,
    formalKind :: ParamType
}

type FormalSeq = [Formal]

data Decl = 
    VarDecl {
        varName :: String,
        varType :: Type
    }
  | ProcDecl {
        procName   :: String,
        procParams :: FormalSeq
  }

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
  
data Var = VarId String 
  | VarIndex {
        varBase  :: Var,
        varIndex ::  Expr
    }
  
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


data Type = IntegerT | RealT | BooleanT 
  | ArrayT { 
        arrayElemType :: Type, 
        arrayLower    :: Int, 
        arrayUpper    :: Int 
    } 
  
