{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Annotated syntax tree
module MiniLAX.AST.Annotated where

-- |
import MiniLAX.Static.Types (ParamKind)

-- | 
class Annotated a l | a -> l where
    attr :: a -> l

-- |
data Name l = Name l String  

instance Annotated (Name l) l where 
    attr (Name l _) = l 
    
deriving instance (Show l) => Show (Name l) 
    
-- |
data Program l = Program l (Name l) (Block l)

instance Annotated (Program l) l where
    attr (Program l _ _) = l
    
deriving instance (Show l) => Show (Program l) 

-- |
data ProgramName l = ProgramName l String

instance Annotated (ProgramName l) l where
    attr (ProgramName l _) = l
    
deriving instance (Show l) => Show (ProgramName l) 

-- |
data Block l = Block l [Decl l] [Stmt l]

instance Annotated (Block l) l where
    attr (Block l _ _) = l
    
deriving instance (Show l) => Show (Block l) 

-- |
data Decl l = 
    VarDecl l (Name l) (Type l)
  | ProcDecl l (ProcHead l) (Block l)
  
  
instance Annotated (Decl l) l where 
    attr (VarDecl l _ _) = l
    attr (ProcDecl l _ _) = l
    
deriving instance (Show l) => Show (Decl l) 
  
-- |
data ProcHead l = ProcHead l (Name l) [Formal l]

instance Annotated (ProcHead l) l where
    attr (ProcHead l _ _) = l
    
deriving instance (Show l) => Show (ProcHead l) 

-- |
data Formal l = Formal l ParamKind (Name l) (Type l)

instance Annotated (Formal l) l where
    attr (Formal l _ _ _) = l
    
deriving instance (Show l) => Show (Formal l) 

-- |
data Stmt l =
    Assignment l (Variable l) (Expr l)
  | ProcCall l (Name l) [Expr l]
  | IfThenElse l (Expr l) [Stmt l] [Stmt l]
  | While l (Expr l) [Stmt l]
  
instance Annotated (Stmt l) l where
    attr (Assignment l _ _) = l
    attr (ProcCall l _ _) = l
    attr (IfThenElse l _ _ _) = l
    attr (While l _ _) = l
  
deriving instance (Show l) => Show (Stmt l) 
  
-- |
data BinOp l = Plus l | Times l | Less l

instance Annotated (BinOp l) l where
    attr (Plus l) = l
    attr (Times l) = l
    attr (Less l) = l
    
deriving instance (Show l) => Show (BinOp l) 

-- |
data UnOp l = Not l

instance Annotated (UnOp l) l where
    attr (Not l) = l
    
deriving instance (Show l) => Show (UnOp l) 
  
-- |  
data Expr l =
    BinaryExpr l (BinOp l) (Expr l) (Expr l)
  | UnaryExpr l (UnOp l) (Expr l)
  | LitExpr l (Literal l)
  | VarExpr l (Variable l)
    
instance Annotated (Expr l) l where
    attr (BinaryExpr l _ _ _) = l
    attr (UnaryExpr l _ _) = l
    attr (LitExpr l _) = l
    attr (VarExpr l _) = l
    
deriving instance (Show l) => Show (Expr l) 
    
-- |  
data Variable l = 
    VarName l (Name l)
  | VarIndex l (Variable l) (Expr l)
  
instance Annotated (Variable l) l where
    attr (VarName l _) = l
    attr (VarIndex l _ _) = l
    
deriving instance (Show l) => Show (Variable l)     
  
-- |  
data Type l = 
    TyInt l 
  | TyReal l 
  | TyBoolean l
  | TyArray l (Type l) (Literal l) (Literal l)
  
instance Annotated (Type l) l where
    attr (TyInt l) = l
    attr (TyReal l) = l
    attr (TyBoolean l) = l
    attr (TyArray l _ _ _) = l
    
deriving instance (Show l) => Show (Type l)     

-- |  
data Literal l =
    LitInt l Int
  | LitReal l Float
  | LitMichal l
  | LitTrue l
  | LitFalse l
  
instance Annotated (Literal l) l where
    attr (LitInt l _) = l
    attr (LitReal l _) = l
    attr (LitMichal l) = l
    attr (LitTrue l) = l
    attr (LitFalse l) = l 
    
deriving instance (Show l) => Show (Literal l)     
    