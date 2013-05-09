{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Annotated syntax tree
module MiniLAX.AST.Annotated where

-- | Parameter kind (by reference vs by value) is not defined as an annotated
-- ast element, since "by value" part lacks any textual representation.
import MiniLAX.Static.Types (ParamKind)


-- | Class of a annotated entity
class Annotated a l | a -> l where
    attr :: a -> l
    
-- | Class of an entity with a name, such as a program, variable etc.
class HasName a where
    getName :: a -> String 


-- | Simple name (label)
data Name l = Name l String  

instance Annotated (Name l) l where 
    attr (Name l _) = l 

instance Functor Name where
    fmap f (Name l s) = Name (f l) s 

    
deriving instance (Show l) => Show (Name l) 
    
-- | Whole program
data Program l = Program l (Name l) (Block l)

instance Annotated (Program l) l where
    attr (Program l _ _) = l
    
instance Functor Program where
    fmap f (Program l name block) = 
        Program (f l) (f `fmap` name) (f `fmap` block)
    
deriving instance (Show l) => Show (Program l) 

-- |
data Block l = Block l [Decl l] [Stmt l]

instance Annotated (Block l) l where
    attr (Block l _ _) = l
    
instance Functor Block where
    fmap f (Block l decls stms) = 
        Block (f l) (map (fmap f) decls) (map (fmap f) stms) 
    
deriving instance (Show l) => Show (Block l) 

-- |
data Decl l = 
    VarDecl l (Name l) (Type l)
  | ProcDecl l (ProcHead l) (Block l)

instance Annotated (Decl l) l where 
    attr (VarDecl l _ _) = l
    attr (ProcDecl l _ _) = l
    
instance Functor Decl where
    fmap f (VarDecl l name tp) = VarDecl (f l) (f `fmap` name) (f `fmap` tp) 
    fmap f (ProcDecl l hd body) = ProcDecl (f l) (f `fmap` hd) (f `fmap` body)
    
deriving instance (Show l) => Show (Decl l) 
  
-- |
data ProcHead l = ProcHead l (Name l) [Formal l]

instance Annotated (ProcHead l) l where
    attr (ProcHead l _ _) = l
    
instance Functor ProcHead where
    fmap f (ProcHead l name formals) = 
        ProcHead (f l) (f `fmap` name) formals'
        where formals' = map (f `fmap`) formals
    
deriving instance (Show l) => Show (ProcHead l) 

-- |
data Formal l = Formal l ParamKind (Name l) (Type l)

instance Annotated (Formal l) l where
    attr (Formal l _ _ _) = l
    
instance Functor Formal where
    fmap f (Formal l k name tp) = Formal (f l) k (f `fmap` name) (f `fmap` tp)
    
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
    
instance Functor Stmt where
    fmap f (Assignment l x y) = Assignment (f l) (f `fmap` x) (f `fmap` y) 
    
    fmap f (ProcCall l name ps)  = 
        ProcCall (f l) (f `fmap` name) ps'
        where ps' = map (f `fmap`) ps
         
    fmap f (IfThenElse l cond ifT ifF) = 
        IfThenElse (f l) (f `fmap` cond) (f' ifT) (f' ifF)
        where f' = map (f `fmap`)
         
    fmap f (While l cond body) = While (f l) (f `fmap` cond) (f' body)
        where f' = map (f `fmap`)
  
deriving instance (Show l) => Show (Stmt l) 
  
-- |
data BinOp l = Plus l | Times l | Less l

instance Annotated (BinOp l) l where
    attr (Plus l) = l
    attr (Times l) = l
    attr (Less l) = l
    
instance Functor BinOp where
    fmap f (Plus l)  = Plus (f l)
    fmap f (Times l) = Times (f l)
    fmap f (Less l)  = Less (f l)
    
deriving instance (Show l) => Show (BinOp l) 

-- |
data UnOp l = Not l

instance Annotated (UnOp l) l where
    attr (Not l) = l
    
instance Functor UnOp where
    fmap f (Not l) = Not (f l)
    
deriving instance (Show l) => Show (UnOp l) 
  
-- |  
data Expr l =
    BinaryExpr l (BinOp l) (Expr l) (Expr l)
  | UnaryExpr l (UnOp l) (Expr l)
  | LitExpr l (Literal l)
  | VarExpr l (Variable l)
  | CastExpr l (Type l) (Expr l)
    
instance Annotated (Expr l) l where
    attr (BinaryExpr l _ _ _) = l
    attr (UnaryExpr l _ _) = l
    attr (LitExpr l _) = l
    attr (VarExpr l _) = l
    attr (CastExpr l _ _) = l
    
instance Functor Expr where
    fmap f (BinaryExpr l op el er) = 
        BinaryExpr (f l) (f `fmap` op) (f `fmap` el) (f `fmap` er)
        
    fmap f (UnaryExpr l op e) = UnaryExpr (f l) (f `fmap` op) (f `fmap` e)
    fmap f (LitExpr l val) = LitExpr (f l) (f `fmap` val)
    fmap f (VarExpr l var) = VarExpr (f l) (f `fmap` var)
    fmap f (CastExpr l t e) = CastExpr (f l) (f `fmap` t) (f `fmap` e)
     

deriving instance (Show l) => Show (Expr l) 
    
-- |  
data Variable l = 
    VarName l (Name l)
  | VarIndex l (Variable l) (Expr l)
  
instance Annotated (Variable l) l where
    attr (VarName l _) = l
    attr (VarIndex l _ _) = l
    
instance Functor Variable where
    fmap f (VarName l name) = VarName (f l) (f `fmap` name)
    fmap f (VarIndex l var idx) = VarIndex (f l) (f `fmap` var) (f `fmap` idx) 
    
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
    
instance Functor Type where
    fmap f (TyInt l) = TyInt (f l)
    fmap f (TyReal l) = TyReal (f l)
    fmap f (TyBoolean l) = TyBoolean (f l)
    fmap f (TyArray l tp low high) = 
        TyArray (f l) (f `fmap` tp) (f `fmap` low) (f `fmap` high)
    
deriving instance (Show l) => Show (Type l)     

-- |  
data Literal l =
    LitInt l Int
  | LitReal l Float
  | LitMichal l
  | LitTrue l
  | LitFalse l
  
instance Annotated (Literal l) l where
    attr (LitInt l _)  = l
    attr (LitReal l _) = l
    attr (LitMichal l) = l
    attr (LitTrue l)   = l
    attr (LitFalse l)  = l 
    
instance Functor Literal where
    fmap f (LitInt l n)  = LitInt (f l) n
    fmap f (LitReal l x) = LitReal (f l) x
    fmap f (LitMichal l) = LitMichal (f l) 
    fmap f (LitTrue l)   = LitTrue (f l)
    fmap f (LitFalse l)  = LitFalse (f l) 
    
deriving instance (Show l) => Show (Literal l)     
    
    
-- | HasName instances for named AST entities
    
instance HasName (Name l) where
    getName (Name _ n) = n
    
instance HasName (Program l) where
    getName (Program _ name _) = getName name 
    
    
instance HasName (Decl l) where
    getName (VarDecl _ name _) = getName name
    getName (ProcDecl _ hd _) = getName hd
    
instance HasName (ProcHead l) where
    getName (ProcHead _ name _) = getName name
    
instance HasName (Formal l) where
    getName (Formal _ _ name _) = getName name
    
      
instance HasName (Variable l) where
    getName (VarName _ name) = getName name
    getName (VarIndex _ var _) = getName var
    
    