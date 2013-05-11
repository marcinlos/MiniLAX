{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Annotated syntax tree
module MiniLAX.AST.Annotated where

--
import Prelude hiding (concat)
import Control.Applicative (pure, (<$>), (<*>))
import Data.Foldable
import Data.Traversable
import Data.Monoid

-- | Parameter kind (by reference vs by value) is not defined as an annotated
-- ast element, since "by value" part lacks any textual representation.
import MiniLAX.Static.Types (ParamKind)


-- | Class of a annotated entity
class Annotated a l | a -> l where
    attr :: a -> l
    
    
-- | Class of an entity with a name, such as a program, variable etc.
class HasName a where
    getName :: a -> String 


fold2 :: (Foldable f, Foldable t, Monoid m) => (a -> m) -> f (t a) -> m
fold2 f = foldMap (foldMap f)

-- | Simple name (label)
data Name l = Name l String  

instance Annotated (Name l) l where 
    attr (Name l _) = l 

instance Functor Name where
    fmap f (Name l s) = Name (f l) s 

instance Foldable Name where
    foldMap f = f . attr

instance Traversable Name where
    traverse f (Name l s) = Name <$> f l <*> pure s
    
deriving instance (Show l) => Show (Name l) 
    
-- | Whole program
data Program l = Program l (Name l) (Block l)

instance Annotated (Program l) l where
    attr (Program l _ _) = l
    
instance Functor Program where
    fmap f (Program l name block) = 
        Program (f l) (f <$> name) (f <$> block)
        
instance Foldable Program where
    foldMap f (Program l _ b) = f l <> foldMap f b
    
instance Traversable Program where
    traverse f (Program l n b) = Program <$> f l <*> n' <*> b'
        where n' = traverse f n
              b' = traverse f b
    
deriving instance (Show l) => Show (Program l) 

-- |
data Block l = Block l [Decl l] [Stmt l]

instance Annotated (Block l) l where
    attr (Block l _ _) = l
    
instance Functor Block where
    fmap f (Block l decls stms) = 
        Block (f l) (map (fmap f) decls) (map (fmap f) stms) 
        
instance Foldable Block where
    foldMap f (Block l decls stmts) = f l <> decls' <> stmts'
        where decls' = fold $ foldMap f <$> decls
              stmts' = fold $ foldMap f <$> stmts
              
instance Traversable Block where
    traverse f (Block l decls stmts) = Block <$> f l <*> decls' <*> stmts'
        where decls' = traverse (traverse f) decls
              stmts' = traverse (traverse f) stmts
    
deriving instance (Show l) => Show (Block l) 

-- |
data Decl l = 
    VarDecl l (Name l) (Type l)
  | ProcDecl l (ProcHead l) (Block l)

instance Annotated (Decl l) l where 
    attr (VarDecl l _ _) = l
    attr (ProcDecl l _ _) = l
    
instance Functor Decl where
    fmap f (VarDecl l name tp) = VarDecl (f l) (f <$> name) (f <$> tp) 
    fmap f (ProcDecl l hd body) = ProcDecl (f l) (f <$> hd) (f <$> body)
    
instance Foldable Decl where
    foldMap f (VarDecl l name tp) = f l <> foldMap f name <> foldMap f tp
    foldMap f (ProcDecl l hd body) = f l <> foldMap f hd <> foldMap f body
    
instance Traversable Decl where
    traverse f (VarDecl l name tp) = VarDecl <$> f l <*> name' <*> tp'
        where name' = traverse f name
              tp'   = traverse f tp
    traverse f (ProcDecl l hd body) = ProcDecl <$> f l <*> hd' <*> body'
        where hd'   = traverse f hd
              body' = traverse f body          
    
deriving instance (Show l) => Show (Decl l) 
  
-- |
data ProcHead l = ProcHead l (Name l) [Formal l]

instance Annotated (ProcHead l) l where
    attr (ProcHead l _ _) = l
    
instance Functor ProcHead where
    fmap f (ProcHead l name formals) = 
        ProcHead (f l) (f <$> name) formals'
        where formals' = map (f <$>) formals
        
instance Foldable ProcHead where
    foldMap f (ProcHead l name formals) = 
        f l <> foldMap f name <> fold formals'
        where formals' = foldMap f <$> formals

instance Traversable ProcHead where
    traverse f (ProcHead l name formals) =
        ProcHead <$> f l <*> name' <*> formals'
        where name'    = traverse f name
              formals' = traverse (traverse f) formals
    
deriving instance (Show l) => Show (ProcHead l) 

-- |
data Formal l = Formal l ParamKind (Name l) (Type l)

instance Annotated (Formal l) l where
    attr (Formal l _ _ _) = l
    
instance Functor Formal where
    fmap f (Formal l k name tp) = Formal (f l) k (f <$> name) (f <$> tp)
    
instance Foldable Formal where
    foldMap f (Formal l _ name tp) = f l <> foldMap f name <> foldMap f tp 
    
instance Traversable Formal where
    traverse f (Formal l k name tp) = 
        Formal <$> f l <*> pure k <*> name' <*> tp'
        where name' = traverse f name
              tp'   = traverse f tp
    
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
    fmap f (Assignment l x y) = Assignment (f l) (f <$> x) (f <$> y) 
    
    fmap f (ProcCall l name ps)  = 
        ProcCall (f l) (f <$> name) ps'
        where ps' = map (f <$>) ps
         
    fmap f (IfThenElse l cond ifT ifF) = 
        IfThenElse (f l) (f <$> cond) (f' ifT) (f' ifF)
        where f' = map (f <$>)
         
    fmap f (While l cond body) = While (f l) (f <$> cond) (f' body)
        where f' = map (f <$>)
        
instance Foldable Stmt where
    foldMap f (Assignment l x y) = f l <> foldMap f x <> foldMap f y
    foldMap f (ProcCall l name ps) = f l <> foldMap f name <> fold2 f ps
    foldMap f (IfThenElse l c ifT ifF) = 
        f l <> foldMap f c <> fold2 f ifT <> fold2 f ifF
    foldMap f (While l c body) = f l <> foldMap f c <> fold2 f body
    
instance Traversable Stmt where
    traverse f (Assignment l x y) = Assignment <$> f l <*> x' <*> y'
        where x' = traverse f x
              y' = traverse f y
    traverse f (ProcCall l name ps) = ProcCall <$> f l <*> name' <*> ps'
        where name' = traverse f name
              ps'   = traverse (traverse f) ps
    traverse f (IfThenElse l c ifT ifF) = 
        IfThenElse <$> f l <*> c' <*> ifT' <*> ifF'
        where c'   = traverse f c
              ifT' = traverse (traverse f) ifT
              ifF' = traverse (traverse f) ifF 
    traverse f (While l c body) = While <$> f l <*> c' <*> body'    
        where c'    = traverse f c
              body' = traverse (traverse f) body
  
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
    
instance Foldable BinOp where
    foldMap = (. attr)
    
instance Traversable BinOp where
    traverse f (Plus l) = Plus <$> f l
    traverse f (Times l) = Times <$> f l
    traverse f (Less l) = Less <$> f l
    
deriving instance (Show l) => Show (BinOp l) 

-- |
data UnOp l = Not l

instance Annotated (UnOp l) l where
    attr (Not l) = l
    
instance Functor UnOp where
    fmap f (Not l) = Not (f l)
    
instance Foldable UnOp where
    foldMap = (. attr)
    
instance Traversable UnOp where
    traverse f (Not l) = Not <$> f l
    
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
        BinaryExpr (f l) (f <$> op) (f <$> el) (f <$> er)
        
    fmap f (UnaryExpr l op e) = UnaryExpr (f l) (f <$> op) (f <$> e)
    fmap f (LitExpr l val) = LitExpr (f l) (f <$> val)
    fmap f (VarExpr l var) = VarExpr (f l) (f <$> var)
    fmap f (CastExpr l t e) = CastExpr (f l) (f <$> t) (f <$> e)
    
instance Foldable Expr where
    foldMap f (BinaryExpr l op el er) =
        f l <> foldMap f op <> foldMap f el <> foldMap f er
    foldMap f (UnaryExpr l op e) = f l <> foldMap f op <> foldMap f e
    foldMap f (LitExpr l val) = f l <> foldMap f val
    foldMap f (VarExpr l var) = f l <> foldMap f var
    foldMap f (CastExpr l t e) = f l <> foldMap f t <> foldMap f e
    
instance Traversable Expr where
    traverse f (BinaryExpr l op left right) = 
        BinaryExpr <$> f l <*> op' <*> left' <*> right'
        where op'    = traverse f op
              left'  = traverse f left
              right' = traverse f right
    traverse f (UnaryExpr l op e) = UnaryExpr <$> f l <*> op' <*> e'
        where op' = traverse f op
              e'  = traverse f e
    traverse f (LitExpr l val) = LitExpr <$> f l <*> traverse f val
    traverse f (VarExpr l var) = VarExpr <$> f l <*> traverse f var
    traverse f (CastExpr l t e) = CastExpr <$> f l <*> t' <*> e'
        where t' = traverse f t
              e' = traverse f e
     

deriving instance (Show l) => Show (Expr l) 
    
-- |  
data Variable l = 
    VarName l (Name l)
  | VarIndex l (Variable l) (Expr l)
  
instance Annotated (Variable l) l where
    attr (VarName l _) = l
    attr (VarIndex l _ _) = l
    
instance Functor Variable where
    fmap f (VarName l name) = VarName (f l) (f <$> name)
    fmap f (VarIndex l var idx) = VarIndex (f l) (f <$> var) (f <$> idx) 
    
instance Foldable Variable where
    foldMap f (VarName l name) = f l <> foldMap f name
    foldMap f (VarIndex l var idx) = f l <> foldMap f var <> foldMap f idx
    
instance Traversable Variable where
    traverse f (VarName l name) = VarName <$> f l <*> traverse f name
    traverse f (VarIndex l var idx) = VarIndex <$> f l <*> var' <*> idx'
        where var' = traverse f var
              idx' = traverse f idx 
    
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
        TyArray (f l) (f <$> tp) (f <$> low) (f <$> high)
        
instance Foldable Type where
    foldMap f (TyArray l tp low high) = 
        f l <> foldMap f tp <> foldMap f low <> foldMap f high
    foldMap f t = f $ attr t
  
  
instance Traversable Type where
    traverse f (TyInt l) = TyInt <$> f l
    traverse f (TyReal l) = TyReal <$> f l
    traverse f (TyBoolean l) = TyBoolean <$> f l
    traverse f (TyArray l tp low high) = 
        TyArray <$> f l <*> tp' <*> low' <*> high'
        where tp'   = traverse f tp
              low'  = traverse f low
              high' = traverse f high  
    
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
    
instance Foldable Literal where
    foldMap = (. attr)
    
instance Traversable Literal where
    traverse f (LitInt l n)  = LitInt <$> f l <*> pure n
    traverse f (LitReal l x) = LitReal <$> f l <*> pure x 
    traverse f (LitMichal l) = LitMichal <$> f l    
    traverse f (LitTrue l)   = LitTrue <$> f l      
    traverse f (LitFalse l)  = LitFalse <$> f l     
    
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
    
    