module MiniLAX.AST.Util where

-- |
import MiniLAX.Parsing.LexerCore
import MiniLAX.AST.Annotated
import MiniLAX.Location


mkName :: Token -> Name Location
mkName Token { tkPos = pos, tkVal = Id val } = Name pos val
mkName _ = error "Cannot make name"

mkLit :: Token -> Literal Location
mkLit Token { tkVal = Int n, tkPos = pos } = LitInt pos n
mkLit Token { tkVal = Float v, tkPos = pos } = LitReal pos v
mkLit Token { tkVal = Keyword kw, tkPos = pos } = ctor kw pos
    where ctor "TRUE" = LitTrue
          ctor "FALSE" = LitFalse
          ctor _ = error "Invalid constant"
mkLit Token { tkVal = Sym "}:->", tkPos = pos } = LitMichal pos
mkLit t = error $ "Cannot make literal " ++ show t 

mkLitExpr :: Token -> Expr Location
mkLitExpr t = let lit = mkLit t
              in LitExpr (attr lit) lit
              
mkBin :: Expr Location -> Token -> Expr Location -> Expr Location
mkBin e Token { tkVal = Sym s, tkPos = pos } e' = 
    BinaryExpr (attr e) (op s pos) e e'
    where op "+" = Plus
          op "*" = Times
          op "<" = Less
          op _   = error "Invalid operator"
mkBin _ _ _ = error "Invalid operator"
 
