{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MiniLAX.Static.ClosuresTest (
    htf_MiniLAX_Static_ClosuresTest_thisModulesTests
) where

-- |
import Test.Framework
import qualified Test.HUnit.Lang (Assertion)

import qualified Data.Map as M

import MiniLAX.AST.Annotated
import MiniLAX.Static.Closures as C



-- Expressions for testing

infixl 8 @@
infixl 7 .*.
infixl 6 .+.
infixl 3 .:=.

var_ :: String -> Variable ()
var_ s = VarName () (Name () s)

toE :: Variable () -> Expr ()
toE = VarExpr ()

var :: String -> Expr ()
var = toE . var_ 

int :: Int -> Expr ()
int = LitExpr () . LitInt ()

real :: Float -> Expr ()
real = LitExpr () . LitReal ()

(@@) :: Variable () -> Expr () -> Variable ()
(@@) = VarIndex ()

(.*.) :: Expr () -> Expr () -> Expr ()
(.*.) = BinaryExpr () (Times ())

(.+.) :: Expr () -> Expr () -> Expr ()
(.+.) = BinaryExpr () (Plus ())

(.:=.) :: Variable () -> Expr () -> Stmt ()
(.:=.) = Assignment ()


testExpr :: [(String, C.Usage)] -> Expr () -> Test.HUnit.Lang.Assertion
testExpr vars expr = assertEqual (M.fromList vars) res
    where Vars res = usedVars expr
    
testStmt :: [(String, C.Usage)] -> Stmt () -> Test.HUnit.Lang.Assertion
testStmt vars stmt = assertEqual (M.fromList vars) res
    where Vars res = usedVarsStmt stmt

rd, wr :: String -> (String, Usage)
rd = flip (,) ReadU
wr = flip (,) WriteU

--test_isIndexed_Var = assertBool (isIndexed 

test_usedVars_Literal :: Test.HUnit.Lang.Assertion
test_usedVars_Literal = testExpr [] (int 7)
          
test_usedVars_SimpleVar :: Test.HUnit.Lang.Assertion
test_usedVars_SimpleVar = testExpr [rd "x"] (var "x")

test_usedVars_Binary = testExpr [rd "x", rd "y"] expr
    where expr = var "x" .*. (int 3 .+. var "y") .+. var"x" 
    
test_usedVars_Complex = testExpr [rd "x", rd "n", rd "z"] expr
    where expr = toE (var_ "x" @@ (int 3 .+. var "n" .*. int 2) @@ var "z")
    
test_usedVars_Assignment = testStmt [wr "x"] stmt
    where stmt = var_ "x" .:=. int 3
    
test_usedVars_ArrayAssignment = testStmt [rd "x"] stmt
    where stmt = var_ "x" @@ int 7 .:=. int 3
    
test_usedVars_ComplexArrayAssignment = testStmt [rd "x", rd "y"] stmt
    where stmt = var_ "x" @@ int 7 @@ (int 3 .*. var "y") .:=. int 3
    
          
