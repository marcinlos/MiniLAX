{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MiniLAX.Parsing.LexerTest (
    htf_MiniLAX_Parsing_LexerTest_thisModulesTests
) where
import MiniLAX.Parsing.Lexer

import Test.Framework
import qualified Test.HUnit.Lang (Assertion)

test_MultilineComments_Empty :: Test.HUnit.Lang.Assertion
test_MultilineComments_Empty = 
    assertEmpty $ alexScanTokens text
    where text = "(* comme\nnt\n\n  \nfdf\nd*)"
