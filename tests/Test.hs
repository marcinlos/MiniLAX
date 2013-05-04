{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

-- | 
import Test.Framework

import {-@ HTF_TESTS @-} MiniLAX.Parsing.LexerTest
import {-@ HTF_TESTS @-} MiniLAX.Static.ClosuresTest

main :: IO ()
main = htfMain htf_importedTests
