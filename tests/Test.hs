{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} MiniLAX.Parsing.LexerTest

main :: IO ()
main = htfMain htf_importedTests
