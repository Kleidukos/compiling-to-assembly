module Main (main) where

import System.IO
import Test.Tasty

import qualified ASMTest
import qualified ParserTest
import qualified TCTest

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMain $
    testGroup
      "Compiler Tests"
      [ ParserTest.specs
      , ASMTest.specs
      , TCTest.specs
      ]
