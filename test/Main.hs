module Main (main) where

import System.IO
import Test.Tasty

import qualified ASMTest
import qualified ParserTest

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMain $
    testGroup
      "Compiler Tests"
      [ ParserTest.specs
      , ASMTest.specs
      ]
