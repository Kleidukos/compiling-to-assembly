module Main (main) where

import System.IO
import Test.Tasty

import qualified CodeGen.ARM32Test as ARM32Test
import qualified ParserTest
import qualified TCTest

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMain $
    testGroup
      "Compiler Tests"
      [ ParserTest.specs
      , ARM32Test.specs
      , TCTest.specs
      ]
