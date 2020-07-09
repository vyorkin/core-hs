module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified Core.Ch01Test as Ch01
import qualified Core.Ch02Test as Ch02

main :: IO ()
main = do
  ch02 <- Ch02.tests
  defaultMain $ testGroup "Tests"
    [ ch02
    ]
