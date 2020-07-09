module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)

import qualified Core.Ch01Test as Ch01
import qualified Core.Ch02Test as Ch02

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Ch01.tests
  , Ch02.tests
  ]
