module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Core.Ch01Test as Ch01
import qualified Core.Ch02Test as Ch02

main :: IO ()
main = do
  ch01 <- Ch01.tests
  ch02 <- Ch02.tests
  defaultMain $ testGroup "Tests"
    [ ch01
    , ch02
    ]
