module Core.Ch01Test where

import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = pure $ testGroup "Ch01" []
