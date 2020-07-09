module Core.Ch02Test where

import Test.Tasty (TestTree, testGroup)

import qualified Core.Ch02.PrettyTest as Pretty

tests :: IO TestTree
tests = do
  pretty <- Pretty.mkTests "/pretty/"
  pure $ testGroup "Ch02"
    [ pretty
    ]
  where
    dir = "test/Core/golden/ch02"
