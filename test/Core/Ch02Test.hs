module Core.Ch02Test where

import System.FilePath ((</>))

import Test.Tasty (TestTree, testGroup)

import qualified Core.Ch02.PrettyTest as Pretty
import qualified Core.Ch02.ParserTest as Parser

tests :: IO TestTree
tests = do
  pretty <- Pretty.mkTests (dir </> "pretty")
  parser <- Parser.mkTests (dir </> "parser")
  pure $ testGroup "Ch02"
    [ pretty
    , parser
    ]
  where
    dir = "test/Core/golden/ch02"
