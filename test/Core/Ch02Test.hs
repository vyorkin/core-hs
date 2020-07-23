module Core.Ch02Test where

import System.FilePath ((</>))

import Test.Tasty (TestTree, testGroup)

import qualified Core.Ch02.PrettyTest as Pretty
import qualified Core.Ch02.ParserTest as Parser
import qualified Core.Ch02.TemplateTest as Template

tests :: IO TestTree
tests = do
  pretty <- Pretty.mkTests (dir </> "pretty")
  parser <- Parser.mkTests (dir </> "parser")
  template <- Template.mkTests (dir </> "template")
  pure $ testGroup "Ch02"
    [ pretty
    , parser
    , template
    ]
  where
    dir = "test/Core/golden/ch02"
