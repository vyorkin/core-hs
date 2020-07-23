module Core.Ch02.TemplateTest
  ( mkTests
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Test.Tasty (TestTree, testGroup)

import Core.Ch02.Template (runProg)
import Core.Ch02.Pretty (rendererRaw)
import Core.TestUtils (glob, golden)

mkTests :: FilePath -> IO TestTree
mkTests path = do
  paths <- glob path "*.core"
  tests <- mapM (golden runFile) paths
  pure $ testGroup "template" tests

runFile :: FilePath -> IO ByteString
runFile path = do
  program <- readFile path
  pure
    . ByteString.pack
    . Text.unpack
    . runText
    . Text.pack
    $ program

runText :: Text -> Text
runText = runProg rendererRaw
