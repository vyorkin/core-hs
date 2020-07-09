module Core.Ch02.PrettyTest
  ( mkTests
  ) where

import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)

import Test.Tasty (TestTree, testGroup)

import Core.Ch02.Language (CoreProgram)
import Core.Ch02.Pretty (renderRaw, ppProgram)

import Core.TestUtils (glob, golden, textToBs)

mkTests :: FilePath -> IO TestTree
mkTests = mkTestGroup "ppProgram" renderProgram

renderProgram :: CoreProgram -> Text
renderProgram = renderRaw ppProgram

mkTestGroup
  :: Read a
  => String      -- ^ Test group name
  -> (a -> Text) -- ^ Printer function
  -> FilePath    -- ^ Base path
  -> IO TestTree
mkTestGroup group pp path = do
  paths <- glob path "*.core"
  cases <- mapM (golden $ ppFile pp) paths
  pure $ testGroup group cases

ppFile :: Read a => (a -> Text) -> FilePath -> IO ByteString
ppFile pp path = read <$> readFile path >>= pure . textToBs . pp
