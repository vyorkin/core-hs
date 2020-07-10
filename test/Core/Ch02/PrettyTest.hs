module Core.Ch02.PrettyTest
  ( mkTests
  ) where

import Debug.Trace (traceM)
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 (ByteString)
import System.FilePath ((<.>))

import Test.Tasty (TestTree, testGroup)

import Core.Ch02.Language (CoreProgram)
import Core.Ch02.Pretty (renderRaw, ppProgram)

import Core.TestUtils (glob, golden, textToBs)

mkTests :: FilePath -> IO TestTree
mkTests = mkTestGroup "ppProgram" renderProgram "ast"

renderProgram :: CoreProgram -> Text
renderProgram = renderRaw ppProgram

mkTestGroup
  :: Read a
  => String      -- ^ Test group name
  -> (a -> Text) -- ^ Printer function
  -> FilePath    -- ^ Extra extension
  -> FilePath    -- ^ Base path
  -> IO TestTree
mkTestGroup group pp ext path = do
  let pat = "*" <.> "core" <.> ext
  traceM $ "pat: " <> pat
  paths <- glob path pat
  cases <- mapM (golden $ ppFile pp) paths
  pure $ testGroup group cases

ppFile :: Read a => (a -> Text) -> FilePath -> IO ByteString
ppFile pp path = read <$> readFile path >>= pure . textToBs . pp
