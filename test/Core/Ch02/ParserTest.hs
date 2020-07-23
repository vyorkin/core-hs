module Core.Ch02.ParserTest
  ( mkTests
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import System.FilePath ((<.>))

import Test.Tasty (TestTree, testGroup)

import Core.Ch02.Parser (parseProgram')
import Core.TestUtils (glob, golden, showEither)

mkTests :: FilePath -> IO TestTree
mkTests path = do
  paths <- glob path "*.core"
  tests <- mapM (golden parseFile) paths
  pure $ testGroup "parser" tests

parseFile :: FilePath -> IO ByteString
parseFile path = do
  program <- readFile path
  let actual = showEither $ parseProgram' program
  pure $ ByteString.pack actual
