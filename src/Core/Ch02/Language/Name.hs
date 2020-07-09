module Core.Ch02.Language.Name
  ( Name(..)
  , unName
  ) where

import Data.Text (Text)
import Data.String (IsString(..))
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Pretty(..))

newtype Name = Name Text
  deriving (Eq, Read, Show)

instance IsString Name where
  fromString = Name . Text.pack

instance Pretty Name where
  pretty (Name s) = pretty s

unName :: IsString a => Name -> a
unName (Name s) = fromString $ Text.unpack s
