module Core.Ch02.Pretty.Types
  ( Printer
  ) where

import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

-- | Something that knows how to print 'a'.
type Printer a = a -> Doc AnsiStyle
