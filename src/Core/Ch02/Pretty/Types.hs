module Core.Ch02.Pretty.Types
  ( Printer
  , Renderer
  ) where

import Data.Text (Text)

import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

-- | Something that knows how to print 'a'.
type Printer a = a -> Doc AnsiStyle

-- | 'Renderer' is something that given a
-- 'Printer a' produces a function that renders 'a' as 'Text'.
type Renderer a = Printer a -> a -> Text
