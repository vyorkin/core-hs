module Core.Ch02.Template.Pretty.Style
  ( ap
  , supercomb
  , num
  ) where

import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, color, colorDull, Color(..), bold)

ap, supercomb, num :: AnsiStyle

ap        = color Yellow
supercomb = color Blue <> bold
num       = color White
