module Core.Ch02.Template.Pretty
  (
  ) where

import Data.Text.Prettyprint.Doc
  (Doc, Pretty(..), braces, hardline, nest, angles, semi, space,
   dot, equals, comma, backslash, hcat, vcat, align, (<+>), annotate)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

import Core.Ch02.Template.Types (State, Stack, Dump, Heap, Node(..), Globals, Stats)
import Core.Ch02.Pretty (renderRaw, renderAnn, render, parensIf, catsep, (<%>))
import qualified Core.Ch02.Template.Pretty.Style as Style
