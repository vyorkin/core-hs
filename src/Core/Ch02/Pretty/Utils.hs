module Core.Ch02.Pretty.Utils
  ( renderRaw
  , renderAnn
  , render
  , layout
  , parensIf
  , asep
  , names
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
  (Doc, SimpleDocStream, Pretty(..), unAnnotate, parens, align,
   sep, hsep, annotate, (<+>), layoutSmart, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal (renderStrict)

renderRaw :: (a -> Doc AnsiStyle) -> a -> Text
renderRaw f = render renderStrict (unAnnotate . f)

renderAnn :: (a -> Doc AnsiStyle) -> a -> Text
renderAnn = render Terminal.renderStrict

render
  :: (SimpleDocStream AnsiStyle -> a)
  -> (e -> Doc AnsiStyle)
  -> e -> a
render f pp = f . layout pp

layout :: (a -> Doc ann) -> a -> SimpleDocStream ann
layout f = layoutSmart defaultLayoutOptions . f

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

names :: Pretty a => AnsiStyle -> [a] -> Doc AnsiStyle
names style ns = annotate style $ hsep (pretty <$> ns)

asep :: Doc ann -> (a -> Doc ann) -> [a] -> Doc ann
asep s f = align . sep . zipWith (<+>) (repeat s) . map f
