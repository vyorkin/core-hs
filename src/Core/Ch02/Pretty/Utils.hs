module Core.Ch02.Pretty.Utils
  ( renderRaw
  , renderAnn
  , render
  , layout
  , parensIf
  , catsep
  , names
  , (<%>)
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
  (Doc, SimpleDocStream, Pretty(..), unAnnotate, parens, align,
   hsep, annotate, (<+>), layoutSmart, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal (renderStrict)
import Core.Ch02.Pretty.Types (Printer)

renderRaw :: Printer a -> a -> Text
renderRaw f = render renderStrict (unAnnotate . f)

renderAnn :: Printer a -> a -> Text
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

catsep
  :: ([Doc ann] -> Doc ann)
  -> Doc ann
  -> (a -> Doc ann)
  -> [a]
  -> Doc ann
catsep cat s f =
    align
  . cat
  . zipWith (<>) (mempty : repeat s)
  . map f

-- | Concatenates a document and a list of
-- styled 'a's using the '<+>' operator if there are any elements in it.
-- Otherwise returns a document as is.
(<%>) :: Pretty a => Doc AnsiStyle -> (AnsiStyle, [a]) -> Doc AnsiStyle
d <%> (_, []) = d
d <%> (s, xs) = d <+> annotate s (hsep (pretty <$> xs))
