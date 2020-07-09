module Core.Ch02.Pretty.Style
  ( var
  , ty
  , lam
  , lit
  , arrow
  , dot
  , colon
  , caseof
  , alter
  , letin
  , constr
  ) where

import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, color, colorDull, Color(..), bold)

var, ty, lam, arrow, dot, lit, colon, caseof, alter, letin, constr :: AnsiStyle

var     = color Magenta
ty      = color Blue <> bold
lam     = color Red <> bold
arrow   = colorDull White <> bold
dot     = color White <> bold
lit     = color Green
colon   = color Cyan
caseof  = color Red <> bold
alter   = color Blue
letin   = color Red <> bold
constr  = color Yellow
