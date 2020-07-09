module Core.Ch02.Pretty
  ( ppScDefn
  , ppExpr
  , ppExprAt

  , module Core.Ch02.Pretty.Utils
  ) where

import Data.Text.Prettyprint.Doc
  (Doc, Pretty(..), braces, hardline, angles, semi, dot, equals,
   comma, backslash, align, sep, unAnnotate, vcat, (<+>), annotate)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

import Core.Ch02.Pretty.Utils (renderRaw, renderAnn, render, parensIf, asep, names)
import qualified Core.Ch02.Pretty.Style as Style
import Core.Ch02.Language (Program, ScDefn(..), Expr(..), IsRec, Defn(..), Alter(..))

ppProgram :: Pretty a => Program a -> Doc AnsiStyle
ppProgram = vcat . map ppScDefn

ppScDefn :: Pretty a => ScDefn a -> Doc AnsiStyle
ppScDefn (name, args, body) =
      pretty name
  <+> names Style.var args
  <+> equals
  <+> ppExpr body

ppExpr :: Pretty a => Expr a -> Doc AnsiStyle
ppExpr = ppExprAt 0

ppRec :: IsRec -> Doc AnsiStyle
ppRec True = "rec"
ppRec False = mempty

ppExprAt :: Pretty a => Int -> Expr a -> Doc AnsiStyle
ppExprAt _ (EVar var) = annotate Style.var $ pretty var
ppExprAt _ (ENum num) = annotate Style.lit $ pretty num
ppExprAt _ (EConstr tag arity) = annotate Style.constr $
  braces (pretty tag <> comma <> pretty arity)
ppExprAt d (EAp e1 e2) = parensIf (d > 0) $
      ppExprAt (d + 1) e1
  <+> ppExprAt (d + 1) e2
ppExprAt d (ELet rec defs body) =
      annotate Style.letin "let"
  <+> annotate Style.letin (ppRec rec)
  <+> ppDefnsAt d defs
  <+> annotate Style.letin "in"
  <+> ppExprAt d body
ppExprAt d (ECase cond alters) =
      annotate Style.caseof "case"
  <+> ppExprAt d cond
  <+> annotate Style.caseof "of"
  <+> hardline
  <+> ppAltersAt d alters
ppExprAt d (ELam vars body) = parensIf (d > 0) $
      annotate Style.lam backslash
  <+> names Style.var vars
  <+> annotate Style.dot dot
   <> ppExprAt 0 body

ppDefnsAt :: Pretty a => Int -> [Defn a] -> Doc AnsiStyle
ppDefnsAt = asep semi . ppDefnAt

ppDefnAt :: Pretty a => Int -> Defn a -> Doc AnsiStyle
ppDefnAt d (Defn n e) = pretty n <+> equals <+> ppExprAt d e

ppAltersAt :: Pretty a => Int -> [Alter a] -> Doc AnsiStyle
ppAltersAt = asep semi . ppAlterAt

ppAlterAt :: Pretty a => Int -> Alter a -> Doc AnsiStyle
ppAlterAt d (Alter tag vars body) =
      annotate Style.alter (angles $ pretty tag)
  <+> names Style.var vars
  <+> annotate Style.arrow "->"
  <+> ppExprAt d body
