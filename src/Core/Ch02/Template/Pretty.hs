module Core.Ch02.Template.Pretty
  ( ppStates
  , ppState
  , ppStack
  ) where

import Data.Text.Prettyprint.Doc
  (Doc, Pretty(..), vcat, vsep, (<+>), fill,
   align, annotate, nest, hardline)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

import Core.Ch02.Addr (Addr)
import Core.Ch02.Template.Types (State, Stack, NodeHeap, Node(..), Stats(..))
import qualified Core.Ch02.Heap as Heap
import Core.Ch02.Pretty (catsep)
import qualified Core.Ch02.Template.Pretty.Style as Style

ppStates :: [State] -> Doc AnsiStyle
ppStates ss = vsep
  [ catsep vcat hardline ppState ss
  , ppStats (last ss)
  ]

ppStats :: State -> Doc AnsiStyle
ppStats (_, _, _, _, stats) =
      hardline <> "Stats:"
   <> nest 1 hardline
  <+> "Total number of steps:"
  <+> pretty (getSteps stats)

ppState :: State -> Doc AnsiStyle
ppState (stack, _, heap, _, _) = ppStack heap stack

-- | Pretty-prints stack nodes by displaying an
-- address of a node and contents this address points to.
ppStack :: NodeHeap -> Stack -> Doc AnsiStyle
ppStack = catsep vcat mempty . ppStackItem

ppStackItem :: NodeHeap -> Addr -> Doc AnsiStyle
ppStackItem heap addr =
  let node = Heap.lookup heap addr
   in fill 4 (annotate Style.addr $ pretty addr)
  <+> ":"
  <+> ppStackNode node

ppStackNode :: Node -> Doc AnsiStyle
ppStackNode = \case
  NAp e1 e2 ->
        annotate Style.ap "NAp"
    <+> pretty e1
    <+> pretty e2
  NSupercomb name _ _ ->
        annotate Style.supercomb "NSupercomb"
    <+> pretty name
  NNum n ->
        annotate Style.num "NNum"
    <+> pretty n
