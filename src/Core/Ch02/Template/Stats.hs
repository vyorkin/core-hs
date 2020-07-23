module Core.Ch02.Template.Stats
  ( init
  , incSteps
  , decSteps
  , getSteps
  , apply
  ) where

import Core.Ch02.Template.Types (State, Stats(..))

incSteps :: Stats -> Stats
incSteps = Stats . (+) 1 . getSteps

decSteps :: Stats -> Stats
decSteps = Stats . (-) 1 . getSteps

-- | Applies a given function to
-- the statistics component of the state.
apply :: (Stats -> Stats) -> State -> State
apply f (stack, dump, heap, globals, stats) =
  (stack, dump, heap, globals, f stats)
