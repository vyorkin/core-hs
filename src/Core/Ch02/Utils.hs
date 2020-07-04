module Core.Ch02.Utils
  ( lookupDef
  ) where

import Data.Maybe (fromMaybe)

-- | Looks up a key in an association list.
-- Defaults to 'def' if nothing found.
lookupDef :: Eq a => a -> b -> [(a, b)] -> b
lookupDef k def = fromMaybe def . lookup k
