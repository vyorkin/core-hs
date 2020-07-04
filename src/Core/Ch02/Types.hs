{-# LANGUAGE TypeOperators #-}

module Core.Ch02.Types
  ( (:=>)
  ) where

-- | Represents an association between 'a' and 'b'.
-- Maps names to addresses in a heap.
type a :=> b = [(a, b)]
