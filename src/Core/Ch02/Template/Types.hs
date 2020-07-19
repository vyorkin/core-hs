{-# LANGUAGE TypeOperators #-}

module Core.Ch02.Template.Types
  ( State
  , Stack
  , Dump
  , Heap
  , Node(..)
  , Globals
  , Stats
  ) where

import Core.Ch02.Addr (Addr)
import qualified Core.Ch02.Heap as H (Heap)
import Core.Ch02.Language (CoreExpr, Name(..))

-- | State of our Template Instantiation machine.
type State = (Stack, Dump, Heap, Globals, Stats)

-- | Spine stack is stack of addresses, each of which identifies
-- a node in the heap. These nodes form the spine of the
-- expressions being evaluated.
type Stack = [Addr]

-- | We'll need it later, so we give it a dummy definition for now.
type Dump = ()

-- | Heap of (tagged) nodes.
type Heap = H.Heap Node

-- | Represents possible nodes in our graph.
data Node
  = NAp Addr Addr -- ^ Application
  | NSupercomb    -- ^ Supercombinator
      Name        -- Holds the name of the supercombinator (used for debugging / odc)
      [Name]      -- List of argument names
      CoreExpr    -- Body expression
  | NNum Int      -- ^ A number

-- | Mappings from supercombinator names to their addresses on a heap.
type Globals = [(Name, Addr)]

-- | Used to collect the runtime performance statistics on what
-- the machine does. For now we will record only the number of steps taken.
type Stats = Int
