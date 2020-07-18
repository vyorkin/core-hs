{-# LANGUAGE TypeOperators #-}

module Core.Ch02.Template.Types
  ( TiState
  , TiStack
  , TiDump
  , TiHeap
  , Node(..)
  , TiGlobals
  , TiStats
  ) where

import Core.Ch02.Addr (Addr)
import Core.Ch02.Heap (Heap)
import Core.Ch02.Language (CoreExpr, Name(..))

-- | State of our Template Instantiation machine.
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

-- | Spine stack is stack of addresses, each of which identifies
-- a node in the heap. These nodes form the spine of the
-- expressions being evaluated.
type TiStack = [Addr]

-- | We'll need it later, so we give it a dummy definition for now.
type TiDump = ()

-- | Heap of (tagged) nodes.
type TiHeap = Heap Node

-- | Represents possible nodes in our graph.
data Node
  = NAp Addr Addr -- ^ Application
  | NSupercomb    -- ^ Supercombinator
      Name        -- Holds the name of the supercombinator (used for debugging / odc)
      [Name]      -- List of argument names
      CoreExpr    -- Body expression
  | NNum Int      -- ^ A number

-- | Mappings from supercombinator names to their addresses on a heap.
type TiGlobals = [(Name, Addr)]

-- | Used to collect the runtime performance statistics on what
-- the machine does. For now we will record only the number of steps taken.
type TiStats = Int
