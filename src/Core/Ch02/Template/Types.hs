{-# LANGUAGE TypeOperators #-}

module Core.Ch02.Template.Types
  ( State
  , Stack
  , Dump
  , NodeHeap
  , Node(..)
  , Globals
  , Stats(..)
  ) where

import Core.Ch02.Addr (Addr)
import Core.Ch02.Heap (Heap)
import Core.Ch02.Language (CoreExpr, Name(..))

-- | State of our Template Instantiation machine.
type State = (Stack, Dump, NodeHeap, Globals, Stats)

-- | Used to collect the runtime performance statistics on what
-- the machine does. For now we will record only the number of steps taken.
newtype Stats = Stats { getSteps :: Int }

instance Semigroup Stats where
  (Stats x) <> (Stats y) = Stats $ x + y

instance Monoid Stats where
  mempty = Stats 0

-- | Spine stack is stack of addresses, each of which identifies
-- a node in the heap. These nodes form the spine of the
-- expressions being evaluated.
type Stack = [Addr]

-- | We'll need it later, so we give it a dummy definition for now.
type Dump = ()

-- | Heap of (tagged) 'Node's.
type NodeHeap = Heap Node

-- | Represents possible nodes in our graph.
data Node
  = NAp Addr Addr -- ^ Application
  | NSupercomb    -- ^ Supercombinator
      Name        -- Holds the name of the supercombinator (used for debugging / odc)
      [Name]      -- List of argument names
      CoreExpr    -- Body expression
  | NNum Int      -- ^ A number
  deriving (Eq, Show)

-- | Mappings from supercombinator names to their addresses on a heap.
type Globals = [(Name, Addr)]
