{-# LANGUAGE TypeOperators #-}

module Core.Ch02.Heap
  ( Heap

  , empty
  , alloc
  , free
  , update
  , lookup

  , addresses
  , size
  ) where

import Prelude hiding (lookup)

import Core.Ch02.Types ((:=>))
import Core.Ch02.Addr (Addr)
import Core.Ch02.Utils (lookupDef)

-- | Mapping of nodes of type 'a' to their addresses.
type Heap a =
  ( Int        -- Heap size
  , [Addr]     -- List of available/free addresses
  , Addr :=> a -- List of nodes with their associated addresses
  )

-- | Empty 'Heap'.
empty :: Heap a
empty = (0, [1..], [])

-- | Allocates a new 'Addr' on a 'Heap'.
alloc :: Heap a -> a -> (Heap a, Addr)
alloc (sz, addr:addrs, nodes) node =
  let heap = (sz + 1, addrs, (addr, node):nodes)
  in (heap, addr)
alloc (_, [], _) _ = error "Impossible happened"

-- | Frees/deallocates a given address.
free :: Heap a -> Addr -> Heap a
free (sz, addrs, nodes) addr =
  ( sz - 1
  , addr:addrs
  , remove addr nodes
  )

-- | Updates a node on a given address.
update :: Heap a -> Addr -> a -> Heap a
update (sz, addrs, nodes) addr node =
  (sz, addrs, (addr, node) : remove addr nodes)

-- | Looks up a node by address.
lookup :: Heap a -> Addr -> a
lookup (_, _, nodes) addr = lookupDef addr err nodes where
  err = error $ "Can't find node #" ++ show addr ++ " in heap"

-- | Returns a list of allocated addresses.
addresses :: Heap a -> [Addr]
addresses (_, _, nodes) = [addr | (addr, _) <- nodes]

-- | Returns size of a 'Heap'.
size :: Heap a -> Int
size (sz, _, _) = sz

remove :: Addr -> Addr :=> a -> Addr :=> a
remove addr = filter ((==) addr . fst)
