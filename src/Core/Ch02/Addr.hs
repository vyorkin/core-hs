module Core.Ch02.Addr
  ( Addr
  , null
  , isNull
  ) where

import Prelude hiding (null)

-- | Represents a 'Heap' address.
type Addr = Int

null :: Addr
null = 0

isNull :: Addr -> Bool
isNull = (==) null
