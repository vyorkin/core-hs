module Core.Ch02.Template1 where

-- State of the Template Instantiation graph reduction machine
-- (stack, dump, heap, globals)
-- (s, d, h, f)

-- Stack - stack of addresses, each of which identifies a
-- node in the heap. These nodes form the spine of the
-- expressions being evaluated.

-- Dump - records the state of the spine stack prior to the evaluation
-- of an argument of a strict primitive. (Will be used later).

-- Heap - collection of tagged nodes.

-- `h[a:node]` means that:
--   in the heap `h`
--   the address `a`
--   refers to the `node`

-- Globals - gives the address of node on the heap
-- representing the supercombinator (or primitive).
