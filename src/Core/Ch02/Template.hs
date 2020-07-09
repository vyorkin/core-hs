{-# LANGUAGE TypeOperators #-}

module Core.Ch02.Template where

-- Spine (of expression) - left-branching chain of application ndoes

-- Unwinding spine - process of following the left branch of
-- the application nodes, starting at the root, until we get to
-- a supercombinator or built-in primitive

-- Stack is used to remember the addresses of the nodes
-- encountered on the way down the spine

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

-- Globals - global mapping of names to heap addresses of
-- supercombinators and primitives.


-- ------------------------------------------
-- Mark-1 transition rules:
-- ------------------------------------------

-- 1. Unwinding of a single application node:

--          a : s  d  h[a : NAp a1 a2]  f
-- ==> a1 : a : s  d  h                 f

-- 2. Supercombinator reduction:

--     a0 : a1 : ... : an : s  d  h[a0 : NSupercomb [x1,...,xn] body]  f
-- ==>                 ar : s  d  h'                                   f
--     where
--     (h', ar) = instantiate body h f[x -> a1, ..., xn -> an]

-- body - expression to "instantiate"
-- h - heap
-- f[x -> a1, ..., xn -> an] - global mapping of names to
-- heap addresses (f) augmented by the mapping of
-- argument names to their addresses obtained from the stack

-- ar - address of the (root of the) newly constructed instance

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import Data.List (mapAccumL)

import Core.Ch02.Utils (lookupDef)
import Core.Ch01.Language (
  CoreProgram, CoreExpr, Expr(..),
  Name(..), Alter, IsRec, CoreScDefn)
import Core.Ch02.Types ((:=>))
import Core.Ch02.Heap (Heap)
import qualified Core.Ch02.Heap as Heap
import Core.Ch02.Addr (Addr)
import qualified Core.Ch02.Addr as Addr
import qualified Core.Ch01.Prelude as Prelude

-- | Runs a program.
-- Returns the results of it's execution.
runProg :: Text -> Text
runProg =
    prettyPrint
  . eval
  . compile
  . parse

-- | Parses a program from the
-- expression found in a specified file.
parse :: Text -> CoreProgram
parse = undefined

-- | Translates a program into a form suitable for execution.
compile :: CoreProgram -> TiState
compile program =
  ( tiStackInit
  , tiDumpInit
  , tiHeapInit
  , tiGlobalsInit
  , tiStatsInit
  )
  where
    supercombinators = program ++ Prelude.defs ++ extraPreludeDefs
    (tiHeapInit, tiGlobalsInit) = mkTiHeap supercombinators
    -- Initial stack contains just one item, the address of
    -- the node for supercombinator 'main', obtained from 'globals'
    tiStackInit = [mainAddr]
    mainAddr = lookupDef (Name "main") mainErr tiGlobalsInit
    mainErr  = error "main is not defined"

-- | List of any further standart functions we may want to add.
-- For the present it is empty.
extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

-- | Executes a program, by performing repeated state
-- transitions until a final state is reached.
-- The result is a list of all the states passed through.
eval :: TiState -> [TiState]
eval state = state : restStates
  where
    nextState  = doAdmin . step $ state
    restStates | tiFinal state = []
               | otherwise     = eval nextState

-- | Detects the final state.
tiFinal :: TiState -> Bool
tiFinal ([addr], _, heap, _, _) =
  let node = Heap.lookup heap addr
   in isDataNode node
tiFinal ([], _, _, _, _) = error "Empty stack"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

-- | Maps one state into its successor.
step :: TiState -> TiState
step state@(stack, _, heap, _, _) =
  let
    addr = head stack
    node = Heap.lookup heap addr
   in
    dispatch node
  where
    dispatch (NNum n) = stepNum state n
    dispatch (NAp a1 a2) = stepAp state a1 a2
    dispatch (NSupercomb name args body) = stepSupercomb state name args body

-- If there is a number the end of a spine then
-- it means that we're trying to apply someting to a number.
stepNum :: TiState -> Int -> TiState
stepNum _ _ = error "Number applied as a function";

-- | Unwinds a single application node by
-- pushing it onto our spine stack.
stepAp :: TiState -> Addr -> Addr -> TiState
stepAp (stack, dump, heap, globals, stats) a1 _ =
  (a1 : stack, dump, heap, globals, stats)

-- | Supercombinator reduction.
stepSupercomb :: TiState -> Name -> [Name] -> CoreExpr -> TiState
stepSupercomb (stack, dump, heap, globals, stats) name args body =
  (stack'', dump, heap', globals, stats)
  where
    stack'  = drop (length (name : args)) stack
    stack'' = resAddr : stack'
    (heap', resAddr) = inst heap env body

    env = globals ++ bindings
    bindings = zip args addrs
    addrs = pullAddr . Heap.lookup heap <$> stack

    pullAddr (NAp _ addr) = addr
    pullAddr _            = error "Not an application node"

-- | Creates an "instance" of the expression in the heap.
-- Returns the new heap and address of the root of the "instance".
-- This is a heart of template instantiation machine.
inst
  :: TiHeap        -- Heap before instantiation
  -> Name :=> Addr -- Association of names to addresses
  -> CoreExpr      -- Body of supercombinator
  -> (TiHeap, Addr)
inst heap env = \case
  EVar v -> (heap, lookupDef v (undefVar v) env)
  ENum n -> Heap.alloc heap (NNum n)
  EConstr tag arity -> instConstr heap env tag arity
  EAp e1 e2 ->
    let (heap',  a1) = inst heap env e1
        (heap'', a2) = inst heap' env e2
     in Heap.alloc heap'' (NAp a1 a2)
  ELet isRec defs body -> instLet heap env isRec defs body
  ECase expr alts -> instCase heap env expr alts
  ELam _args _expr -> error "Attempt to instantiate a lambda expression"
  where
    undefVar :: Name -> a
    undefVar =
        error
      . Text.unpack
      . (<>) "Undefined name: "
      . unName

-- | Instantiates type constructor.
instConstr
  :: TiHeap        -- Heap
  -> Name :=> Addr -- (Augmented) env
  -> Int           -- Tag
  -> Int           -- Arity
  -> (TiHeap, Addr)
instConstr _heap _env _tag _arity =
  error "Can't instantiate consturctors yet"

instLet
  :: TiHeap        -- Heap
  -> Name :=> Addr -- (Augmented) env
  -> IsRec         -- Is recursive let
  -> a :=> Expr a  -- Definitions
  -> (Expr a)      -- Body of let(rec)
  -> (TiHeap, Addr)
instLet _heap _env _isrec _defs _body =
  error "Can't instantiate let(isrec) yet"

instCase
  :: TiHeap        -- Heap
  -> Name :=> Addr -- (Augmented) env
  -> Expr a
  -> [Alter a]
  -> (TiHeap, Addr)
instCase _heap _env _expr _alts =
  error "Can't instantiate case expressions yet"

-- | Does any administrative work required between steps.
-- For now we just increase the number of steps taken so far.
doAdmin :: TiState -> TiState
doAdmin = applyToStats tiStatsIncSteps

-- | Pretty-prints results of an execution.
prettyPrint :: [TiState] -> Text
prettyPrint = undefined

-- | State of our Template Instantiation machine.
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

-- | Spine stack is stack of addresses, each of which identifies
-- a node in the heap. These nodes form the spine of the
-- expressions being evaluated.
type TiStack = [Addr]

-- | We'll need it later, so we give it a dummy definition for now.
type TiDump = ()

tiDumpInit :: TiDump
tiDumpInit = ()

-- | Heap of (tagged) nodes.
type TiHeap = Heap Node

-- | Constructs an initial heap containing 'NSupercomb' node
-- for each supercombinator, together with an association list 'TiGlobals'
-- which maps each supercombinator name onto the address of its node.
mkTiHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
mkTiHeap = mapAccumL allocSc Heap.empty

-- mapAccumL
--   :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)

-- λ> mapAccumL (\acc x -> (acc + 1, x + 1)) 0 [1..5] :: (Int, [Int])
-- (5,[2,3,4,5,6])

allocSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocSc heap (name, args, body) = (heap', global) where
  (heap', addr) = Heap.alloc heap node
  node = NSupercomb name args body
  global = (name, addr)

-- | Represents possible nodes in our graph.
data Node
  = NAp Addr Addr -- ^ Application
  | NSupercomb    -- ^ Supercombinator
      Name        -- Holds the name of the supercombinator (used for debugging / odc)
      [Name]      -- List of argument names
      CoreExpr    -- Body expression
  | NNum Int      -- ^ A number

-- | Mappings from supercombinator names to their addresses on a heap.
type TiGlobals = Name :=> Addr

-- | Used to collect the runtime performance statistics on what
-- the machine does. For now we will record only the number of steps taken.
type TiStats = Int

tiStatsInit :: TiStats
tiStatsInit = 0

tiStatsIncSteps :: TiStats -> TiStats
tiStatsIncSteps = (+) 1

tiStatsDecSteps :: TiStats -> TiStats
tiStatsDecSteps = (-) 1

tiStatsGetSteps :: TiStats -> Int
tiStatsGetSteps = id

-- | Applies a given function to
-- the statistics component of the state.
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, globals, stats) =
  (stack, dump, heap, globals, f stats)
