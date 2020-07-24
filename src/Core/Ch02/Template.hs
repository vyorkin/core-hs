-- Mark-1
-- ------
-- Simplest possible implementation of a
-- graph reducer based on Template Instantiation.

-- Spine (of expression) - left-branching chain of application nodes

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

-- Example:
-- --------
-- f E1 E2 E3
-- where 'f' takes 2 arguments
-- outermost application is: f E1 E2
-- spine: [1, 2, 3]

-- stack
--  ---
-- | *-|--------> 3
-- |---|         / \
-- | *-|-----> @2!  E3  <<- outermost function application
-- |---|      /   \
-- | *-|---> @1    E2
-- |---|    /  \
-- | *-|-> f    E1
--  ---

-- Heap - collection of tagged (addressable) nodes.

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

module Core.Ch02.Template
  ( runProg
  , parse
  , compile
  , eval
  , step
  , inst
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (mapAccumL)

import Core.Ch02.Utils (lookupDef)
import Core.Ch02.Language (
  CoreProgram, CoreExpr, Expr(..),
  Name(..), Defn(..), unName, Alter, IsRec, CoreScDefn)
import qualified Core.Ch02.Heap as Heap
import Core.Ch02.Addr (Addr)
import qualified Core.Ch02.Prelude as Prelude
import Core.Ch02.Parser (parseProgram')
import Core.Ch02.Template.Types (State, Dump, NodeHeap, Node(..), Globals)
import Core.Ch02.Pretty (Renderer)
import qualified Core.Ch02.Template.Stats as Stats
import Core.Ch02.Template.Pretty (ppStates)

-- | Runs a program.
-- Returns the results of it's execution.
runProg :: Renderer [State] -> Text -> Text
runProg pp = pp ppStates . eval . compile . parse

-- | Parses a program from the
-- expression found in a specified file.
parse :: Text -> CoreProgram
parse = either error id . parseProgram' . Text.unpack

-- | Translates a program into a form suitable for execution.
compile :: CoreProgram -> State
compile program =
  ( stackInit
  , dumpInit
  , heapInit
  , globalsInit
  , mempty
  )
  where
    supercombinators = program ++ Prelude.defs ++ extraPreludeDefs

    -- [ (8,NSupercomb (Name "twice") [Name "f"] (EAp (EAp (EVar (Name "compose")) (EVar (Name "f"))) (EVar (Name "f"))))
    -- , (7,NSupercomb (Name "compose") [Name "f",Name "g",Name "x"] (EAp (EVar (Name "f")) (EAp (EVar (Name "g")) (EVar (Name "x")))))
    -- , (6,NSupercomb (Name "cons") [Name "x",Name "y"] (EAp (EAp (EConstr 2 2) (EVar (Name "x"))) (EVar (Name "y"))))
    -- , (5,NSupercomb (Name "nil") [] (EConstr 1 0))
    -- , (4,NSupercomb (Name "S") [Name "f",Name "g",Name "x"] (EAp (EAp (EVar (Name "f")) (EVar (Name "x"))) (EAp (EVar (Name "g")) (EVar (Name "x")))))
    -- , (3,NSupercomb (Name "K1") [Name "x",Name "y"] (EVar (Name "y")))
    -- , (2,NSupercomb (Name "K") [Name "x",Name "y"] (EVar (Name "x")))
    -- , (1,NSupercomb (Name "I") [Name "x"] (EVar (Name "x")))]

    (heapInit, globalsInit) = mkHeap supercombinators
    -- Initial stack contains just one item, the address of
    -- the node for supercombinator 'main', obtained from 'globals'
    stackInit = [mainAddr]
    mainAddr = lookupDef (Name "main") mainErr globalsInit
    mainErr = error "main is not defined"

-- | List of any further standart functions we may want to add.
-- For the present it is empty.
extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

-- | Executes a program, by performing repeated state
-- transitions until a final state is reached.
-- The result is a list of all the states passed through.
eval :: State -> [State]
eval state = state : restStates
  where
    nextState  = doAdmin . step $ state
    restStates | isFinal state = []
               | otherwise     = eval nextState

-- | Detects the final state.
isFinal :: State -> Bool
isFinal ([addr], _, heap, _, _) =
  let node = Heap.lookup heap addr
   in isDataNode node
isFinal ([], _, _, _, _) = error "Empty stack"
isFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

-- | Maps one state into its successor.
step :: State -> State
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

-- | If there is a number at the end of a spine then
-- it means that we're trying to apply something to a number.
stepNum :: State -> Int -> State
stepNum _ _ = error "Number applied as a function";

-- | Unwinds a single application node by
-- pushing it onto our spine stack.
stepAp :: State -> Addr -> Addr -> State
stepAp (stack, dump, heap, globals, stats) a1 _ =
  (a1 : stack, dump, heap, globals, stats)

-- | Supercombinator reduction.
stepSupercomb :: State -> Name -> [Name] -> CoreExpr -> State
stepSupercomb (stack, dump, heap, globals, stats) name args body =
  (stack'', dump, heap', globals, stats)
  where
    stack'  = drop (length (name : args)) stack
    stack'' = resAddr : stack'
    (heap', resAddr) = inst heap env body

    env :: [(Name, Addr)]
    env = globals ++ bindings

    bindings :: [(Name, Addr)]
    bindings = zip args addrs

    addrs :: [Addr]
    addrs = pullAddr . Heap.lookup heap <$> stack

    pullAddr (NAp _ addr) = addr
    pullAddr node         = error $ "Not an application node: " <> show node

-- | Creates an "instance" of the expression in the heap.
-- Returns the new heap and address of the root of the "instance".
-- This is a heart of template instantiation machine.
inst
  :: NodeHeap           -- Heap before instantiation
  -> [(Name, Addr)] -- Env: argument bindings + globals
  -> CoreExpr       -- Body of supercombinator
  -> (NodeHeap, Addr)
inst heap env = \case
  EVar v -> (heap, lookupDef v (undefVar v) env)
  ENum n -> Heap.alloc heap (NNum n)
  EConstr tag arity -> instConstr heap env tag arity
  EAp e1 e2 ->
    let (heap' , a1) = inst heap  env e1
        (heap'', a2) = inst heap' env e2
     in Heap.alloc heap'' (NAp a1 a2)
  ELet isRec defs body -> instLet heap env isRec defs body
  ECase expr alts -> instCase heap env expr alts
  -- Lambda can not be a body of a supercombinator
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
  :: NodeHeap       -- Heap
  -> [(Name, Addr)] -- (Augmented) env
  -> Int            -- Tag
  -> Int            -- Arity
  -> (NodeHeap, Addr)
instConstr _heap _env _tag _arity =
  error "Can't instantiate constructors yet"

instLet
  :: NodeHeap       -- Heap
  -> [(Name, Addr)] -- (Augmented) env
  -> IsRec          -- Is recursive let
  -> [Defn a]       -- Definitions
  -> (Expr a)       -- Body of let(rec)
  -> (NodeHeap, Addr)
instLet _heap _env _isrec _defs _body =
  error "Can't instantiate let(isrec) yet"

instCase
  :: NodeHeap       -- Heap
  -> [(Name, Addr)] -- (Augmented) env
  -> Expr a
  -> [Alter a]
  -> (NodeHeap, Addr)
instCase _heap _env _expr _alts =
  error "Can't instantiate case expressions yet"

-- | Does any administrative work required between steps.
-- For now we just increase the number of steps taken so far.
doAdmin :: State -> State
doAdmin = Stats.apply Stats.incSteps

dumpInit :: Dump
dumpInit = ()

-- | Constructs an initial heap containing 'NSupercomb' node for
-- each supercombinator, together with 'Globals' (association
-- list which maps each supercombinator name onto the address of its node).
mkHeap :: [CoreScDefn] -> (NodeHeap, Globals)
mkHeap = mapAccumL allocSc Heap.empty

-- mapAccumL
--   :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)

-- λ> mapAccumL (\acc x -> (acc + 1, x + 1)) 0 [1..5] :: (Int, [Int])
-- (5,[2,3,4,5,6])

allocSc :: NodeHeap -> CoreScDefn -> (NodeHeap, (Name, Addr))
allocSc heap (name, args, body) = (heap', global) where
  (heap', addr) = Heap.alloc heap node
  node = NSupercomb name args body
  global = (name, addr)
