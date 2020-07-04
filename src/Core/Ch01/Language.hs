module Core.Ch01.Language
  ( CoreExpr
  , Expr(..)
  , Name(..)

  , IsRec
  , recursive
  , nonRecursive

  , Alter
  , CoreAlt

  , bindersOf
  , rhssOf
  , isAtomicExpr

  , Program
  , CoreProgram
  , ScDefn
  , CoreScDefn

  ) where

import Data.Text (Text)

type CoreExpr = Expr Name

data Expr a
  = EVar Name              -- ^ Variables
  | ENum Int               -- ^ Numbers
  | EConstr Int Int        -- ^ Constructor with tag and arity
  | EAp (Expr a) (Expr a)  -- ^ Applications
  | ELet                   -- ^ Let(rec) expressions
      IsRec                -- Indicates if this is a recursive expression
      [(a, Expr a)]        -- Definitions
      (Expr a)             -- Body of let(rec)
  | ECase                  -- ^ Case expression
      (Expr a)             -- Expression to scrutinize
      [Alter a]            -- Alternatives
  | ELam [a] (Expr a)      -- ^ Lambda abstractions
  deriving (Show)

newtype Name = Name { unName :: Text }
  deriving (Eq, Show)

type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = True

-- | Case expression alternative.
data Alter a = Alter
  Int      -- Constructor tag
  [a]      -- Bound variables
  (Expr a) -- Expression body
  deriving (Show)

type CoreAlt = Alter Name

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

-- | Program is a list of supercombinator definitions.
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- | Supercombinator definition.
-- The argument list might be empty,
-- in the case of a supercombinator with non arguments.
type ScDefn a = (Name, [a], Expr a)

type CoreScDefn = ScDefn Name
