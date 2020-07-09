module Core.Ch02.Language.Expr
  ( Expr(..)
  , Alter(..)
  , Defn(..)
  , IsRec

  , recursive
  , nonRecursive

  , bindersOf
  , rhssOf
  , isAtomicExpr
  ) where

import Core.Ch02.Language.Name (Name)

data Expr a
  = EVar Name              -- ^ Variables
  | ENum Int               -- ^ Numbers
  | EConstr Int Int        -- ^ Constructor with tag and arity
  | EAp (Expr a) (Expr a)  -- ^ Applications
  | ELet                   -- ^ Let(rec) expressions
      IsRec                -- Indicates if this is a recursive expression
      [Defn a]             -- Definitions
      (Expr a)             -- Body of let(rec)
  | ECase                  -- ^ Case expression
      (Expr a)             -- Expression to scrutinize
      [Alter a]            -- Alternatives
  | ELam [a] (Expr a)      -- ^ Lambda abstractions
  deriving (Eq, Read, Show)

data Defn a = Defn a (Expr a)
  deriving (Eq, Read, Show)

type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = True

-- | Case expression alternative.
data Alter a = Alter
  Int      -- Constructor tag
  [a]      -- Bound variables
  (Expr a) -- Expression body
  deriving (Eq, Read, Show)

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False
