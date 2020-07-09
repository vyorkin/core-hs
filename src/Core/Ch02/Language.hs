module Core.Ch02.Language
  ( CoreProgram
  , CoreScDefn
  , CoreExpr
  , CoreAlt

  , Program
  , ScDefn

  , module Core.Ch02.Language.Name
  , module Core.Ch02.Language.Expr

  ) where

import Core.Ch02.Language.Name (Name(..), unName)
import Core.Ch02.Language.Expr
  (Expr(..), IsRec, Alter(..), Defn(..), recursive,
   nonRecursive, bindersOf, rhssOf, isAtomicExpr)

type CoreProgram = Program Name
type CoreScDefn  = ScDefn Name
type CoreExpr    = Expr Name
type CoreAlt     = Alter Name

-- A Core program consists of a set of supercombinator definitions,
-- including a distinguished one, 'main'. To execute a program we
-- evaluate 'main'.

-- | Program is a list of supercombinator definitions.
type Program a = [ScDefn a]

-- Each supercombinator is defined by a single equation whose
-- arguments are all simple variables. Not all supercombinator
-- have arguments. Supercombinators with no arguments are called
-- constant applicative forms (CAF's).

-- | Supercombinator definition. The argument list might be
-- empty, in the case of a supercombinator with non arguments.
type ScDefn a = (Name, [a], Expr a)

-- No pattern matching is permitted for supercombinator arguments.
-- Pattern matching is performed by the 'case' expression.
