module Core.Ch01.Prelude
  ( defs
  ) where

import Core.Ch01.Language (CoreProgram, Expr(..), Name(..))

-- "standard prelude" for our Core language

-- I x = x
-- K x y = x
-- K1 x y = y
-- S f g x = f x (g x)
-- compose f g x = f (g x)
-- twice f = compose f f

defs :: CoreProgram
defs =
  [ ( Name "I"
    , [Name "x"]
    , EVar (Name "x")
    )
  , ( Name "K"
    , [Name "x", Name "y"]
    , EVar (Name "x")
    )
  , ( Name "K1"
    , [Name "x", Name "y"]
    , EVar (Name "y")
    )
  , ( Name "S"
    , [Name "f", Name "g", Name "x"]
    , EAp
      (EAp (EVar (Name "f")) (EVar (Name "x")))
      (EAp (EVar (Name "g")) (EVar (Name "x")))
    )
  , ( Name "nil"
    , []
    , EConstr 1 0
    )
  , ( Name "cons"
    , [Name "x", Name "y"]
    , EAp
      (EAp (EConstr 2 2) (EVar (Name "x")))
      (EVar (Name "y"))
    )
  , ( Name "compose"
    , [Name "f", Name "g", Name "x"]
    , EAp
      (EVar (Name "f"))
      (EAp (EVar (Name "g")) (EVar (Name "x")))
    )
  , ( Name "twice"
    , [Name "f"]
    , EAp
      (EAp (EVar (Name "compose")) (EVar (Name "f")))
      (EVar (Name "f"))
    )
  ]
