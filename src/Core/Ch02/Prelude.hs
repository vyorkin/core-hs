module Core.Ch02.Prelude
  ( defs
  ) where

import Core.Ch02.Language (CoreProgram, Expr(..), Name(..))

-- "standard prelude" for our Core language

-- I x = x
-- K x y = x
-- K1 x y = y
-- S f g x = f x (g x)
-- compose f g x = f (g x)
-- twice f = compose f f

defs :: CoreProgram
defs =
  [ ( "I", ["x"], EVar "x" )
  , ( "K", ["x", "y"], EVar "x" )
  , ( "K1", ["x", "y"], EVar "y" )
  , ( "S", ["f", "g", "x"]
    , EAp
      (EAp (EVar "f") (EVar "x"))
      (EAp (EVar "g") (EVar "x"))
    )
  , ( "nil", [], EConstr 1 0 )
  , ( "cons" , ["x", "y"]
    , EAp
      (EAp (EConstr 2 2) (EVar "x"))
      (EVar "y")
    )
  , ( "compose", ["f", "g", "x"]
    , EAp (EVar "f") (EAp (EVar "g" (EVar "x")))
    )
  , ( "twice", ["f"]
    , EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")
    )
  ]
