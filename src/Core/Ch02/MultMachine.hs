module Core.Ch02.MultMachine
  ( evalMult
  ) where

-- | (n, m, d, t)
type MultState = (Int, Int, Int, Int)

evalMult :: MultState -> [MultState]
evalMult state
  | multFinal state = [state]
  | otherwise       = state : evalMult (stepMult state)

stepMult :: MultState -> MultState
stepMult (n, m, d, t)
  | d > 0     = (n, m, d - 1, t + 1)
  | otherwise = (n, m - 1, n, t)

multFinal :: MultState -> Bool
multFinal (_, m, d, _) = m == 0 && d == 0

-- Ex 2.3:

-- λ> evalMult (2, 3, 0, 0)
-- [(2,3,0,0),(2,2,2,0),(2,2,1,1),(2,2,0,2),(2,1,2,2),(2,1,1,3),(2,1,0,4),(2,0,2,4),(2,0,1,5),(2,0,0,6)]
