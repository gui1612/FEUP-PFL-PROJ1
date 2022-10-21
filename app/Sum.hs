module Sum where

import Poli

--------------------------------------------------------------------------------

--Function for doing the Sum of two polinomials
sumPoli :: Poli -> Poli -> Poli
sumPoli [] [] = []
sumPoli (x:xs) (y:ys) = internalSum (merge (x:xs) (y:ys))

--------------------------------------------------------------------------------
