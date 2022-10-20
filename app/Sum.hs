module Sum where

import Poli

--------------------------------------------------------------------------------

--Auxiliary function that merges two polinomials and does its internal sum
sumAuxPoli :: Poli -> Poli -> Poli
sumAuxPoli [] [] = []
sumAuxPoli (x:xs) (y:ys) = internalSum (merge (x:xs) (y:ys))

--Function for doing the Sum of two polinomials
sumPoli :: Poli -> Poli -> Poli
sumPoli [] [] = []
sumPoli (x:xs) (y:ys) = (sumAuxPoli (x:xs) (y:ys))

--------------------------------------------------------------------------------
