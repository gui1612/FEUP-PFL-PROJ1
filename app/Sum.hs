module Sum where

import Poli

sumAuxPoli :: Poli -> Poli -> Poli
sumAuxPoli [] [] = []
sumAuxPoli (x:xs) (y:ys) = internalSum (merge (x:xs) (y:ys))


sumPoli :: Poli -> Poli -> Poli
sumPoli [] [] = []
sumPoli (x:xs) (y:ys) = (sumAuxPoli (x:xs) (y:ys))
