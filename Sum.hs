module Sum where

import Poli

sumAuxPoli :: Poli -> Poli -> Poli
sumAuxPoli [] [] = []
sumAuxPoli (x:xs) (y:ys) = internalSum (merge (x:xs) (y:ys))


sumPoli :: Poli -> Poli -> String
sumPoli [] [] = []
sumPoli (x:xs) (y:ys) = tellPoli (sumAuxPoli (x:xs) (y:ys))
