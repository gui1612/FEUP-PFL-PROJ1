module Prod where

import Poli
import Moni
import Vars

prodAuxVars :: Vars -> Vars
prodAuxVars (x:y:xs) | fst x == fst y = prodAuxVars ((fst x,snd x + snd y) : xs)
                     | otherwise = x : prodAuxVars (y:xs)

prodVars :: Vars -> Vars -> Vars
prodVars l1 l2 = prodAuxVars (sortVars (merge l1 l2))

prodMoni :: Moni -> Moni -> Moni
prodMoni (Moni x1 vars1) (Moni x2 vars2) = Moni (x1 * x2) (prodVars vars1 vars2)

prodPoli :: Poli -> Poli -> Poli
prodPoli [] _  = []
prodPoli _ []  = []
prodPoli l1 l2 = filter (\x -> coeficient x /= 0) (internalSum (sortPoli [prodMoni x y| x <- l1, y <- l2]))
