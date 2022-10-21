module Prod where

import Poli
import Moni
import Vars

------------------------------------------------------------------------

--Auxiliary function that handles two Vars during
--the Multiplication of two Monomials
prodAuxVars :: Vars -> Vars
prodAuxVars [] = []
prodAuxVars [x] = [x]
prodAuxVars (x:y:xs) | fst x == fst y = prodAuxVars ((fst x,snd x + snd y) : xs) --same variable
                     | snd x == 0 = prodAuxVars ((fst y, snd y) : xs) --degree of x is 0
                     | snd y == 0 = prodAuxVars ((fst x, snd x) : xs) --degree of y is 0
                     | otherwise = x : prodAuxVars (y:xs) -- x and y don't have the same variable nor degree

--Function that merges two lists of Vars, sorts the
--resulted list and calls an auxiliary function
prodVars :: Vars -> Vars -> Vars
prodVars l1 l2 = prodAuxVars (sortVars (merge l1 l2))

--Function that Multiplicates two monomials
prodMoni :: Moni -> Moni -> Moni
prodMoni (Moni x1 vars1) (Moni x2 vars2) = Moni (x1 * x2) (prodVars vars1 vars2)

--Function that Multiplicates two polinomials, sorts the resulting polinomial,
--does its internal sum (normalizes it) and removes the members that have
--0 as a coeficient
prodPoli :: Poli -> Poli -> Poli
prodPoli [] _  = []
prodPoli _ []  = []
prodPoli l1 l2 | aux == [] = [(Moni 0 [('_',0)])]
               | otherwise = aux
               where aux = normalizePoli ([prodMoni x y| x <- l1, y <- l2])

--------------------------------------------------------------------------------
