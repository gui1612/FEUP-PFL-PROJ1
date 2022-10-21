module Poli where

{-# LANGUAGE FlexibleInstances #-}

import Data.List (sortBy)
import Moni

type Poli = [Moni]

--------------------------------------------------------------------------------

--Auxiliary function that merges two list into one
merge :: [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = x : y : merge xs ys

--Auxiliary function that sorts a Polynomial
sortPoli :: Poli -> Poli
sortPoli [] = []
sortPoli x = sortBy (\(Moni x1 y1) (Moni x2 y2) -> compareMoni (Moni x1 y1) (Moni x2 y2)) x

--Auxiliary that transforms a Polynomial into a String
tellPoliAux :: Poli -> String
tellPoliAux [] = ""
tellPoliAux [x] | coeficient x == 0        = ""  --Polynomial with only one member that has 0 as the Coefficient
                | otherwise                = show x
tellPoliAux (x:xs) | coeficient a == 0     = "" ++ next
                   | next == ""     = show a --Polynomial with only one member that has 0 as the Coefficient
                   | otherwise             = show a ++ " + " ++ next
                   where (a:ab) = sortPoli (x:xs) --Sorted Polynomial
                         next = tellPoliAux ab

--Function that transforms a Polynomial into a String
tellPoli :: Poli -> String
tellPoli [] = ""
tellPoli l | sumCoefs l == 0 = "0"
           | otherwise = tellPoliAux l

--Auxiliary function that Sums the Coefficients of the members of the Polynomial
sumCoefs :: Poli -> Int
sumCoefs [] =  0
sumCoefs [x] = coeficient x
sumCoefs (x:xs) = coeficient x + sumCoefs xs

--Auxiliary function that sums the Monomials/Elements of the Polynomial
--in order to normalize the Polynomial (sums elements with the same Vars)
internalSum :: Poli -> Poli
internalSum [] = []
internalSum [x] = [x]
internalSum (x:y:xs) | (variable x1 == variable y1) && (degree x1 == degree y1) = internalSum (sumMoni x1 y1 : xs1)
                     | otherwise = x1 : internalSum (y1:xs1)
                     where (x1:y1:xs1) = sortPoli (x:y:xs)

--Auxiliary function that removes the Monomials/Elements that have 0
--as the Coefficient
normalizeAuxPoli :: Poli -> Poli
normalizeAuxPoli [] = []
normalizeAuxPoli (x:xs) = aux : normalizeAuxPoli xs
                       where aux = normalizeMoni x

--Function that Normalizes a Polynomial
normalizePoli :: Poli -> Poli
normalizePoli []  = []
normalizePoli [x] | coeficient x == 0 = [(Moni 0 [('_',0)])] --Polynomial with only one element and has Coefficient 0
                  | otherwise = [normalizeMoni x]
normalizePoli l | filterAux == [] = [(Moni 0 [('_',0)])] --After being filtered, if the list becomes empty, it means that the result of the normalization is 0
                | otherwise = filterAux
                where aux       = normalizeAuxPoli l
                      sumAux    = internalSum aux --Internal sum of the polynomial
                      filterAux = filter (\x -> coeficient x /= 0) sumAux --Filters the elements of the polynomial that have coefficient 0