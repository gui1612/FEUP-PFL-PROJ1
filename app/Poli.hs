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

--Auxiliary function that sorts a Polinomial
sortPoli :: Poli -> Poli
sortPoli [] = []
sortPoli x = sortBy (\(Moni x1 y1) (Moni x2 y2) -> compareMoni (Moni x1 y1) (Moni x2 y2)) x

--Auxiliary that transforms a Polinomial into a String
tellPoliAux :: Poli -> String
tellPoliAux [] = ""
tellPoliAux [x] | coeficient x == 0        = ""  --Polinomial with only one member that has 0 as the coeficient
                | otherwise                = show x                  
tellPoliAux (x:xs) | coeficient a == 0     = "" ++ next              
                   | next == ""     = show a --Polinomial with only one member that has 0 as the coeficient
                   | otherwise             = show a ++ " + " ++ next 
                   where (a:ab) = sortPoli (x:xs) --Sorted Polinomial
                         next = tellPoliAux ab

--Function that transforms a Polinomial into a String
tellPoli :: Poli -> String
tellPoli [] = ""
tellPoli l | sumCoefs l == 0 = "0"
           | otherwise = tellPoliAux l

--Auxiliary function that Sums the coeficients of the members of the Polinomial
sumCoefs :: Poli -> Int
sumCoefs [] =  0
sumCoefs [x] = coeficient x
sumCoefs (x:xs) = coeficient x + sumCoefs xs

--Auxiliary function that sums the Monomials/Elements of the Polinomial
--in order to normalize the Polinomial (sums elements with the same Vars)
internalSum :: Poli -> Poli
internalSum [] = []
internalSum [x] = [x]
internalSum (x:y:xs) | (variable x1 == variable y1) && (degree x1 == degree y1) = internalSum (sumMoni x1 y1 : xs1)
                    | otherwise = x1 : internalSum (y1:xs1)
                    where (x1:y1:xs1) = sortPoli (x:y:xs)

--Auxiliary function that removes the Monomials/Elements that have 0 
--as the coeficient
normalizeAuxPoli :: Poli -> Poli
normalizeAuxPoli [] = []
normalizeAuxPoli (x:xs) | coeficient aux == 0   = normalizeAuxPoli xs
                        | otherwise             = aux : normalizeAuxPoli xs
                       where aux               = normalizeMoni x

--Function that Normalizes a Polinomial
normalizePoli :: Poli -> Poli
normalizePoli []  = []
normalizePoli [x] | coeficient x == 0 = [] --Polinomial with only one element and has Coeficient 0
                  | otherwise = [normalizeMoni x]
normalizePoli l  = internalSum aux
                    where aux = normalizeAuxPoli l

-- Parses a polinomial from a string
polinomial :: [Char] -> Poli
polinomial []   = []
polinomial (x:xs) | first == '-'                = monomial (first : auxSign) : polinomial nextSign --Negative element of the Polinomial
                  | first == '+'                = monomial auxSign : polinomial nextSign --Positive element of the Polinomial (not the first)
                  | otherwise                   = monomial aux :  polinomial next --Positive First element of the Polinomial 
                  where filteredString = filter (/= ' ') (x:xs) --removes the Spaces
                        first = head filteredString 
                        aux = takeWhile (\n -> n /='+' && n /= '-') filteredString --First element of the Polinomial
                        auxSign = takeWhile (\n -> n /='+' && n /= '-') (drop 1 filteredString) --First element of the Polinomial (starts with a sign - '-' or '+')
                        next = dropWhile (\n -> n /= '+' && n /= '-') filteredString --Next element of the Polinomial
                        nextSign = dropWhile (\n -> n /= '+' && n /= '-') (drop 1 filteredString) --Next element of the Polinomial (starts with a sign - '-' or '+')

--------------------------------------------------------------------------------
