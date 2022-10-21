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

--Auxiliary function that sorts a polinomial
sortPoli :: Poli -> Poli
sortPoli [] = []
sortPoli x = sortBy (\(Moni x1 y1) (Moni x2 y2) -> compareMoni (Moni x1 y1) (Moni x2 y2)) x

--Auxiliary function that
tellPoliAux :: Poli -> String
tellPoliAux [] = ""
tellPoliAux [x] | coeficient x == 0        = ""
                | otherwise                = show x
tellPoliAux (x:xs) | coeficient a == 0     = "" ++ next
                   | next == ""     = show a
                   | otherwise             = show a ++ " + " ++ next
                   where (a:ab) = sortPoli (x:xs)
                         next = tellPoliAux ab

tellPoli :: Poli -> String
tellPoli [] = ""
tellPoli l | sumCoefs l == 0 = "0"
           | otherwise = tellPoliAux l


sumCoefs :: Poli -> Int
sumCoefs [] =  0
sumCoefs [x] = coeficient x
sumCoefs (x:xs) = coeficient x + sumCoefs xs

internalSum :: Poli -> Poli
internalSum [] = []
internalSum [x] = [x]
internalSum (x:y:xs) | (variable x1 == variable y1) && (degree x1 == degree y1) = internalSum (sumMoni x1 y1 : xs1)
                    | otherwise = x1 : internalSum (y1:xs1)
                    where (x1:y1:xs1) = sortPoli (x:y:xs)

normalizeAuxPoli :: Poli -> Poli
normalizeAuxPoli [] = []
normalizeAuxPoli (x:xs) | coeficient aux == 0   = normalizeAuxPoli xs
                        | otherwise             = aux : normalizeAuxPoli xs
                       where aux               = normalizeMoni x

normalizePoli :: Poli -> Poli
normalizePoli []  = []
normalizePoli [x] | coeficient x == 0 = []
                  | otherwise = [normalizeMoni x]
normalizePoli l  = internalSum aux
                    where aux = normalizeAuxPoli l

-- Parses a polinomial from a string
polinomial :: [Char] -> Poli
polinomial []   = []
polinomial (x:xs) | first == '-'                = monomial (first : auxSign) : polinomial nextSign
                  | first == '+'                = monomial auxSign : polinomial nextSign
                  | otherwise                   = monomial aux :  polinomial next
                  where filteredString = filter (/= ' ') (x:xs)
                        first = head filteredString
                        aux = takeWhile (\n -> n /='+' && n /= '-') filteredString
                        auxSign = takeWhile (\n -> n /='+' && n /= '-') (drop 1 filteredString)
                        next = dropWhile (\n -> n /= '+' && n /= '-') filteredString
                        nextSign = dropWhile (\n -> n /= '+' && n /= '-') (drop 1 filteredString)

--------------------------------------------------------------------------------
