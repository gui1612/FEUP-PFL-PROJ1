module Poli where

{-# LANGUAGE FlexibleInstances #-}

import Data.List (sortBy)
import Moni

-- newtype Poli = Poli { moni :: [Moni] }
type Poli = [Moni]


merge :: [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = x : y : merge xs ys

sortPoli :: Poli -> Poli
sortPoli [] = []
sortPoli x = sortBy (\(Moni x1 y1) (Moni x2 y2) -> compareMoni (Moni x1 y1) (Moni x2 y2)) x

tellPoli :: Poli -> String
tellPoli [] = ""
tellPoli [x] | coeficient x == 0        = ""
             | otherwise                = show x
tellPoli (x:xs) | coeficient a == 0     = "" ++ tellPoli ab
                | otherwise             = show a ++ " + " ++ tellPoli ab
               where (a:ab) = sortPoli (x:xs)

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
normalizePoli l = internalSum aux
                    where aux = normalizeAuxPoli l

-- Parses a polinomial from a string
polinomial :: [Char] -> Poli
polinomial []   = []
polinomial (x:xs) | first == '-'                = monomial (first : aux) : polinomial next
               | otherwise                   = monomial aux :  polinomial next
               where filteredString = filter (/= ' ') (x:xs)
                     first = head filteredString
                     aux = takeWhile (\n -> n /='+' && n /= '-') filteredString
                     next = drop 1 (dropWhile (\n -> n /= '+' && n /= '-') filteredString)