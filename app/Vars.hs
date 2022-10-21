module Vars where

import Data.List (sortBy)
import Data.Char (isDigit, digitToInt, isLetter)

--------------------------------------------------------------------------------

type Vars = [( Char, Int )]

--Function that prints out a Var
tellVars :: Vars -> String
tellVars [] = ""
tellVars [x] | snd x == 0                   = ""
             | snd x == 1                   = "*" ++ [fst x]
             | otherwise                    = "*" ++ [fst x] ++ "^" ++ show (snd x)
tellVars (x:xs) | snd x == 0 = [] ++ tellVars xs
                | snd x == 1 = "*" ++ [fst x] ++ tellVars xs
                | snd x > 1 = "*" ++ [fst x] ++ "^" ++ show (snd x) ++ tellVars xs

--Auxiliary function that compares two Vars (decreasing order)
compareVars :: (Char,Int) -> (Char,Int) -> Ordering
compareVars (x1,y1) (x2,y2) | x1 > x2   = GT
                            | x1 < x2   = LT
                            | otherwise = EQ

--Auxiliary function that sorts Vars, using the compareVars function
sortVars :: Vars -> Vars
sortVars [] = []
sortVars x = sortBy(\(x1,y1) (x2,y2) -> compareVars (x1,y1) (x2,y2)) x

--Function that Normalizes the variables of a monomial -> sorts the
--elements and joins the ones that have the same variable (and sums
--their degrees)
normalizeVars :: Vars -> Vars
normalizeVars [] = []
normalizeVars [x] | snd x == 0 = [('_',0)] --When the Degree of the variable is 0
                  | otherwise = [x]
normalizeVars (x:y:xs) | fst x1 == fst y1 = normalizeVars ((fst x1, snd x1 + snd y1) : xs1)
                       | otherwise = x1 : y1 : normalizeVars xs1
                       where (x1:y1:xs1) = sortVars (x:y:xs)

--Auxiliary function that parses a string into an element of Vars
findAuxVars :: [Char] -> (Char,Int)
findAuxVars x | length x == 1 = (head x,1) --Degree == 1
              | degree == 0 = ('_',degree) --Monomial with only the Coeficient
              | otherwise = (var,degree)
              where var = head x
                    degree = digitToInt (last x)


--Function that finds the Vars of Monomial in a String
findVars :: [Char] -> Vars
findVars [] = []
findVars x    | onlyDigitsMoni == x                  = [('_',0)] --Monomial that only has a coeficient
              | (isDigit $ head x) || head x == '-'    = findAuxVars digitAux : findVars digitNext --Find the Vars of a Monomial whose Coeficient /= 1
              | otherwise                            = findAuxVars aux : findVars next --Find the Vars of a Monomial whose Coeficient == 1
              where filteredMoni = drop 1 (dropWhile (/= '*') x) --Remove the Coeficient
                    aux = takeWhile (/= '*') x --First Var
                    digitAux = takeWhile (/= '*') filteredMoni --First Var (Coeficient /= 1)
                    next = drop 1 (dropWhile (/= '*') x ) --Next element
                    digitNext = drop 1 (dropWhile (/= '*') filteredMoni) --Next element (Coeficient /= 1)
                    onlyDigitsMoni = filter (\n -> isDigit n || n == '-') x --To check if a Monomial only has digits

--------------------------------------------------------------------------------
