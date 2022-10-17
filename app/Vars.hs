module Vars where

import Data.List (sortBy)
import Data.Char (isDigit, digitToInt, isLetter)

type Vars = [( Char, Int )]

tellVars :: Vars -> String
tellVars [] = ""
tellVars [x] | snd x == 0                   = ""
             | snd x == 1                   = "*" ++ [fst x]
             | otherwise                    = "*" ++ [fst x] ++ "^" ++ show (snd x)
tellVars (x:xs) | snd x == 0 = [] ++ tellVars xs
                | snd x == 1 = "*" ++ [fst x] ++ tellVars xs --acontece 2*(x^2)*(y)*z por ex.
                | snd x > 1 = "*" ++ [fst x] ++ "^" ++ show (snd x) ++ tellVars xs

compareVars :: (Char,Int) -> (Char,Int) -> Ordering
compareVars (x1,y1) (x2,y2) | x1 > x2   = GT
                            | x1 < x2   = LT
                            | otherwise = EQ

sortVars :: Vars -> Vars
sortVars [] = []
sortVars x = sortBy(\(x1,y1) (x2,y2) -> compareVars (x1,y1) (x2,y2)) x

-- instance Show Vars where
--     show []      = ""
--     show [x]     | snd x == 0                   = ""
--                         | snd x == 1                   = [fst x]
--                         | otherwise                    = [fst x] ++ "^" ++ show (snd x)
--     show (x:xs) | snd x == 1 = [fst x] ++ "*" ++ show xs --acontece 2*(x^2)*(y)*z por ex.
--                 | snd x > 1 = [fst x] ++ "^" ++ show (snd x) ++ "*" ++ show xs

normalizeVars :: [(Char,Int)] -> [(Char,Int)]
normalizeVars [] = []
normalizeVars [x] = [x]
normalizeVars (x:y:xs) | fst x1 == fst y1 = normalizeVars ((fst x1, snd x1 + snd y1) : xs1)
                       | otherwise = x1 : y1 : normalizeVars xs1
                       where (x1:y1:xs1) = sortVars (x:y:xs)


-- Tirar coeficiente
-- Chegar até ao 1o grau
-- Passar para o proximo elemento
findAuxVars :: [Char] -> (Char,Int)
findAuxVars x | length x == 1 = (head x,1)
              | degree == 0 = ('_',degree)
              | otherwise = (var,degree)
              where var = head x
                    degree = digitToInt (last x)

findVars :: [Char] -> Vars
findVars [] = []
findVars x    | onlyDigitsMoni == x        = [('_',0)]
              | isDigit $ head x           = findAuxVars digitAux : findVars digitNext
              | otherwise                 = findAuxVars aux : findVars next
              where filteredMoni = drop 1 (dropWhile (/= '*') x)
                    aux = takeWhile (/= '*') x
                    digitAux = takeWhile (/= '*') filteredMoni
                    next = drop 1 (dropWhile (/= '*') x )
                    digitNext = drop 1 (dropWhile (/= '*') filteredMoni)
                    onlyDigitsMoni = filter isDigit x

findVariable :: [Char] -> Char
findVariable x = head (dropWhile (not . isLetter) x)
