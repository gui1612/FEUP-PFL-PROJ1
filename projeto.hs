{-# LANGUAGE BlockArguments #-}
import Data.Char (isDigit, digitToInt, isLetter)
import Data.Text (splitOn)

import Sum
import Poli
import Vars
--module Main where

--  main :: IO()
--  main = do
--    poli <- getLine
--    putStrLn poli
--------------------------------------------------------------------------------
--General functions, data and a)



-- compareMoni (Moni x1 [(y1, z1)]) (Moni x2 [(y2,z2)]) | z1 > z2                               = LT
--                                                      |


--                                                      | z1 > z2                               = LT
--                                                      | z1 < z2                               = GT
--                                                      | (z1 == z2) && (y1 > y2)               = LT
--                                                      | (z1 == z2) && (y1 < y2)               = GT
--                                                      | (z1 == z2) && (y1 == y2) && (x1 > x2) = LT
--                                                      | (z1 == z2) && (y1 == y2) && (x1 < x2) = GT
--                                                      | otherwise                             = EQ






normalizeVars :: [(Char,Int)] -> [(Char,Int)]
normalizeVars [] = []
normalizeVars [x] = [x]
normalizeVars (x:y:xs) | fst x1 == fst y1 = (fst x1, snd x1 + snd y1) : normalizeVars xs1
                       | otherwise = x1 : y1 : normalizeVars xs1
                       where (x1:y1:xs1) = sortVars (x:y:xs)

normalizeMoni :: Moni -> Moni
normalizeMoni (Moni coef vars) = Moni coef (normalizeVars vars)

internalSum :: Poli -> Poli
internalSum [] = []
internalSum [x] = [x]
internalSum (x:y:xs) | (variable x1 == variable y1) && (degree x1 == degree y1) = internalSum (sumMoni x1 y1 : xs1)
                     | otherwise = x1 : internalSum (y1:xs1)
                     where (x1:y1:xs1) = sortPoli (x:y:xs)

normalizeAuxPoli :: Poli -> Poli
normalizeAuxPoli (x:xs) | coeficient aux == 0   = normalizeAuxPoli xs
                        | otherwise             = aux : normalizeAuxPoli xs
                        where aux               = normalizeMoni x

normalizePoli :: Poli -> Poli
normalizePoli []  = []
normalizePoli [x] | coeficient x == 0 = []
                  | otherwise = [normalizeMoni x]
normalizePoli l = internalSum aux
                     where aux = normalizeAuxPoli l


--------------------------------------------------------------------------------
--b)

sumMoni :: Moni -> Moni -> Moni
sumMoni (Moni x1 [(y1, z1)]) (Moni x2 [(y2,z2)])    | (y1 == y2) && (z1 == z2) = (Moni (x1 + x2) [(y1,z1)])
                                                    | otherwise = error "Couldn't sum these monomials"

sumAuxPoli :: Poli -> Poli -> Poli
sumAuxPoli (x:xs) (y:ys) = internalSum (merge (x:xs) (y:ys))


sumPoli :: Poli -> Poli -> String
sumPoli [] [] = []
sumPoli (x:xs) (y:ys) = tellPoli (sumAuxPoli (x:xs) (y:ys))
--------------------------------------------------------------------------------
--c)



prodAuxVars :: [(Char,Int)] -> [(Char,Int)]
prodAuxVars [x] = [x]
prodAuxVars (x:y:xs) | (fst x) == (fst y) = prodAuxVars ((fst x,snd x + snd y) : xs)
                     | otherwise = [x] ++ prodAuxVars (y:xs)

prodVars :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)]
prodVars l1 l2 = prodAuxVars (sortVars (merge l1 l2))

prodMoni :: Moni -> Moni -> Moni
prodMoni (Moni x1 vars1) (Moni x2 vars2) = Moni (x1 * x2) (prodVars vars1 vars2)

prodPoli :: Poli -> Poli -> Poli
prodPoli [] _  = []
prodPoli _ []  = []
prodPoli l1 l2 = internalSum (sortPoli [prodMoni x y| x <- l1, y <- l2])

-------------------------------------------------------------------
--d)

--derivAuxMoni :: Char -> [(Char,Int)] -> [(Char,Int)]
--derivAuxMoni _ [] = []
--derivAuxMoni var (x:xs) | fst x == var = x : deri

derivMoni :: Char -> Moni -> Moni
derivMoni var (Moni coef vars ) | null filteredVars = Moni 0 [('_',0)]
                                | otherwise = Moni coefAux [(varAux,degreeAux)]
                                 where filteredVars = filter (\(y,z) ->  y == var) vars
                                       coefAux = coef * snd (head filteredVars)
                                       varAux = fst (head filteredVars)
                                       degreeAux = snd (head filteredVars) - 1

derivPoli :: Char -> Poli -> Poli
derivPoli x [] = []
derivPoli x l = filter (\x -> coeficient x /= 0) [derivMoni x y | y <- l]

-------------------------------------------------------------------
--parseString para polinomio


--remove the '*' and the rest of the unnecessary chars
filterMoni :: [Char] -> [Char]
filterMoni = filter (\x -> isDigit x || isLetter x)

--find the coeficient of the monomial
findCoef :: [Char] -> [Int]
findCoef x = [digitToInt y | y <- takeWhile isDigit x]

--finds the degree of the monomial

-- TODO: Graus sao digitos depois de '^'

-- Tirar coeficiente
-- Chegar até ao 1o grau
-- Passar para o proximo elemento
findAuxVars :: [Char] -> (Char,Int)
findAuxVars x | length x == 1 = (head x,1)
              | otherwise = (var,digitToInt degree)
              where var = head x
                    degree = last x

findVars :: [Char] -> Vars
findVars [] = []
findVars x    | isDigit (head x)          = findAuxVars digitAux : findVars digitNext
              | otherwise                 = findAuxVars aux : findVars next
              where filteredMoni = tail (dropWhile (/= '*') x)
                    aux = takeWhile (/= '*') x
                    digitAux = takeWhile (/= '*') filteredMoni
                    next = drop 1 (dropWhile (/= '*') x )
                    digitNext = drop 1 (dropWhile (/= '*') filteredMoni)


-- findDegree [] = 1
-- findDegree x   | takeWhile (\n -> isDi) Text
--                |
--                |
--                where

findVariable :: [Char] -> Char
findVariable x = head (dropWhile (not . isLetter) x)

--parses a list of ints to an int
parseNum :: [Int] -> Int
parseNum = foldl (\acc x -> (if acc == 0 then acc + x else acc * 10 + x)) 0

monomial :: [Char] -> Moni
monomial x | length aux == 1            = Moni coef [('_',0)]
           | otherwise                  = Moni coef (findVars x)
          where coef     = parseNum (findCoef aux)
                aux      = filterMoni x

-- Parses a polinomial from a string

-- Step 1: Tirar espaços
-- Step 2: Dividir o polinomio numa lista de monomios (retirar '+' e '-' nao iniciais)


polinomial :: [Char] -> Poli
polinomial []   = []
polinomial (x:xs) | first == '-'                = monomial (first : aux) : polinomial next
                  | otherwise                   = monomial aux :  polinomial next
                  where filteredString = filter (/= ' ') (x:xs)
                        first = head filteredString
                        aux = takeWhile (\n -> n /='+' && n /= '-') filteredString
                        next = drop 1 (dropWhile (\n -> n /= '+' && n /= '-') filteredString)

-- Test Cases
a = (Moni 2 [('x', 3)])
b = (Moni (-5) [('x', 3)])
c = (Moni 5 [('y', 2)])
d = (Moni 0 [('x', 2)])
e = (Moni 1 [('x', 1)])
f = (Moni 2 [('x', 1)])
g = (Moni 2 [('x', 0)])
h = (Moni 2 [('x', 2), ('y', 1), ('x', 3)])

poli_ex = [a, b, c, d, e, f, g]


main = do
  putStrLn "please enter a monomial: "
  input <- getLine
  putStrLn input
