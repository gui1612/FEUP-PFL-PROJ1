{-# LANGUAGE BlockArguments #-}
import Data.Char (isDigit, digitToInt, isLetter)

--module Main where

--  main :: IO()
--  main = do
--    poli <- getLine
--    putStrLn poli

data Moni = Moni Int (Char,Int)
type Poli = [Moni]

degree :: Moni -> Int
degree (Moni _ (_,x)) = x

--remove the '*' and the rest of the unnecessary chars
filterMoni :: [Char] -> [Char]
filterMoni = filter (\x -> isDigit x || isLetter x)

--find the coeficient of the monomial
findCoef :: [Char] -> [Int]
findCoef x = [digitToInt y | y <- takeWhile isDigit x]

--find the degree of the monomial
findDegree :: [Char] -> [Int]
findDegree x = reverse [digitToInt y | y <- takeWhile isDigit (reverse x)]

findVariable :: [Char] -> Char
findVariable x = head (dropWhile isDigit x)


--parse a list of ints to an int
parseNum :: [Int] -> Int
parseNum = foldl (\acc x -> (if acc == 0 then acc + x else acc * 10 + x)) 0

monimial :: [Char] -> Moni
monimial x = Moni coef (variable, degree)
          where coef = parseNum (findCoef aux)
                variable = findVariable aux
                degree = parseNum (findDegree aux)
                aux = filterMoni aux

main = do
  putStrLn "please enter a monomial: "
  input <- getLine
  putStrLn input
