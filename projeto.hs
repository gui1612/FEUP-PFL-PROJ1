{-# LANGUAGE BlockArguments #-}
import Data.Char (isDigit, digitToInt, isLetter)

--module Main where

--  main :: IO()
--  main = do
--    poli <- getLine
--    putStrLn poli

data Moni = Moni Int [(Char,Int)] deriving (Show)
type Poli = [Moni]

degree :: Moni -> Int
degree (Moni _ [(_,x)]) = x

variable :: Moni -> Char
variable (Moni _ [(x,_)]) = x

coeficient :: Moni -> Int
coeficient (Moni x [(_,_)]) = x

sumMoni :: Moni -> Moni -> Moni
sumMoni (Moni x1 [(y1, z1)]) (Moni x2 [(y2,z2)]) | (y1 == y2) && (z1 == z2) = (Moni (x1 + x2) [(y1,z1)])
                                             | otherwise = error "Couldn't sum these monomials"

sumPoli :: Poli -> Poli -> Poli
sumPoli [] [] = []
sumPoli [] x = x
sumPoli x [] = x
sumPoli (x:xs) (y:ys) | (variable x == variable y) && (degree x == degree y) = [sumMoni x y] ++ sumPoli xs ys
                      | otherwise = sumPoli (x:xs) ys ++ sumPoli xs (y:ys)

prodMoni :: Moni -> Moni -> Moni
prodMoni (Moni x1 [(y1, z1)]) (Moni x2 [(y2,z2)]) | y1 == y2 = (Moni (x1 * x2) [(y1,z1 + z2)])
                                                  | otherwise = (Moni (x1 * x2) [(y1,z1),(y2,z2)])

prodPoli :: Poli -> Poli -> Poli
prodPoli [] _ = []
prodPoli _ [] = []
prodPoli l1 l2 = [prodMoni x y| x <- l1, y <- l2]


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
findVariable x = head (dropWhile (\n -> not (isLetter n)) x)

--parse a list of ints to an int
parseNum :: [Int] -> Int
parseNum = foldl (\acc x -> (if acc == 0 then acc + x else acc * 10 + x)) 0

monimial :: [Char] -> Moni
monimial x = (Moni coef [(variable, degree)])
          where coef = parseNum (findCoef aux)
                variable = findVariable aux
                degree = parseNum (findDegree aux)
                aux = filterMoni aux

main = do
  putStrLn "please enter a monomial: "
  input <- getLine
  putStrLn input
