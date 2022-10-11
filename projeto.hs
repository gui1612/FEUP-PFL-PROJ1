{-# LANGUAGE BlockArguments #-}
import Data.Char (isDigit, digitToInt, isLetter)
import Data.List (sortBy)

--module Main where

--  main :: IO()
--  main = do
--    poli <- getLine
--    putStrLn poli
--------------------------------------------------------------------------------
--General functions, data and a)

data Moni = Moni { coef :: Int, vars :: [(Char,Int)]} deriving (Show, Ord, Eq)
type Poli = [Moni]

tellVars :: [(Char,Int)] -> String
tellVars [] = ""
tellVars [x] | snd x == 0                   = ""
             | snd x == 1                   = [fst x]
             | otherwise                    = [fst x] ++ "^" ++ show (snd x)
tellVars (x:xs) = "(" ++ [fst x] ++ "^" ++ show (snd x) ++ ")*" ++ tellVars xs --acontece 2*(x^2)*(y)*z por ex.

tellMoni :: Moni -> String
tellMoni (Moni {coef = c, vars = v}) | c == 0                       = "0"
                                     | c == 1                       = tellVars v
                                     | c < 0 && (tellVars v /= "" ) = "(" ++ show c ++ ")*" ++ tellVars v
                                     | c < 0 && (tellVars v == "")  = "(" ++ show c ++ ")"
                                     | c > 0 && (tellVars v == "")  = show c
                                     | otherwise                    = show c ++ "*" ++ tellVars v

tellPoli :: Poli -> String
tellPoli [] = ""
tellPoli [x] | coeficient x == 0        = ""
             | otherwise                = tellMoni x
tellPoli [x,y] | coeficient y == 0      = tellMoni x
               | coeficient x == 0      = tellMoni y
               | otherwise              = tellMoni x ++ " + " ++ tellMoni y
tellPoli (x:xs) | coeficient x == 0     = "" ++ tellPoli xs
                | otherwise             = tellMoni x ++ " + " ++ tellPoli xs

compareMoni :: Moni -> Moni -> Ordering
compareMoni (Moni x1 [(y1, z1)]) (Moni x2 [(y2,z2)]) | z1 > z2                               = LT
                                                     | z1 < z2                               = GT
                                                     | (z1 == z2) && (y1 > y2)               = LT
                                                     | (z1 == z2) && (y1 < y2)               = GT
                                                     | (z1 == z2) && (y1 == y2) && (x1 > x2) = LT
                                                     | (z1 == z2) && (y1 == y2) && (x1 < x2) = GT
                                                     | otherwise                             = EQ

compareVars :: (Char,Int) -> (Char,Int) -> Ordering
compareVars (x1,y1) (x2,y2) | x1 > x2 = GT
                            | x1 < x2 = LT
                            | otherwise = EQ

degree :: Moni -> Int
degree (Moni _ [(_,x)]) = x

variable :: Moni -> Char
variable (Moni _ [(x,_)]) = x

coeficient :: Moni -> Int
coeficient (Moni x [(_,_)]) = x

mergePoli :: Poli -> Poli -> Poli
mergePoli [] [] = []
mergePoli x [] = x
mergePoli [] x = x
mergePoli (x:xs) (y:ys) = x : y : mergePoli xs ys


sortPoli :: Poli -> Poli
sortPoli [] = []
sortPoli x = sortBy (\(Moni x1 y1) (Moni x2 y2) -> compareMoni (Moni x1 y1) (Moni x2 y2)) x


internalSum :: Poli -> Poli
internalSum [] = []
internalSum [x] = [x]
internalSum (x:y:xs) | (variable x == variable y) && (degree x == degree y) = internalSum (sumMoni x y : xs)
                     | otherwise = x : internalSum (y:xs)

normalizePoli :: Poli -> String
normalizePoli []  = []
normalizePoli [x] = tellMoni x
normalizePoli x   = tellPoli (internalSum (sortPoli x))


--------------------------------------------------------------------------------
--b)

sumMoni :: Moni -> Moni -> Moni
sumMoni (Moni x1 [(y1, z1)]) (Moni x2 [(y2,z2)])    | (y1 == y2) && (z1 == z2) = (Moni (x1 + x2) [(y1,z1)])
                                                    | otherwise = error "Couldn't sum these monomials"

sumAuxPoli :: Poli -> Poli -> Poli
sumAuxPoli (x:xs) (y:ys) = internalSum (mergePoli (x:xs) (y:ys))


sumPoli :: Poli -> Poli -> String
sumPoli (x:xs) (y:ys) = tellPoli (sumAuxPoli (x:xs) (y:ys))
--------------------------------------------------------------------------------
--c)



prodAuxVars :: (Char,Int) -> (Char,Int) -> (Char,Int)
prodAuxVars (x1,y1) (x2,y2) | x1 == x2 = (x1,y1+y2)

prodVars :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)]
prodVars p1 p2 = [ prodAuxVars (x1,y1) (x2,y2) | (x1,y1) <- p1, (x2,y2) <- p2, x1 == x2]

prodMoni :: Moni -> Moni -> Moni
prodMoni (Moni x1 vars1) (Moni x2 vars2) = (Moni (x1 * x2) (prodVars vars1 vars2))

prodPoli :: Poli -> Poli -> String
prodPoli [] _  = []
prodPoli _ []  = []
prodPoli l1 l2 = tellPoli (internalSum (sortPoli [prodMoni x y| x <- l1, y <- l2]))

-------------------------------------------------------------------
--d)

derivMoni :: Moni -> Moni
derivMoni (Moni _ [(x,0)]) = Moni 0 [(x,0)]
derivMoni (Moni x [(y,z)]) = Moni (x * z) [(y,z-1)]

derivPoli :: Poli -> Poli
derivPoli [] = []
derivPoli l = [derivMoni x | x <- l]

-------------------------------------------------------------------
--parseString para polinomio


--remove the '*' and the rest of the unnecessary chars
filterMoni :: [Char] -> [Char]
filterMoni = filter (\x -> isDigit x || isLetter x)

--find the coeficient of the monomial
findCoef :: [Char] -> [Int]
findCoef x = [digitToInt y | y <- takeWhile isDigit x]

--finds the degree of the monomial
findDegree :: [Char] -> [Int]
findDegree x = reverse [digitToInt y | y <- takeWhile isDigit (reverse x)]

findVariable :: [Char] -> Char
findVariable x = head (dropWhile (not . isLetter) x)

--parses a list of ints to an int
parseNum :: [Int] -> Int
parseNum = foldl (\acc x -> (if acc == 0 then acc + x else acc * 10 + x)) 0

monomial :: [Char] -> Moni
monomial x | length aux == 1 = Moni coef [('_',0)]
           | length aux == 2 = Moni coef [(variable,1)]
           | otherwise = Moni coef [(variable, degree)]
          where coef = parseNum (findCoef aux)
                variable = findVariable aux
                degree = parseNum (findDegree aux)
                aux = filterMoni x

polinomial :: [Char] -> Poli
polinomial []   = []
polinomial (x:xs) = [monomial ( takeWhile (\n -> n /= ' ') (dropWhile (\n -> (n == ' ') || (n == '+')) (x:xs)))] ++ polinomial (dropWhile (\n -> (n /= ' ') || (n /= '+')) xs)

main = do
  putStrLn "please enter a monomial: "
  input <- getLine
  putStrLn input
