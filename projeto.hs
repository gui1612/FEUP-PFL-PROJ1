{-# LANGUAGE BlockArguments #-}
import Data.Char (isDigit, digitToInt, isLetter)
import Data.List (sortBy)
import Data.Text (splitOn)

--module Main where

--  main :: IO()
--  main = do
--    poli <- getLine
--    putStrLn poli
--------------------------------------------------------------------------------
--General functions, data and a)

data Moni = Moni { coef :: Int, vars :: [(Char,Int)]} deriving (Show, Ord, Eq)
type Poli = [Moni]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
            where (w, s'') = break p s'


tellVars :: [(Char,Int)] -> String
tellVars [] = ""
tellVars [x] | snd x == 0                   = ""
             | snd x == 1                   = [fst x]
             | otherwise                    = [fst x] ++ "^" ++ show (snd x)
tellVars (x:xs) | snd x == 1 = [fst x] ++ "*" ++ tellVars xs --acontece 2*(x^2)*(y)*z por ex.
                | snd x > 1 = [fst x] ++ "^" ++ show (snd x) ++ "*" ++ tellVars xs

tellMoni :: Moni -> String
tellMoni (Moni {coef = c, vars = v}) | c == 0                       = "0"
                                     | c == 1                       = vars
                                     | c < 0 && (vars /= "" )       = "(" ++ show c ++ ")*" ++ vars
                                     | c < 0 && (vars == "")        = "(" ++ show c ++ ")"
                                     | c > 0 && (vars == "")        = show c
                                     | otherwise                    = show c ++ "*" ++ vars
                                     where vars = tellVars v

tellPoli :: Poli -> String
-- tellPoli [] = ""
-- tellPoli


tellPoli [] = ""
tellPoli [x] | coeficient x == 0        = ""
             | otherwise                = tellMoni x
tellPoli [x,y] | coeficient b == 0      = tellMoni a
               | coeficient a == 0      = tellMoni b
               | otherwise              = tellMoni a ++ " + " ++ tellMoni b
               where [a, b] = sortPoli [x, y]
tellPoli (x:xs) | coeficient a == 0     = "" ++ tellPoli ab
                | otherwise             = tellMoni a ++ " + " ++ tellPoli ab
               where (a:ab) = sortPoli (x:xs)

-- TODO: !!Refactor!! (Ternary and use aux functions)
compareMoni :: Moni -> Moni -> Ordering
compareMoni x1 x2    | degX1 > degX2                    = LT
                     | degX1 < degX2                    = GT
                     | coefX1 < coefX2                  = LT
                     | coefX1 > coefX2                  = GT
                     | varX1 < varX2                    = LT
                     | varX1 < varX2                    = GT
                     | otherwise                        = EQ
                     where degX1  = degree x1
                           degX2  = degree x2
                           coefX1 = coeficient x1
                           coefX2 = coeficient x2
                           varX1  = variable x1
                           varX2  = variable x2

-- compareMoni (Moni x1 [(y1, z1)]) (Moni x2 [(y2,z2)]) | z1 > z2                               = LT
--                                                      |


--                                                      | z1 > z2                               = LT
--                                                      | z1 < z2                               = GT
--                                                      | (z1 == z2) && (y1 > y2)               = LT
--                                                      | (z1 == z2) && (y1 < y2)               = GT
--                                                      | (z1 == z2) && (y1 == y2) && (x1 > x2) = LT
--                                                      | (z1 == z2) && (y1 == y2) && (x1 < x2) = GT
--                                                      | otherwise                             = EQ

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

-- TODO: Graus sao digitos depois de '^'

-- Tirar coeficiente
-- Chegar até ao 1o grau
-- Passar para o proximo elemento
findAuxVars :: [Char] -> (Char,Int)
findAuxVars x | length x == 1 = (head x,1)
              | otherwise = (var,digitToInt degree)
              where var = head x
                    degree = last x

findVars :: [Char] -> [(Char,Int)]
findVars [] = []
findVars x    | isDigit (head x)          = (findAuxVars digitAux) : (findVars digitNext)
              | otherwise                 = (findAuxVars aux) : (findVars next)
              where filteredMoni = tail (dropWhile (/= '*') x)
                    aux = takeWhile (\n -> n /= '*') x
                    digitAux = takeWhile (\n -> n /= '*') filteredMoni
                    next = drop 1 (dropWhile (\n -> n /= '*') x )
                    digitNext = drop 1 (dropWhile (\n -> n /= '*') filteredMoni)


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
polinomial (x:xs) | first == '-'                = (monomial (first : aux)) : polinomial next
                  | otherwise                   = (monomial aux) :  polinomial next
                  where filteredString = filter (\n -> (n /= ' ')) (x:xs)
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
poli_ex = [a, b, c, d, e, f, g]


main = do
  putStrLn "please enter a monomial: "
  input <- getLine
  putStrLn input
