module Moni where
import Data.Char (isDigit, digitToInt, isLetter)
import Vars

data Moni = Moni { coef :: Int, vars :: Vars} deriving (Ord, Eq)

instance Show Moni where
   show (Moni {coef = c, vars = v})     | c == 1                      = drop 1 aux
                                        | c < 0 && (aux /= "" )       = "(" ++ show c ++ ")" ++ aux
                                        | c < 0 && (aux == "")        = "(" ++ show c ++ ")"
                                        | c > 0 && (aux == "")        = show c
                                        | otherwise                   = show c ++ aux
                                        where aux = tellVars v


--List of Degrees of a Monomial
degree :: Moni -> [Int]
degree (Moni x l) = [snd y | y <- l]

--List of Variables of a Monomial
variable :: Moni -> [Char]
variable (Moni x l) = [fst y | y <- l]

--Coeficient of a Monomial
coeficient :: Moni -> Int
coeficient (Moni x l) = x

--Auxiliary function that compares two Monomials (decreasing order)
--First checks the degree, then the coeficient and finally the variable
compareMoni :: Moni -> Moni -> Ordering
compareMoni x1 x2    | degX1 > degX2                    = LT
                     | degX1 < degX2                    = GT
                     | coefX1 < coefX2                  = LT
                     | coefX1 > coefX2                  = GT
                     | varX1 < varX2                    = LT
                     | varX1 > varX2                    = GT
                     | otherwise                        = EQ
                     where degX1  = maximum (degree x1)
                           degX2  = maximum (degree x2)
                           coefX1 = coeficient x1
                           coefX2 = coeficient x2
                           varX1  = maximum (variable x1)
                           varX2  = maximum (variable x2)

--Function that Normalizes a Monomial
normalizeMoni :: Moni -> Moni
normalizeMoni (Moni coef vars) = Moni coef (normalizeVars vars)

--Function that Sums two Monomials
sumMoni :: Moni -> Moni -> Moni
sumMoni (Moni x1 []) (Moni x2 []) = (Moni (x1+x2) [])
sumMoni (Moni x1 vars1) (Moni x2 vars2) = (Moni (x1 + x2) vars1)

--Function that removes the '*' and the rest of the unnecessary chars from the Monomial
filterMoni :: [Char] -> [Char]
filterMoni = filter (\x -> isDigit x || isLetter x || x == '-')

--find the Coeficient of the Monomial
findCoef :: [Char] -> [Char]
findCoef x = [y | y <- takeWhile (\n -> n == '-' || isDigit n ) x ]

--parses a list of char digits to an int
parseNum :: [Char] -> Int
parseNum [] = 0
parseNum l | head l /= '-' = foldl (\acc x -> (if acc == 0 then acc + digitToInt x else acc * 10 + digitToInt x)) 0 l --positive number
           | otherwise = (-1) * (foldl (\acc x -> (if acc == 0 then acc + digitToInt x else acc * 10 + digitToInt x)) 0 (tail l)) --negative number

--parses a string of monomial to the monomial data type
monomial :: [Char] -> Moni
monomial x | (length aux == 1) && isLetter (head x)          = Moni 1 [(head x,1)]
           | (length aux == 1) && isDigit (head x)           = Moni coef [('_',0)]
           | null coefAux && vars /= []                      = Moni 1 vars
           | null coefAux && vars == []                      = Moni 1 [('_',0)]
           | not (null coefAux) && vars == []                = Moni coef [('_',0)]
           | otherwise                                       = Moni coef vars
           where coef     = parseNum coefAux
                 aux      = filterMoni x
                 coefAux  = findCoef aux
                 vars     = filter (\(y,z) -> z /= 0) (findVars x)
