module Moni where
import Data.Char (isDigit, digitToInt, isLetter)
import Vars

data Moni = Moni { coef :: Int, vars :: Vars} deriving (Ord, Eq)

instance Show Moni where
    show (Moni {coef = c, vars = v})                 | c == 0                       = "0"
                                                     | c == 1                       = vars
                                                     | c < 0 && (vars /= "" )       = "(" ++ show c ++ ")*" ++ vars
                                                     | c < 0 && (vars == "")        = "(" ++ show c ++ ")"
                                                     | c > 0 && (vars == "")        = show c
                                                     | otherwise                    = show c ++ "*" ++ vars
                                                     where vars = show v


degree :: Moni -> [Int]
degree (Moni x l) = [snd y | y <- l]

variable :: Moni -> [Char]
variable (Moni x l) = [fst y | y <- l]

coeficient :: Moni -> Int
coeficient (Moni x l) = x


-- TODO: !!Refactor!! (Ternary and use aux functions)
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

normalizeMoni :: Moni -> Moni
normalizeMoni (Moni coef vars) = Moni coef (normalizeVars vars)

sumMoni :: Moni -> Moni -> Moni
sumMoni (Moni x1 [(y1, z1)]) (Moni x2 [(y2,z2)])    | (y1 == y2) && (z1 == z2) = (Moni (x1 + x2) [(y1,z1)])
                                                    | otherwise = error "Couldn't sum these monomials"

--remove the '*' and the rest of the unnecessary chars
filterMoni :: [Char] -> [Char]
filterMoni = filter (\x -> isDigit x || isLetter x)

--find the coeficient of the monomial
findCoef :: [Char] -> [Int]
findCoef x = [digitToInt y | y <- takeWhile isDigit x]

--parses a list of ints to an int
parseNum :: [Int] -> Int
parseNum = foldl (\acc x -> (if acc == 0 then acc + x else acc * 10 + x)) 0

monomial :: [Char] -> Moni
monomial x | length aux == 1            = Moni coef [('_',0)]
        | otherwise                  = Moni coef (findVars x)
       where coef     = parseNum (findCoef aux)
             aux      = filterMoni x
