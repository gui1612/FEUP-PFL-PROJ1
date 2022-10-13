module Poli where 

{-# LANGUAGE FlexibleInstances #-}
import Data.List (sortBy)
import Vars


data Moni = Moni { coef :: Int, vars :: Vars} deriving (Ord, Eq)

-- newtype Poli = Poli { moni :: [Moni] }
type Poli = [Moni]



instance Show Moni where
    show (Moni {coef = c, vars = v})                 | c == 0                       = "0"
                                                     | c == 1                       = vars
                                                     | c < 0 && (vars /= "" )       = "(" ++ show c ++ ")*" ++ vars
                                                     | c < 0 && (vars == "")        = "(" ++ show c ++ ")"
                                                     | c > 0 && (vars == "")        = show c
                                                     | otherwise                    = show c ++ "*" ++ vars
                                                     where vars = show v

-- tellMoni :: Moni -> String
-- tellMoni (Moni {coef = c, vars = v}) | c == 0                       = "0"
--                                                      | c == 1                       = vars
--                                                      | c < 0 && (vars /= "" )       = "(" ++ show c ++ ")*" ++ vars
--                                                      | c < 0 && (vars == "")        = "(" ++ show c ++ ")"
--                                                      | c > 0 && (vars == "")        = show c
--                                                      | otherwise                    = show c ++ "*" ++ vars
--                                                      where vars = tellVars v

degree :: Moni -> [Int]
degree (Moni x l) = [snd y | y <- l]

variable :: Moni -> [Char]
variable (Moni x l) = [fst y | y <- l]

coeficient :: Moni -> Int
coeficient (Moni x l) = x

merge :: [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = x : y : merge xs ys

sortPoli :: Poli -> Poli
sortPoli [] = []
sortPoli x = sortBy (\(Moni x1 y1) (Moni x2 y2) -> compareMoni (Moni x1 y1) (Moni x2 y2)) x

-- instance Show Poli where
--     show [] = ""
--     show [x] | coeficient x == 0            = ""
--                 | otherwise                 = tellMoni x

--     show (x:xs)     | coeficient a == 0         = "" ++ tellPoli ab
--                     | otherwise             = show a ++ " + " ++ show ab
--                 where (a:ab) = sortPoli (x:xs)

tellPoli :: Poli -> String
-- tellPoli [] = ""
-- tellPoli

tellPoli [] = ""
tellPoli [x] | coeficient x == 0        = ""
             | otherwise                = show x

tellPoli (x:xs) | coeficient a == 0     = "" ++ tellPoli ab
                | otherwise             = show a ++ " + " ++ tellPoli ab
               where (a:ab) = sortPoli (x:xs)


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
