module Vars where 

import Data.List (sortBy)

type Vars = [( Char, Int )]

tellVars :: [( Char, Int )] -> String
tellVars [] = ""
tellVars [x] | snd x == 0                   = ""
             | snd x == 1                   = [fst x]
             | otherwise                    = [fst x] ++ "^" ++ show (snd x)
tellVars (x:xs) | snd x == 1 = [fst x] ++ "*" ++ tellVars xs --acontece 2*(x^2)*(y)*z por ex.
                | snd x > 1 = [fst x] ++ "^" ++ show (snd x) ++ "*" ++ tellVars xs

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

