module DataType where
data Moni = Moni { coef :: Int, vars :: [(Char,Int)]} deriving (Show, Ord, Eq)
type Poli = [Moni]

maxElem :: Ord a => [a] -> a
maxElem [x] = x
maxElem (x:y:xs) = maxElem ((if x >= y then x else y):xs)

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
