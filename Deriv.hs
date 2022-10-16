module Deriv where

import Poli
import Moni
import Vars

derivVars :: Char -> Vars -> Vars
derivVars _ [] = []
derivVars var (x:xs) | fst x == var = (var,snd x - 1) : derivVars var xs
                     | otherwise = x : derivVars var xs

derivMoni :: Char -> Moni -> Moni
derivMoni var (Moni coef vars ) | null filteredVars = Moni 0 [('_',0)]
                                | otherwise = Moni coefNew varsAux
                                 where filteredVars = filter (\(y,z) ->  y == var) vars
                                       coefNew = coef * snd coefAux
                                       varsAux = derivVars var vars
                                       coefAux = head (dropWhile (\x -> fst x /= var) vars)


derivPoli :: Char -> Poli -> Poli
derivPoli x [] = []
derivPoli x l = filter (\x -> coeficient x /= 0) [derivMoni x y | y <- l]
