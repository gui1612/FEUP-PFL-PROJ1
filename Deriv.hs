module Deriv where

import Poli
import Moni
import Vars

derivVars :: Char -> Vars -> Vars
derivVars _ [] = []
<<<<<<< HEAD
derivVars var (x:xs) | fst x == var = (var,snd x - 1) : derivVars var xs
                     | otherwise = x : derivVars var xs
=======
derivVars var (x:xs) | fst x == var && (snd x - 1) == 0 = ('_',newDegree) : derivVars var xs
                     | fst x == var                   = (var,newDegree) : derivVars var xs
                     | otherwise = x : derivVars var xs
                     where newDegree = snd x - 1
>>>>>>> 562b9de2d1a22015e2bbcabc3a037a7559e1d7b7

derivMoni :: Char -> Moni -> Moni
derivMoni var (Moni coef vars ) | null filteredVars = Moni 0 [('_',0)]
                                | otherwise = Moni coefNew varsAux
                                 where filteredVars = filter (\(y,z) ->  y == var) vars
                                       coefNew = coef * snd coefAux
                                       varsAux = derivVars var vars
                                       coefAux = head (dropWhile (\x -> fst x /= var) vars)


derivPoli :: Char -> Poli -> Poli
derivPoli x [] = []
derivPoli x l = internalSum (filter (\x -> coeficient x /= 0) [derivMoni x y | y <- l])
