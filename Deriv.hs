module Deriv where

import Poli
import Moni

derivMoni :: Char -> Moni -> Moni
derivMoni var (Moni coef vars ) | null filteredVars = Moni 0 [('_',0)]
                                | otherwise = Moni coefAux [(varAux,degreeAux)]
                                 where filteredVars = filter (\(y,z) ->  y == var) vars
                                       coefAux = coef * snd (head filteredVars)
                                       varAux = fst (head filteredVars)
                                       degreeAux = snd (head filteredVars) - 1

derivPoli :: Char -> Poli -> Poli
derivPoli x [] = []
derivPoli x l = filter (\x -> coeficient x /= 0) [derivMoni x y | y <- l]
