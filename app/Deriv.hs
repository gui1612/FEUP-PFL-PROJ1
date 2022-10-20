module Deriv where

import Poli
import Moni
import Vars

--------------------------------------------------------------------------------

--Auxiliary Function that handles the variables
--during the derivation of a monomial
derivVars :: Char -> Vars -> Vars
derivVars _ [] = []
derivVars var (x:xs) | fst x == var && (snd x - 1) == 0 = ('_',newDegree) : derivVars var xs
                     | fst x == var                   = (var,newDegree) : derivVars var xs
                     | otherwise = x : derivVars var xs
                     where newDegree = snd x - 1

--Function for Deriving a Monomial, according to the variable given
derivMoni :: Char -> Moni -> Moni
derivMoni var (Moni coef vars ) | null filteredVars = Moni 0 [('_',0)] --Monomial that doesn't have the variable var
                                | otherwise = Moni coefNew varsAux
                                 where filteredVars = filter (\(y,z) ->  y == var) vars --filtered list of Vars that only include elements
                                                                                        --that have the variable var
                                       coefNew = coef * snd coefAux --new Coeficient
                                       varsAux = derivVars var vars --new Vars
                                       coefAux = head (dropWhile (\x -> fst x /= var) vars) --Var that has the same variable as var

--Function for Deriving a Polinomial, according to the variable given, which then
--does the internal sum of the resulting polinomial (normalizes it) and removes
--the members that have 0 as its coeficient
derivPoli :: Char -> Poli -> Poli
derivPoli x [] = []
derivPoli x l = internalSum (filter (\x -> coeficient x /= 0) [derivMoni x y | y <- l])

--------------------------------------------------------------------------------
