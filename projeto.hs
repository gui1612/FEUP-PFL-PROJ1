{-# LANGUAGE BlockArguments #-}
import Data.Char (isDigit, digitToInt, isLetter)
import Data.Text (splitOn)

import Sum
import Prod
import Deriv
import Poli
import Moni
import Vars
--module Main where

--  main :: IO()
--  main = do
--    poli <- getLine
--    putStrLn poli
-- Test Cases
a = (Moni 2 [('x', 3)])
b = (Moni (-5) [('x', 3)])
c = (Moni 5 [('y', 2)])
d = (Moni 0 [('x', 2)])
e = (Moni 1 [('x', 1)])
f = (Moni 2 [('x', 1)])
g = (Moni 2 [('x', 0)])
h = (Moni 2 [('x', 2), ('y', 1), ('x', 3)])

poli_ex = [a, b, c, d, e, f, g]


main = do
  putStrLn "please enter a monomial: "
  input <- getLine
  putStrLn input
