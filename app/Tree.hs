module Tree where

import Data.Char(isSpace, isDigit, digitToInt, isLetter)
import Data.Maybe ()

import Moni
import Poli
import Sum
import Prod

--------------------------------------------------------------------------------

--Tokens that represent the elements of a String
data Token
 = PlusTok --'+'
 | TimesTok --'*'
 | OpenTok --'('
 | NegTok --'-'
 | DegreeTok --'^'
 | CloseTok --')'
 | VarTok Char --like 'x' or 'y'
 | IntTok  Int --like '2' or '35'
 deriving (Show)

--Auxiliary function that checks if a char is a digit or not
notisDigit :: Char -> Bool
notisDigit chr = (not . isDigit) chr


--Lexer function that transforms a String into a list of tokens
lexer :: String -> [Token]
lexer []              = []
lexer ('+' : restStr) = PlusTok   : lexer restStr
lexer ('-' : restStr) = NegTok    : lexer restStr
lexer ('*' : restStr) = TimesTok  : lexer restStr
lexer ('^' : restStr) = DegreeTok : lexer restStr
lexer ('(' : restStr) = OpenTok   : lexer restStr
lexer (')' : restStr) = CloseTok  : lexer restStr
lexer (chr : restStr) | isSpace chr = lexer restStr
                      | isLetter chr = VarTok (chr) : lexer restStr

lexer str@(chr : _)   | isDigit chr = IntTok (stringToInt digitStr) : lexer restStr
                                    where
                                    (digitStr, restStr) = break (not . isDigit) str
                                    stringToInt         = foldl (\acc chr -> 10 * acc + digitToInt chr) 0

lexer (_ : restString)  = error ("lexer: unexpected character!")

--Adapted Binary Tree
data Expr
  = IntLit Int --integer constants, leaves of the expression tree
  | Sum   Expr Expr --sum node
  | Mult  Expr Expr --multiplication node
  | Coef Int --coeficient leaf
  | Var (Char,Int) --vars leaf
  | Monomial Expr Expr --monomial leaf
  deriving (Show)

parseInt :: [Token] -> Maybe (Expr, [Token])
parseInt (IntTok n : restTokens) = Just (IntLit n, restTokens)
parseInt tokens = Nothing

--parseMoni :: [Token] -> Maybe (Expr, [Token])
--parseMoni (IntTok a : TimesTok b : VarTok c : DegreeTok d : IntTok e: restTokens) =
--            Just ()

--Parser that accepts a list of tokens and finds the Coeficient
parseCoef :: [Token] -> Maybe (Expr, [Token])
parseCoef (IntTok n : restTokens) = Just (Coef n, restTokens) --found a positive coeficient
parseCoef (VarTok n : restTokens) = Just (Coef 1, (VarTok n : restTokens)) --found only Vars, which means the degree is 1
parseCoef (NegTok : IntTok n : restTokens) = Just (Coef (-1 * n), restTokens) --found a negative coeficient
parseCoef (NegTok : VarTok n : restTokens) = Just (Coef (-1), (VarTok n : restTokens)) --found Vars with a NegTok, which means the degree is -1
parseCoef tokens = Nothing

--Parser that accepts a list of tokens and finds the Vars
parseVars :: [Token] -> Maybe (Expr, [Token])
parseVars (VarTok n : DegreeTok : IntTok m : restTokens) = Just (Var (n,m) , restTokens) --found the pattern '_^_'
parseVars (VarTok ch : restTokens) = Just (Var (ch,1), restTokens) --found the pattern '_'
parseVars tokens = Just (Var ('_',0), tokens) --doesn't have Vars

--Parser that accepts a list of tokens and finds a binary tree with Monomials
parseMoni :: [Token] -> Maybe (Expr, [Token])
parseMoni tokens = case parseCoef tokens of
                    Just (expr1, (TimesTok : VarTok x: restTokens1)) -> --found a monomial that has Vars
                      case parseVars (VarTok x : restTokens1) of
                        Just (expr2, restTokens2) -> Just (Monomial expr1 expr2, restTokens2)
                        Nothing -> Nothing
                    Just (expr1, restTokens3) -> --found a monomial that doesn't have Vars
                      case parseVars restTokens3 of
                        Just (expr2, restTokens4) -> Just (Monomial expr1 expr2, restTokens4)
                        Nothing -> Nothing
                    result -> result

--Parser that accepts a list of tokens and finds a binary tree with Monomials or Products of Monomials
parseProdOrMoni :: [Token] -> Maybe (Expr, [Token])
parseProdOrMoni tokens = case parseMoni tokens of
                          Just (expr1, (TimesTok : restTokens1)) ->
                           case parseProdOrMoni restTokens1 of
                             Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
                             Nothing -> Nothing
                          result -> result

--Parser that accepts a list of tokens and finds a binary tree with Monomials, Products of Monomials or Sums of Monomials
parseSumOrProdOrMoni :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrMoni tokens = case parseProdOrMoni tokens of
                                Just (expr1, (PlusTok : restTokens1)) ->
                                  case parseSumOrProdOrMoni restTokens1 of
                                    Just (expr2, restTokens2) -> Just (Sum expr1 expr2, restTokens2)
                                    Nothing -> Nothing
                                Just (expr3, (NegTok : restTokens3)) ->
                                  case parseSumOrProdOrMoni (NegTok : restTokens3) of
                                    Just (expr4, restTokens4) -> Just (Sum expr3 expr4, restTokens4)
                                    Nothing -> Nothing
                                result -> result

--Parser that accepts a list of tokens and returns a binary tree
parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrMoni tokens of
    Just (expr, []) -> expr
    _               -> error "Could not parse input"

--Auxiliary function that evaluates the expression represented by a binary tree
eval :: Expr -> Poli --fazer fromJust e fst
eval (Monomial (Coef n) (Var (x,y))) = [(Moni n [(x,y)])] --Monomial
eval (Sum expr1 expr2) = sumPoli (eval expr1) (eval expr2) -- Sum of Monomials
eval (Mult expr1 expr2) = prodPoli (eval expr1) (eval expr2) --Multiplication of Monomials

--Function that parses a String into a Poli
parsePoli :: String -> Poli
parsePoli poli = eval (parse (lexer poli))

--------------------------------------------------------------------------------
