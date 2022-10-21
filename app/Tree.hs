module Tree where

import Data.Char(isSpace, isDigit, digitToInt, isLetter)
import Data.Maybe ()

import Vars
import Moni
import Poli
import Sum
import Prod

data Token
 = PlusTok
 | TimesTok
 | OpenTok
 | NegTok
 | DegreeTok
 | CloseTok
 | VarTok Char
 | IntTok  Int
 deriving (Show)

notisDigit :: Char -> Bool
notisDigit chr = (not . isDigit) chr

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

--parser :: [Token] -> Expr

data Expr
  = IntLit Int
  | Sum   Expr Expr
  | Mult  Expr Expr
  | Coef Int
  | Var (Char,Int)
  | Monomial Expr Expr
  deriving (Show)

parseCoef :: [Token] -> Maybe (Expr, [Token])
parseCoef (IntTok n : restTokens) = Just (Coef n, restTokens)
parseCoef (VarTok n : restTokens) = Just (Coef 1, (VarTok n : restTokens))
parseCoef (NegTok : IntTok n : restTokens) = Just (Coef (-1 * n), restTokens)
parseCoef (NegTok : VarTok n : restTokens) = Just (Coef (-1), (VarTok n : restTokens))
parseCoef tokens = Nothing

parseVars :: [Token] -> Maybe (Expr, [Token])
parseVars (VarTok n : DegreeTok : IntTok m : restTokens) = Just (Var (n,m) , restTokens)
parseVars (VarTok ch : restTokens) = Just (Var (ch,1), restTokens)
parseVars tokens = Just (Var ('_',0), tokens)


parseMoni :: [Token] -> Maybe (Expr, [Token])
parseMoni tokens = case parseCoef tokens of
                    Just (expr1, (TimesTok : VarTok x: restTokens1)) ->
                      case parseVars (VarTok x : restTokens1) of
                        Just (expr2, restTokens2) -> Just (Monomial expr1 expr2, restTokens2)
                        Nothing -> Nothing
                    Just (expr1, restTokens3) ->
                      case parseVars restTokens3 of
                        Just (expr2, restTokens4) -> Just (Monomial expr1 expr2, restTokens4)
                        Nothing -> Nothing
                    result -> result


parseProdOrMoni :: [Token] -> Maybe (Expr, [Token])
parseProdOrMoni tokens = case parseMoni tokens of
                          Just (expr1, (TimesTok : restTokens1)) ->
                           case parseProdOrMoni restTokens1 of
                             Just (expr2, restTokens2) -> Just (Mult expr1 expr2, restTokens2)
                             Nothing -> Nothing
                          result -> result


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

parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrMoni tokens of
    Just (expr, []) -> expr
    _               -> error "Could not parse input"

eval :: Expr -> Poli --fazer fromJust e fst
eval (Monomial (Coef n) (Var (x,y))) = [(Moni n [(x,y)])]
eval (Sum expr1 expr2) = sumPoli (eval expr1) (eval expr2)
eval (Mult expr1 expr2) = prodPoli (eval expr1) (eval expr2)
