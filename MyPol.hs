module MyPol where

import Data.List.Split as S
import qualified Data.Text as T
import Data.Char(digitToInt)

type Coef = Int
type Power = Int
type Var = Char

type Monomial = (Coef, [(Var, Power)])
type Polynomial = [Monomial]

{- varToString -}
varToString :: [(Var, Power)] -> String
varToString [] = ""
varToString ((v, 1): xs) = [v] ++ varToString xs
varToString ((v, p): xs) = [v] ++ "^" ++ show p ++ varToString xs

{- monomialToString -}
monomialToString :: Monomial -> String
monomialToString (c, []) = show c
monomialToString (c, xs) = show c ++ varToString xs


{- polynomialToString -}
polynomialToString :: Polynomial -> String
polynomialToString [] = ""
polynomialToString ((c, xs):[]) = monomialToString (c, xs)
polynomialToString ((c, xs):ys) = monomialToString (c, xs) ++ " + " ++ polynomialToString ys


{- stringToVarPower -}
stringToVarPower :: String -> [(Var, Power)]
stringToVarPower [] = []
stringToVarPower (x:[]) = [(x, 1)]
stringToVarPower (x:y:xs) = if y == '^' then (x, head (map digitToInt (take 1 xs)) :: Power) : stringToVarPower (drop 1 xs) else (x, 1) : stringToVarPower (y : xs)


{- stringToMonomial  (Coef , [(Var, Power)]) -}
stringToMonomial :: String -> Monomial
stringToMonomial s = (head (map digitToInt (take 1 s)), stringToVarPower (drop 1 s))

{- stringToPolynomial -}
stringToPolynomial :: String -> Polynomial
stringToPolynomial s = map stringToMonomial (S.splitOn " + " s)

{- addPolynomial -}


