module MyPol where

import Data.List.Split as S
import qualified Data.Text as T

type Coef = Int
type Power = Int
type Var = Char

type Monomial = (Coef, [(Var, Power)])
type Polynomial = [Monomial]

{- polynomialToString -}
polynomialToString :: Polynomial -> String
polynomialToString [] = ""
polynomialToString ((c, []):xs) = show c ++ polynomialToString xs
polynomialToString ((c, xs):[]) = show c ++ show xs ++ polynomialToString []
polynomialToString ((c, xs):ys) = show c ++ show xs ++ " + " ++ polynomialToString ys

{- stringToPolynomial 
-}
stringToPolynomial :: String -> Polynomial
stringToPolynomial s = map stringToMonomial (S.splitOn " + " s)

{- stringToMonomial
-}
stringToMonomial :: String -> Monomial
stringToMonomial s = 
