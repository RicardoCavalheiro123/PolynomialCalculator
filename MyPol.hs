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


{- normalizePolynomial -}
normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial p = p


{- addPolynomials -}
addPolynomial :: Polynomial -> Polynomial -> Polynomial
addPolynomial x y = addPolynomial x y ++ [d | d <- y, notElement d x]

{-add Polynomial -}
--2xy + 3x + 4y + 5  ------ 2x + 3
--[(2,[('x', 1),('y', 1)]), (3,[('x', 1)]), (4,[('y', 1)]), (5,[]),] [(2,[('x', 1)]), (3,[])]
addPolynomial2 :: Polynomial -> Polynomial -> Polynomial
addPolynomial2 [] y = []
addPolynomial2 (x:xs) ys = [findEqualMon x ys] ++ addPolynomial xs ys

{- stringToMonomial -}
-- (3,[('x', 1)])  ------   [(2,[('x', 1)]), (3,[('',0)])]
findEqualMon :: Monomial -> Polynomial-> Monomial
findEqualMon x [] = x
findEqualMon x (y:ys)| (varsOfMonomio x == varsOfMonomio y) = (coefOfMonomio x + coefOfMonomio y, varsOfMonomio x)
                    | otherwise = findEqualMon x ys

coefOfMonomio :: Monomial -> Int
coefOfMonomio (x, y) = x

varsOfMonomio :: Monomial -> [(Var, Power)]
varsOfMonomio (x, y) = y

notElement :: Monomial -> Polynomial -> Bool
notElement x [] = True
notElement x (y:ys) | (varsOfMonomio x == varsOfMonomio y) = False
                 | otherwise = notElement x ys



{- multiplyPolynomial -}
multiplyMonPol :: Monomial -> Polynomial -> Polynomial
multiplyMonPol _ [] = []
multiplyMonPol (c1, xs1) ((c2, xs2):ys) = (c1 * c2, xs1 ++ xs2) : multiplyMonPol (c1, xs1) ys

multiplyPolPol :: Polynomial -> Polynomial -> Polynomial
multiplyPolPol [] _ = []
multiplyPolPol (x:xs) ys = multiplyMonPol x ys ++ multiplyPolPol xs ys

multiplyPolynomial :: Polynomial -> Polynomial -> Polynomial
multiplyPolynomial p1 p2 = normalizePolynomial (multiplyPolPol p1 p2)
