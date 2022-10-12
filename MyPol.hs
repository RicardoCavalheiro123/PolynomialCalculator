module MyPol where

import Data.Char ( digitToInt, isLetter, isDigit )
import Data.List.Split ( oneOf, split )

type Coef = Int
type Power = Int
type Var = Char

type Monomial = (Coef, [(Var, Power)])
type Polynomial = [Monomial]
type Operation = Char

{- terms & ops -}
terms :: [String] -> [String]
terms [] = []
terms [x] = [x]
terms (x:y:xs) = x : terms xs

opperations :: [String] -> [String]
opperations [] = []
opperations [x] = []
opperations (x:y:xs) = y : opperations xs

{- Get coeficients and variables -}

coefOfMonomio :: Monomial -> Int
coefOfMonomio (x, y) = x

varsOfMonomio :: Monomial -> [(Var, Power)]
varsOfMonomio (x, y) = y


{- input -}
replaceWhiteSpaces :: String -> String
replaceWhiteSpaces [] = []
replaceWhiteSpaces (x:xs) | x == ' ' = replaceWhiteSpaces xs
                          | otherwise = x : replaceWhiteSpaces xs

jointSignals :: [String] -> [String]
jointSignals [] = []
jointSignals [x] = [x]
jointSignals (x:y:xs) | x == "+" = jointSignals (y:xs)
                      | x == "-" = jointSignals (("-"++y):xs)
                      | otherwise = x : jointSignals (y:xs)

stringToExp :: String -> [(Var, Power)]
stringToExp [] = []
stringToExp [x] = [(x, 1)]
stringToExp (x:y:xs) = if y == '^' then (x, head (map digitToInt (take 1 xs)) :: Power) : stringToExp (drop 1 xs) else (x, 1) : stringToExp (y : xs)


stringToMonomial :: String -> Monomial
stringToMonomial s | isDigit (head s) = (read (takeWhile isDigit s)::Int, stringToExp (dropWhile isDigit s))
                   | isLetter (head s) = (1, stringToExp s)
                   | otherwise = (-1 * read (takeWhile isDigit (tail s))::Int, stringToExp (dropWhile isDigit (tail s)))

stringToPolynomial :: String -> Polynomial
stringToPolynomial s = map stringToMonomial (jointSignals (if head v == "" then tail v else v))
                            where v = split (oneOf "+-") (replaceWhiteSpaces s)

{- output -}
varToString :: [(Var, Power)] -> String
varToString [] = ""
varToString ((v, 1): xs) = v : varToString xs
varToString ((v, p): xs) = v : "^" ++ show p ++ varToString xs


monomialToString :: Monomial -> String
monomialToString (c, []) = show c
monomialToString (1, xs) = varToString xs
monomialToString (c, xs) = show c ++ varToString xs


polynomialToString :: Polynomial -> String
polynomialToString [] = ""
polynomialToString [(c, xs)] = monomialToString (c, xs)
polynomialToString ((c, xs):(k, zs):ys) = monomialToString (c, xs) ++ (if k<0 then " - " else " + ") ++ polynomialToString ((abs k, zs):ys)
                                


{- normalizePolynomial -}
normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial [] = []
normalizePolynomial (x:xs) = (sumOfEqualMon x xs, varsOfMonomio x) : normalizePolynomial (filterList x xs)

normalize :: String -> String
normalize s = polynomialToString (normalizePolynomial (stringToPolynomial s))

filterList :: Monomial -> Polynomial -> Polynomial
filterList _ [] = []
filterList (c, xs) ((c1, xs1):ys) | xs == xs1 = filterList (c, xs) ys
                                  | otherwise = (c1, xs1) : filterList (c, xs) ys



{- addPolynomials -}
addPolynomial :: String -> String -> String
addPolynomial x y = polynomialToString (addPolynomial2 p1 p2 ++ [d | d <- p2, notElement d p1])
                        where p1 = stringToPolynomial x
                              p2 = stringToPolynomial y

{-add Polynomial -}
--2xy + 3x + 4y + 5  ------ 2x + 3
--[(2,[('x', 1),('y', 1)]), (3,[('x', 1)]), (4,[('y', 1)]), (5,[]),] [(2,[('x', 1)]), (3,[])]
addPolynomial2 :: Polynomial -> Polynomial -> Polynomial
addPolynomial2 [] y = []
addPolynomial2 (x:xs) ys = findEqualMon x ys : addPolynomial2 xs ys


-- (3,[('x', 1)])  ------   [(2,[('x', 1)]), (3,[('',0)])]
findEqualMon :: Monomial -> Polynomial-> Monomial
findEqualMon x [] = x
findEqualMon x (y:ys)| varsOfMonomio x == varsOfMonomio y = (coefOfMonomio x + coefOfMonomio y, varsOfMonomio x)
                    | otherwise = findEqualMon x ys

                    -- (3,[('x', 1)])  ------   [(2,[('x', 1)]), (3,[('',0)])]
sumOfEqualMon :: Monomial -> Polynomial-> Int
sumOfEqualMon x [] = coefOfMonomio x
sumOfEqualMon x (y:ys)| varsOfMonomio x == varsOfMonomio y = coefOfMonomio y + sumOfEqualMon x ys
                    | otherwise = sumOfEqualMon x ys

notElement :: Monomial -> Polynomial -> Bool
notElement x [] = True
notElement x (y:ys) | varsOfMonomio x == varsOfMonomio y = False
                 | otherwise = notElement x ys



{- multiplyPolynomial -}
checkPower :: (Var, Power) -> [(Var, Power)] -> Bool
checkPower x [] = False
checkPower x (y:ys) | fst x == fst y = True
                    | otherwise = checkPower x ys

addPower :: (Var, Power) -> [(Var, Power)] -> [(Var, Power)]
addPower x [] = [x]
addPower x (y:ys) | fst x == fst y = (fst x, snd x + snd y) : ys
                  | otherwise = y : addPower x ys

addPowers :: [(Var, Power)] -> [(Var, Power)] -> [(Var, Power)]
addPowers [] y = y
addPowers (x:xs) y = if checkPower x y then addPowers xs (addPower x y) else addPowers xs (x:y)

multiplyMonPol :: Monomial -> Polynomial -> Polynomial
multiplyMonPol _ [] = []
multiplyMonPol (c1, xs1) ((c2, xs2):ys) = (c1 * c2, addPowers xs1 xs2) : multiplyMonPol (c1, xs1) ys

multiplyPolPol :: Polynomial -> Polynomial -> Polynomial
multiplyPolPol [] _ = []
multiplyPolPol (x:xs) ys = multiplyMonPol x ys ++ multiplyPolPol xs ys

multiplyPolynomial :: String -> String -> String
multiplyPolynomial p1 p2 = polynomialToString (normalizePolynomial (multiplyPolPol (stringToPolynomial p1) (stringToPolynomial p2)))

{- derivativePolynomial (Coef, [(Var, Power)]) -}

derivativeMonomial :: Monomial -> Monomial
derivativeMonomial (c, []) = (0, [])
derivativeMonomial (c, xs) | snd (head xs) == 1 = (c * snd (head xs), tail xs)
                           | otherwise = (c * snd (head xs), (fst (head xs), snd (head xs) - 1) : tail xs)

derivativePolynomial :: Polynomial -> Polynomial
derivativePolynomial [] = []
derivativePolynomial ((c, xs):ys) = derivativeMonomial (c, xs) : derivativePolynomial ys

{- derivative e.g. d/dx (x^2 + 2x + 1) = 2x + 2 -}
derivative :: String -> String
derivative s = polynomialToString (derivativePolynomial (stringToPolynomial s))


