module MyPol where

import Data.Char ( digitToInt, isLetter, isDigit )
import Data.List.Split ( oneOf, split )
import Data.List

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
                   | otherwise = (-1 * fst w, snd w) where w = stringToMonomial (tail s)

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

                   
{- Sorting-}
sortVariables :: [(Var, Power)] -> [(Var, Power)]
sortVariables [] = []
sortVariables xs = sortBy (\(x, y) (z, w) -> compare x z) xs

sortMonomial :: Monomial -> Monomial
sortMonomial (c, xs) = (c, sortVariables xs)

sortPolynomial :: Polynomial -> Polynomial
sortPolynomial [] = []
sortPolynomial xs = sortBy (\(x, y) (z, w) -> compare y w) (map sortMonomial xs)

{-Cleanu polunomial -> sort, nomalize and clean-}

cleanupPolynomialForOutput :: Polynomial -> Polynomial 
cleanupPolynomialForOutput pol = cleanPolynomial(normalizePolynomial(sortPolynomial pol))


{- normalizePolynomial -}
normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial [] = []
normalizePolynomial (x:xs) = (sumOfEqualMon x xs, varsOfMonomio x) : normalizePolynomial (filterList x xs)

normalize :: String -> String
normalize s = polynomialToString (cleanupPolynomialForOutput(stringToPolynomial s))

filterList :: Monomial -> Polynomial -> Polynomial
filterList _ [] = []
filterList (c, xs) ((c1, xs1):ys) | xs == xs1 = filterList (c, xs) ys
                                  | otherwise = (c1, xs1) : filterList (c, xs) ys


myclear :: [(Var,Power)] -> [(Var,Power)]
myclear [] = []
myclear ((v, p):xs) | p == 0 = myclear xs
                    | otherwise = (v, p) : myclear xs

cleanPolynomial :: Polynomial -> Polynomial
cleanPolynomial [] = []
cleanPolynomial ((c, v):ys) | c == 0 = cleanPolynomial ys
                            | otherwise = (c,myclear v) : cleanPolynomial ys

{- add Polynomials by string -}
addPolynomialString :: String -> String -> String
addPolynomialString x y = polynomialToString (cleanupPolynomialForOutput(addPolynomial p1 p2 ++ [d | d <- p2, notElement d p1]))
                        where p1 = stringToPolynomial x
                              p2 = stringToPolynomial y

{-add Polynomials -}
addPolynomial :: Polynomial -> Polynomial -> Polynomial
addPolynomial [] y = []
addPolynomial (x:xs) ys = findEqualMon x ys : addPolynomial xs ys


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
multiplyPolynomial p1 p2 = polynomialToString (cleanupPolynomialForOutput( multiplyPolPol (stringToPolynomial p1) (stringToPolynomial p2)))

{- derivativePolynomial (Coef, [(Var, Power)]) -}
derivativePower :: Char -> [(Var, Power)] -> ([(Var, Power)], Int)
derivativePower c [] = ([], 0)
derivativePower c ((v, p):xs) | c == v = (xs, p)
                              | otherwise = let (ys, k) = derivativePower c xs in ((v, p):ys, k)


derivativeMonomial :: Char -> Monomial -> Monomial
derivativeMonomial v (0, xs) = (0, xs)
derivativeMonomial v (c, []) = (0, [])
derivativeMonomial v (c, xs) | v == fst (head xs) && 1 /= snd (head xs) = (c * snd (head xs),  (fst (head xs), snd (head xs) - 1):tail xs)
                             | v == fst (head xs) && 1 == snd (head xs) = (c * snd (head xs),  (fst (head xs), snd (head xs) - 1):tail xs)
                             | otherwise = (fst d, head xs : snd d) where d = derivativeMonomial v (c, tail xs)

derivativePolynomial :: Char -> Polynomial -> Polynomial
derivativePolynomial v [] = []
derivativePolynomial v ((c, xs):ys) = derivativeMonomial v (c, xs) : derivativePolynomial v ys

{- derivative e.g. d/dx (x^2 + 2x + 1) = 2x + 2 -}
derivative :: Char -> String -> String
derivative v s = polynomialToString (cleanupPolynomialForOutput( derivativePolynomial   v (stringToPolynomial s)))


