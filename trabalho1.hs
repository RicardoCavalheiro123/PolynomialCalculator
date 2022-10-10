

    
{- main
-}
main ::  IO()
main = do
    putStrLn "Deseja:"
    putStrLn "1.normalizar polinómios"
    putStrLn "2.adicionar polinómios"
    putStrLn "3.multiplicar polinómios"
    putStrLn "4.calcular a derivada de um polinómio"
    opçao <- getLine
    if opçao == "1" 
        then do putStrLn "Digite o primeiro polinómio"
                pol <- getLine
                --Polinomio = listToTuple splitOn "+" pol
                --putStrLn (trim " dsa ")
                
                print (S.splitOn "+" pol)
                
    else if opçao == "2"
        then putStrLn opçao
    else if opçao == "3" 
        then putStrLn opçao
    else if opçao == "4" 
        then putStrLn opçao
    else putStrLn "Wrong number"

{- monomial 
-}
type Monomial = (Int, [(Char, Int)])

{- polynomial
-}
type Polynomial = [Monomial]

{- polynomialToString
-}
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
stringToMonomial s = (read (T.unpack (T.takeWhile (/= '*') (T.pack s))) :: Int, stringToVariable (T.unpack (T.drop 1 (T.dropWhile (/= '*') (T.pack s)))))

{- stringToVariable
-}
stringToVariable :: String -> [(Char, Int)]
stringToVariable [] = []
stringToVariable (x:[]) = [(x, 1)]
stringToVariable (x:xs) = if x == '^' then (head xs, read (tail xs) :: Int) : stringToVariable [] else (x, 1) : stringToVariable xs

{- add polynomial
-}
addPolynomial :: Polynomial -> Polynomial -> Polynomial
addPolynomial p1 p2 = p1 ++ p2

{- multiply polynomial
-}
multiplyPolynomial :: Polynomial -> Polynomial -> Polynomial
multiplyPolynomial p1 p2 = p1 ++ p2

{- derivative polynomial
-}
derivativePolynomial :: Polynomial -> Polynomial
derivativePolynomial p = p

{- normalize polynomial
-}
normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial p = p