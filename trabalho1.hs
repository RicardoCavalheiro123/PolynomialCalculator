import Data.List.Split as S
import qualified Data.Text as T


type Monomio = (Int, [(Char, Char)])
type Polinomio = [Monomio]

listToTuple :: [a] -> (a,a,a)
listToTuple [x,y,z] = (x,y,z)

listToTuple2 :: [a] -> (a,a)
listToTuple2 [x,y] = (x,y)

polinomios ::  IO()
polinomios = do
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

{-Normalizar monomio em tuplo 
Ex. 2x -> (2,x,1)
    2xy^2
-}
normalizarMonomio :: String -> Monomio
normalizarMonomio monomio = (coeficiente, variaveis)
    where
        coeficiente = read (takeWhile (/= 'x') monomio) :: Int
        variaveis = map listToTuple2 (S.splitOn "x" (dropWhile (/= 'x') monomio))

{-Normalizar polinómio em lista de tuplos
Ex. 2x + 3xy^2 -> [(2,x,1),(3,x,2)]
-}
normalizarPolinomio :: String -> Polinomio
normalizarPolinomio polinomio = map normalizarMonomio (S.splitOn "+" polinomio)
    
{-Adicionar polinómios
Ex. 2x + 3xy^2 + 4x^2y + 5x^2y^2 + 6x^2y^3 + 7x^3y^3 + 8x^3y^4
2x + 3xy^2 + 4x^2y + 5x^2y^2 + 6x^2y^3 + 7x^3y^3 + 8x^3y^4
-}
adicionarPolinomios :: Polinomio -> Polinomio -> Polinomio
adicionarPolinomios polinomio1 polinomio2 = polinomio1 ++ polinomio2

{-Multiplicar polinómios
Ex. 2x + 3xy^2 + 4x^2y + 5x^2y^2 + 6x^2y^3 + 7x^3y^3 + 8x^3y^4
2x + 3xy^2 + 4x^2y + 5x^2y^2 + 6x^2y^3 + 7x^3y^3 + 8x^3y^4
-}
multiplicarPolinomios :: Polinomio -> Polinomio -> Polinomio
multiplicarPolinomios polinomio1 polinomio2 = polinomio1 ++ polinomio2

{-Calcular a derivada de um polinómio
Ex. 2x + 3xy^2 + 4x^2y + 5x^2y^2 + 6x^2y^3 + 7x^3y^3 + 8x^3y^4
2x + 3xy^2 + 4x^2y + 5x^2y^2 + 6x^2y^3 + 7x^3y^3 + 8x^3y^4
-}
